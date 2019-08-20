module System.Fuse.Main
    ( fuseMainReal
    , fuseMain -- :: FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , fuseRun -- :: String -> [String] -> FuseOperations fh -> (Exception -> IO Errno) -> IO ()

    , fuseMainInline -- :: FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , fuseRunInline -- :: String -> [String] -> FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , defaultExceptionHandler -- :: Exception -> IO Errno
                                                                                      , defaultFuseOps

    ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Control.Exception as E (Exception, handle, finally, SomeException, bracket_, bracket)
import Data.Maybe
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe   as B
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Marshal
import System.Environment ( getProgName, getArgs )
import System.IO ( hPutStrLn, stderr, withFile, stdin, stdout, IOMode(..) )
import System.Posix.Types
import System.Posix.Files ( accessModes, intersectFileModes, unionFileModes )
import System.Posix.Directory(changeWorkingDirectory)
import System.Posix.Process(forkProcess,createSession,exitImmediately)
import System.Posix.IO ( OpenMode(..), OpenFileFlags(..) )
import qualified System.Posix.Signals as Signals
import GHC.IO.Handle(hDuplicateTo)
import System.Exit

import Polysemy

import System.Fuse.Bindings

-- Mounts the filesystem, forks, and then starts fuse
fuseMainReal
    :: (Exception e, Member (Embed IO) r)
    => (forall x. Sem r x -> IO x)
    -> Maybe (Fd -> IO () -> IO b, b -> IO (), Either String () -> IO a)
    -> Bool
    -> FuseOperations fh (Sem r)
    -> (e -> IO Errno)
    -> Ptr CFuseArgs
    -> String
    -> IO a
fuseMainReal lowerSem inline foreground ops handler pArgs mountPt =
    let strategy = case inline of
            Just (register, unregister, act) -> runInline register unregister act
            Nothing -> if foreground
                then (>>) (changeWorkingDirectory "/") . procMain
                else daemon . procMain
     in withCString mountPt $ \cMountPt -> bracket
        (fuse_mount cMountPt pArgs)
        (const $ fuse_unmount cMountPt nullPtr) $ \pFuseChan -> do
            if pFuseChan == nullPtr
                then case inline of
                    Nothing -> exitFailure
                    -- TODO: Add some way to notify the called application
                    -- whether fuse is up, or not
                    Just (_, _, act) -> act $ Left "Failed to create fuse handle"
                else do
                    withStructFuse lowerSem pFuseChan pArgs ops handler strategy

    -- here, we're finally inside the daemon process, we can run the main loop
    where procMain pFuse = do
            session <- fuse_get_session pFuse
            -- calling fuse_session_exit to exit the main loop only
            -- appears to work with the multithreaded fuse loop.
            -- In the single-threaded case, FUSE depends on their
            -- recv() call to finish with EINTR when signals arrive.
            -- This doesn't happen with GHC's signal handling in place.
            withSignalHandlers (fuse_session_exit session) $ do
                retVal <- fuse_loop_mt pFuse
                if retVal == 1
                    then exitWith ExitSuccess
                    else exitFailure

-- TODO: Add an unregister function to run as well
runInline
    :: (Fd -> IO () -> IO b)
    -> (b -> IO ())
    -> (Either String () -> IO a)
    -> Ptr CStructFuse
    -> IO a
runInline register unregister act pFuse = bracket (callocBytes fuseBufSize) free $ \buf -> do
    session <- fuse_get_session pFuse
    let registerChan chan cont = do
            fd <- fuse_chan_fd chan
            bracket
                (register fd (handleOnce session buf chan))
                unregister
                (const cont)
    ret <- forAllChans session registerChan $ withSignalHandlers (fuse_session_exit session) (act $ Right ())
    fuse_session_exit session
    pure ret

-- | Main function of FUSE.
-- This is all that has to be called from the @main@ function. On top of
-- the 'FuseOperations' record with filesystem implementation, you must give
-- an exception handler converting Haskell exceptions to 'Errno'.
--
-- This function does the following:
--
--   * parses command line options (@-d@, @-s@ and @-h@) ;
--
--   * passes all options after @--@ to the fusermount program ;
--
--   * mounts the filesystem by calling @fusermount@ ;
--
--   * installs signal handlers for 'System.Posix.Signals.keyboardSignal',
--     'System.Posix.Signals.lostConnection',
--     'System.Posix.Signals.softwareTermination' and
--     'System.Posix.Signals.openEndedPipe' ;
--
--   * registers an exit handler to unmount the filesystem on program exit ;
--
--   * registers the operations ;
--
--   * calls FUSE event loop.
fuseMain :: (Exception e, (Member (Embed IO) r))
         => (forall x. Sem r x -> IO x)
         -> FuseOperations fh (Sem r)
         -> (e -> IO Errno)
         -> IO ()
fuseMain lowerSem ops handler = do
    -- this used to be implemented using libfuse's fuse_main. Doing this will fork()
    -- from C behind the GHC runtime's back, which deadlocks in GHC 6.8.
    -- Instead, we reimplement fuse_main in Haskell using the forkProcess and the
    -- lower-level fuse_new/fuse_loop_mt API.
    prog <- getProgName
    args <- getArgs
    fuseRun lowerSem prog args ops handler

fuseRun :: (Member (Embed IO) r)
        => (forall x. Sem r x -> IO x)
        -> String
        -> [String]
        -> Exception e
        => FuseOperations fh (Sem r)
        -> (e -> IO Errno)
        -> IO ()
fuseRun lowerSem prog args ops handler =
    catch
       (withFuseArgs prog args (\pArgs ->
         do cmd <- fuseParseCommandLine pArgs
            case cmd of
              Nothing -> fail ""
              Just (Nothing, _, _) -> fail "Usage error: mount point required"
              Just (Just mountPt, _, foreground) -> fuseMainReal lowerSem Nothing foreground ops handler pArgs mountPt))
       ((\errStr -> when (not $ null errStr) (putStrLn errStr) >> exitFailure) . ioeGetErrorString)

-- | Inline version of 'fuseMain'. This prevents exiting and keeps the fuse
-- file system in the same process (and therefore memory space)
fuseMainInline :: (Exception e, (Member (Embed IO) r))
               => (forall x. Sem r x -> IO x)
               -> (Fd -> IO () -> IO b)
               -> (b -> IO ())
               -> (Either String () -> IO a)
               -> FuseOperations fh (Sem r)
               -> (e -> IO Errno)
               -> IO a
fuseMainInline lowerSem register unregister act ops handler = do
    -- this used to be implemented using libfuse's fuse_main. Doing this will fork()
    -- from C behind the GHC runtime's back, which deadlocks in GHC 6.8.
    -- Instead, we reimplement fuse_main in Haskell using the forkProcess and the
    -- lower-level fuse_new/fuse_loop_mt API.
    prog <- getProgName
    args <- getArgs
    fuseRunInline lowerSem register unregister act prog args ops handler

fuseRunInline :: (Exception e, (Member (Embed IO) r))
              => (forall x. Sem r x -> IO x)
              -> (Fd -> IO () -> IO b)
              -> (b -> IO ())
              -> (Either String () -> IO a)
              -> String
              -> [String]
              -> FuseOperations fh (Sem r)
              -> (e -> IO Errno)
              -> IO a
fuseRunInline lowerSem register unregister act prog args ops handler =
    catch (withFuseArgs prog args $ \pArgs -> do
        cmd <-fuseParseCommandLine pArgs
        case cmd of
            Nothing -> act $ Left ""
            Just (Nothing, _, _) -> act $ Left "Usage error: mount point required"
            Just (Just mountPt, _, foreground) -> fuseMainReal lowerSem (Just (register, unregister, act)) foreground ops handler pArgs mountPt)
       (act . Left . ioeGetErrorString)


-- | Empty \/ default versions of the FUSE operations.
defaultFuseOps :: FuseOperations fh IO
defaultFuseOps =
    FuseOperations { fuseGetFileStat = \_ -> return (Left eNOSYS)
                   , fuseReadSymbolicLink = \_ -> return (Left eNOSYS)
                   , fuseCreateDevice = \_ _ _ _ ->  return eNOSYS
                   , fuseCreateDirectory = \_ _ -> return eNOSYS
                   , fuseRemoveLink = \_ -> return eNOSYS
                   , fuseRemoveDirectory = \_ -> return eNOSYS
                   , fuseCreateSymbolicLink = \_ _ -> return eNOSYS
                   , fuseRename = \_ _ -> return eNOSYS
                   , fuseCreateLink = \_ _ -> return eNOSYS
                   , fuseSetFileMode = \_ _ -> return eNOSYS
                   , fuseSetOwnerAndGroup = \_ _ _ -> return eNOSYS
                   , fuseSetFileSize = \_ _ -> return eNOSYS
                   , fuseSetFileTimes = \_ _ _ -> return eNOSYS
                   , fuseOpen =   \_ _ _   -> return (Left eNOSYS)
                   , fuseRead =   \_ _ _ _ -> return (Left eNOSYS)
                   , fuseWrite =  \_ _ _ _ -> return (Left eNOSYS)
                   , fuseGetFileSystemStats = \_ -> return (Left eNOSYS)
                   , fuseFlush = \_ _ -> return eOK
                   , fuseRelease = \_ _ -> return ()
                   , fuseSynchronizeFile = \_ _ -> return eNOSYS
                   , fuseOpenDirectory = \_ -> return eNOSYS
                   , fuseReadDirectory = \_ -> return (Left eNOSYS)
                   , fuseReleaseDirectory = \_ -> return eNOSYS
                   , fuseSynchronizeDirectory = \_ _ -> return eNOSYS
                   , fuseAccess = \_ _ -> return eNOSYS
                   , fuseInit = return ()
                   , fuseDestroy = return ()
                   }




-- Calls fuse_parse_cmdline to parses the part of the commandline arguments that
-- we care about. fuse_parse_cmdline will modify the CFuseArgs struct passed in
-- to remove those arguments; the CFuseArgs struct containing remaining arguments
-- must be passed to fuse_mount/fuse_new.
--
-- The multithreaded runtime will be used regardless of the threading flag!
-- See the comment in fuse_session_exit for why.
fuseParseCommandLine :: Ptr CFuseArgs -> IO (Maybe (Maybe String, Bool, Bool))
fuseParseCommandLine pArgs =
    alloca (\pMountPt ->
        alloca (\pMultiThreaded ->
            alloca (\pFG ->
                do poke pMultiThreaded 0
                   poke pFG 0
                   retval <- fuse_parse_cmdline pArgs pMountPt pMultiThreaded pFG
                   if retval == 0
                     then do cMountPt <- peek pMountPt
                             mountPt <- if cMountPt /= nullPtr
                                          then do a <- peekCString cMountPt
                                                  free cMountPt
                                                  return $ Just a
                                          else return $ Nothing
                             multiThreaded <- peek pMultiThreaded
                             foreground <- peek pFG
                             return $ Just (mountPt, multiThreaded == 1, foreground == 1)
                     else return Nothing)))
