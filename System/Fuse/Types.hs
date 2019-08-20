module System.Fuse.Types where

import Foreign.C
import Foreign.Ptr
import System.Posix.Types

data CStat -- struct stat
data CStructFuse -- struct fuse
data CFuseOperations -- struct fuse_operations
data CFuseFileInfo -- struct fuse_file_info
data CFuseConnInfo -- struct fuse_conn_info
type CGetAttr = CString -> Ptr CStat -> IO CInt

{- | Used by 'fuseGetFileStat'.  Corresponds to @struct stat@ from @stat.h@;
     @st_dev@, @st_ino@ and @st_blksize@ are omitted, since (from the libfuse
     documentation): \"the @st_dev@ and @st_blksize@ fields are ignored.  The
     @st_ino@ field is ignored except if the use_ino mount option is given.\"

     /TODO: at some point the inode field will probably be needed./
-}
data FileStat = FileStat { statEntryType :: EntryType
                         , statFileMode :: FileMode
                         , statLinkCount :: LinkCount
                         , statFileOwner :: UserID
                         , statFileGroup :: GroupID
                         , statSpecialDeviceID :: DeviceID
                         , statFileSize :: FileOffset
                         , statBlocks :: Integer
                         , statAccessTime :: EpochTime
                         , statModificationTime :: EpochTime
                         , statStatusChangeTime :: EpochTime
                         }
    deriving Show


-- | The Unix type of a node in the filesystem.
data EntryType
    = Unknown            -- ^ Unknown entry type
    | NamedPipe
    | CharacterSpecial
    | Directory
    | BlockSpecial
    | RegularFile
    | SymbolicLink
    | Socket
      deriving(Show)
