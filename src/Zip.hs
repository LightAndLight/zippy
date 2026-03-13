-- | https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
module Zip where

import qualified Codec.Compression.Zlib as Zlib
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.UTF8 as Utf8
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Digest.CRC32 as CRC32
import Data.Foldable (foldlM)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word16, Word32, Word64)
import System.IO (Handle, IOMode (..), SeekMode (..), hClose, hSeek, hSetFileSize, hTell, openFile)

data ArchiveBuilder
  = ArchiveBuilder
  { archiveBuilderRead :: Word64 -> IO ByteString
  {- ^ Read the given number of bytes from the current seek position.

  Seek position is advanced by the number of bytes read.
  -}
  , archiveBuilderWrite :: ByteString -> IO ()
  {- ^ Write the value to the current seek position.

  Seek position is advanced by the number of bytes written.
  -}
  , archiveBuilderSeek :: Word64 -> IO ()
  -- ^ Set the seek position.
  , archiveBuilderTell :: IO Word64
  -- ^ Get the seek position.
  , archiveBuilderClose :: IO ()
  {- ^ Close the archive.

  Archive operations should not be used after closing.
  -}
  , archiveBuilderChanges :: IORef (DList Change)
  }

data Change
  = Added
      -- | Local header offset
      !Word32
      !FileInfo
      -- | File name
      !ByteString

-- | Fields common to @local file header@ and @central directory header@.
data FileInfo
  = FileInfo
  { fileInfoVersionNeededToExtract :: !Word16
  , fileInfoGeneralPurposeBitFlag :: !Word16
  , fileInfoCompressionMethod :: !Word16
  , fileInfoLastModFileTime :: !Word16
  , fileInfoLastModFileDate :: !Word16
  , fileInfoCrc32 :: !Word32
  , fileInfoCompressedSize :: !Word32
  , fileInfoUncompressedSize :: !Word32
  , fileInfoFileNameLength :: !Word16
  , fileInfoExtraFieldLength :: !Word16
  }

writeFileInfo :: FileInfo -> ArchiveBuilder -> IO ()
writeFileInfo fileInfo archive = do
  writeLE16 archive $ fileInfoVersionNeededToExtract fileInfo
  writeLE16 archive $ fileInfoGeneralPurposeBitFlag fileInfo
  writeLE16 archive $ fileInfoCompressionMethod fileInfo
  writeLE16 archive $ fileInfoLastModFileTime fileInfo
  writeLE16 archive $ fileInfoLastModFileDate fileInfo
  writeLE32 archive $ fileInfoCrc32 fileInfo
  writeLE32 archive $ fileInfoCompressedSize fileInfo
  writeLE32 archive $ fileInfoUncompressedSize fileInfo
  writeLE16 archive $ fileInfoFileNameLength fileInfo
  writeLE16 archive $ fileInfoExtraFieldLength fileInfo

create :: FilePath -> IO ArchiveBuilder
create path = do
  handle <- openFile path ReadWriteMode
  hSetFileSize handle 0
  changesRef <- newIORef DList.empty
  pure
    ArchiveBuilder
      { archiveBuilderRead = handleRead handle
      , archiveBuilderWrite = handleWrite handle
      , archiveBuilderSeek = handleSeek handle
      , archiveBuilderTell = handleTell handle
      , archiveBuilderClose = handleClose handle
      , archiveBuilderChanges = changesRef
      }

addFile ::
  {-| File name

  Preconditions:
  * @file name's length in bytes when UTF-8 encoded < 2^16@
  * @file's size < 2^32@
  -}
  FilePath ->
  ArchiveBuilder ->
  IO ()
addFile fileName archive = do
  content <- ByteString.readFile fileName
  addContent (Utf8.fromString fileName) content archive

addContent ::
  -- | File name (precondition: @length < 2^16@)
  ByteString ->
  -- | Content (precondition: @length < 2^32@)
  ByteString ->
  ArchiveBuilder ->
  IO ()
addContent fileName content = do
  let compressed@(Compressed uncompressedSize crc32 _compression compressedContent) = compress (Deflate Normal) content
  if uncompressedSize > fromIntegral (ByteString.length compressedContent)
    then addCompressedContent fileName compressed
    else addCompressedContent fileName (Compressed uncompressedSize crc32 Stored content)

data Compression
  = -- | No compression
    Stored
  | Deflate !DeflateMode
  deriving (Show, Eq)

data DeflateMode
  = Normal
  | Maximum
  | Fast
  | SuperFast
  deriving (Show, Eq)

data Compressed
  = Compressed
      -- | Uncompressed size (precondition: @length < 2^32@)
      !Word64
      -- | CRC-32 of uncompressed content
      !Word32
      -- | Compression used
      !Compression
      -- | Compressed content (precondition: @length < 2^32@)
      !ByteString

compress :: Compression -> ByteString -> Compressed
compress compression content =
  Compressed uncompressedSize (CRC32.crc32 content) compression $
    case compression of
      Stored -> content
      Deflate mode ->
        let compressionParams =
              Zlib.defaultCompressParams
                { Zlib.compressLevel =
                    case mode of
                      Normal -> Zlib.defaultCompression
                      Maximum -> Zlib.CompressionLevel 9
                      Fast -> Zlib.CompressionLevel 3 -- What do other implementations use?
                      SuperFast -> Zlib.CompressionLevel 1
                , Zlib.compressMethod = Zlib.deflateMethod
                }
        in LazyByteString.toStrict (Zlib.compressWith compressionParams $ LazyByteString.fromStrict content)
  where
    uncompressedSize = fromIntegral (ByteString.length content) :: Word64

addCompressedContent ::
  -- | File name (precondition: @length < 2^16@)
  ByteString ->
  -- | Compressed content
  Compressed ->
  ArchiveBuilder ->
  IO ()
addCompressedContent fileName (Compressed uncompressedSize crc32 compression compressedContent) archive = do
  localHeaderOffset <- do
    offset <- archiveBuilderTell archive
    if offset < 2 ^ (32 :: Int)
      then pure (fromIntegral offset :: Word32)
      else error $ "ZIP file is too large (ZIP64 not yet supported)"

  uncompressedSize' <- do
    if uncompressedSize < 2 ^ (32 :: Int)
      then pure (fromIntegral uncompressedSize :: Word32)
      else error $ "ZIP file entry's uncompressed content is too large (ZIP64 not yet supported)"

  compressedSize <- do
    let len = ByteString.length compressedContent
    if len < 2 ^ (32 :: Int)
      then pure (fromIntegral len :: Word32)
      else error $ "ZIP file entry's compressed content is too large (ZIP64 not yet supported)"

  fileNameLength <- do
    let len = ByteString.length fileName
    if len < 2 ^ (16 :: Int)
      then pure (fromIntegral len :: Word16)
      else error $ "ZIP file entry's file name is too large (ZIP64 not yet supported)"

  let
    fileInfo =
      FileInfo
        { fileInfoVersionNeededToExtract = 0xFF00
        , fileInfoGeneralPurposeBitFlag =
            -- Bit 15: Reserved by PKWARE.
            (0 `shiftL` 15)
              .|.
              -- Bit 14: Reserved by PKWARE for alternate streams.
              (0 `shiftL` 14)
              .|.
              -- Bit 13: Set when encrypting the Central Directory to indicate selected data values in the Local Header are masked to hide their actual values.
              (0 `shiftL` 13)
              .|.
              -- Bit 12: Reserved by PKWARE for enhanced compression.
              (0 `shiftL` 12)
              .|.
              -- Bit 11: Language encoding flag (EFS).  If this bit is set, the filename and comment fields for this file MUST be encoded using UTF-8.
              (0 `shiftL` 11)
              .|.
              -- Bit 10: Currently unused.
              (0 `shiftL` 10)
              .|.
              -- Bit 9: Currently unused.
              (0 `shiftL` 9)
              .|.
              -- Bit 8: Currently unused.
              (0 `shiftL` 8)
              .|.
              -- Bit 7: Currently unused.
              (0 `shiftL` 7)
              .|.
              -- Bit 6: Strong encryption.
              (0 `shiftL` 6)
              .|.
              -- Bit 5: If this bit is set, this indicates that the file is compressed patched data.
              (0 `shiftL` 5)
              .|.
              -- Bit 4: Reserved for use with method 8, for enhanced deflating.
              (0 `shiftL` 4)
              .|.
              -- Bit 3: If this bit is set, the fields crc-32, compressed size and uncompressed size are set to zero in the local header.
              (0 `shiftL` 3)
              .|. ( case compression of
                      Stored -> 0
                      Deflate mode ->
                        {- (For Methods 8 and 9 - Deflating)

                            Bit 2  Bit 1
                        -}
                        case mode of
                          -- 0      0    Normal (-en) compression option was used.
                          Normal -> (0 `shiftL` 2) .|. (0 `shiftL` 1)
                          -- 0      1    Maximum (-exx/-ex) compression option was used.
                          Maximum -> (0 `shiftL` 2) .|. (1 `shiftL` 1)
                          -- 1      0    Fast (-ef) compression option was used.
                          Fast -> (1 `shiftL` 2) .|. (0 `shiftL` 1)
                          -- 1      1    Super Fast (-es) compression option was used.
                          SuperFast -> (1 `shiftL` 2) .|. (1 `shiftL` 1)
                  )
              .|.
              -- Bit 0: If set, indicates that the file is encrypted.
              0
        , fileInfoCompressionMethod =
            case compression of
              Stored -> 0
              Deflate{} -> 8
        , fileInfoLastModFileTime = 0
        , fileInfoLastModFileDate = 0
        , fileInfoCrc32 = crc32
        , fileInfoCompressedSize = compressedSize
        , fileInfoUncompressedSize = uncompressedSize'
        , fileInfoFileNameLength = fileNameLength
        , fileInfoExtraFieldLength = 0
        }
  writeLocalFileHeader fileInfo fileName archive
  archiveBuilderWrite archive compressedContent

  let change = Added localHeaderOffset fileInfo fileName
  modifyIORef' (archiveBuilderChanges archive) (`DList.snoc` change)

writeLocalFileHeader ::
  FileInfo ->
  -- | File name
  ByteString ->
  ArchiveBuilder ->
  IO ()
writeLocalFileHeader fileInfo fileName archive = do
  -- local file header signature
  writeLE32 archive 0x04034b50

  writeFileInfo fileInfo archive

  -- file name
  archiveBuilderWrite archive fileName

  -- extra field
  pure ()

finish_ :: ArchiveBuilder -> IO ()
finish_ archive = do
  centralDirectoryOffset <- do
    pos <- archiveBuilderTell archive
    if pos < 2 ^ (32 :: Int)
      then pure (fromIntegral pos :: Word32)
      else error "Central directory offset is too large (ZIP64 not yet supported)"

  changes <- fmap DList.toList . readIORef $ archiveBuilderChanges archive
  centralDirectoryCount <- do
    count <-
      foldlM
        ( \acc change -> do
            case change of
              Added localHeaderOffset fileInfo fileName -> do
                writeCentralDirectoryHeader localHeaderOffset fileInfo fileName archive
                pure (acc + 1 :: Word64)
        )
        0
        changes

    if count < 2 ^ (16 :: Int)
      then pure (fromIntegral count :: Word16)
      else error "Central directory has too many entries (ZIP64 not yet supported)"

  endOfCentralDirectoryOffset <- do
    pos <- archiveBuilderTell archive
    if pos < 2 ^ (32 :: Int)
      then pure (fromIntegral pos :: Word32)
      else error "End-of-central-directory offset is too large (ZIP64 not yet supported)"
  let centralDirectorySize = endOfCentralDirectoryOffset - centralDirectoryOffset

  writeEndOfCentralDirectoryRecord
    centralDirectoryCount
    centralDirectorySize
    centralDirectoryOffset
    archive
  archiveBuilderClose archive

writeCentralDirectoryHeader ::
  -- | Local header offset
  Word32 ->
  FileInfo ->
  -- | File name
  ByteString ->
  ArchiveBuilder ->
  IO ()
writeCentralDirectoryHeader localHeaderOffset fileInfo fileName archive = do
  -- central file header signature
  writeLE32 archive 0x02014b50

  -- version made by
  writeLE16 archive 0

  writeFileInfo fileInfo archive

  -- file comment length
  writeLE16 archive 0

  -- disk number start
  writeLE16 archive 0

  -- internal file attributes
  writeLE16 archive 0

  -- external file attributes
  writeLE32 archive 0

  -- relative offset of local header
  writeLE32 archive localHeaderOffset

  -- file name
  archiveBuilderWrite archive fileName

  -- extra field
  pure ()

  -- file comment
  pure ()

writeEndOfCentralDirectoryRecord ::
  -- | Number of entries in the central directory
  Word16 ->
  -- | Size of the central directory
  Word32 ->
  -- | Offset of the central directory
  Word32 ->
  ArchiveBuilder ->
  IO ()
writeEndOfCentralDirectoryRecord centralDirectoryCount centralDirectorySize centralDirectoryOffset archive = do
  -- end of central dir signature
  writeLE32 archive 0x06054b50

  -- number of this disk
  writeLE16 archive 0

  -- number of the disk with the start of the central directory
  writeLE16 archive 0

  -- total number of entries in the central directory on this disk
  writeLE16 archive centralDirectoryCount

  -- total number of entries in the central directory
  writeLE16 archive centralDirectoryCount

  -- size of the central directory
  writeLE32 archive centralDirectorySize

  -- offset of start of central directory with respect to the starting disk number
  writeLE32 archive centralDirectoryOffset

  -- .ZIP file comment length
  writeLE16 archive 0

  -- .ZIP file comment
  pure ()

-- Little-endian encoding

writeLE16 :: ArchiveBuilder -> Word16 -> IO ()
writeLE16 archive value = do
  archiveBuilderWrite archive . ByteString.singleton . fromIntegral $ 0x000000FF .&. value
  archiveBuilderWrite archive . ByteString.singleton . fromIntegral $
    (0x0000FF00 .&. value) `shiftR` 8

writeLE32 :: ArchiveBuilder -> Word32 -> IO ()
writeLE32 archive value = do
  archiveBuilderWrite archive . ByteString.singleton . fromIntegral $ 0x000000FF .&. value
  archiveBuilderWrite archive . ByteString.singleton . fromIntegral $
    (0x0000FF00 .&. value) `shiftR` 8
  archiveBuilderWrite archive . ByteString.singleton . fromIntegral $
    (0x00FF0000 .&. value) `shiftR` 16
  archiveBuilderWrite archive . ByteString.singleton . fromIntegral $
    (0xFF000000 .&. value) `shiftR` 24

-- `Handle` operations

handleRead ::
  Handle ->
  -- | Number of bytes
  Word64 ->
  IO ByteString
handleRead handle size = ByteString.hGet handle (fromIntegral size)

handleWrite :: Handle -> ByteString -> IO ()
handleWrite handle value = ByteString.hPut handle value

handleSeek ::
  Handle ->
  -- | Absolute offset
  Word64 ->
  IO ()
handleSeek handle pos = hSeek handle AbsoluteSeek (fromIntegral pos)

handleTell ::
  Handle ->
  IO Word64
handleTell = fmap fromIntegral . hTell

handleClose :: Handle -> IO ()
handleClose = hClose
