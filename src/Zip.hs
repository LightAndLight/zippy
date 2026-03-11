-- | https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
module Zip where

import qualified Codec.Compression.Zlib as Zlib
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Digest.CRC32 as CRC32
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word16, Word32, Word64)
import System.IO (Handle, IOMode (..), SeekMode (..), hClose, hSeek, hSetFileSize, hTell, openFile)

data Archive
  = Archive
  { archiveRead :: Word64 -> IO ByteString
  {- ^ Read the given number of bytes from the current seek position.

  Seek position is advanced by the number of bytes read.
  -}
  , archiveWrite :: ByteString -> IO ()
  {- ^ Write the value to the current seek position.

  Seek position is advanced by the number of bytes written.
  -}
  , archiveSeek :: Word64 -> IO ()
  -- ^ Set the seek position.
  , archiveTell :: IO Word64
  -- ^ Get the seek position.
  , archiveClose :: IO ()
  {- ^ Close the archive.

  Archive operations should not be used after closing.
  -}
  , archiveChanges :: IORef (DList Change)
  }

data Change
  = Added
      -- | Local header offset
      Word32
      -- | File name
      ByteString

create :: FilePath -> IO Archive
create path = do
  handle <- openFile path ReadWriteMode
  hSetFileSize handle 0
  changesRef <- newIORef DList.empty
  pure
    Archive
      { archiveRead = handleRead handle
      , archiveWrite = handleWrite handle
      , archiveSeek = handleSeek handle
      , archiveTell = handleTell handle
      , archiveClose = handleClose handle
      , archiveChanges = changesRef
      }

add ::
  -- | File name (precondition: @length < 2^16@)
  ByteString ->
  -- | Content (precondition: @length < 2^32@)
  ByteString ->
  Archive ->
  IO ()
add fileName content archive = do
  localHeaderOffset <- do
    offset <- archiveTell archive
    if offset < 2 ^ (32 :: Int)
      then pure (fromIntegral offset :: Word32)
      else error $ "ZIP file is too large (ZIP64 not yet supported)"

  uncompressedSize <- do
    let len = ByteString.length content
    if len < 2 ^ (32 :: Int)
      then pure (fromIntegral len :: Word32)
      else error $ "ZIP file entry's uncompressed content is too large (ZIP64 not yet supported)"

  crc32 <- pure $! CRC32.crc32 content

  let compressionParams =
        Zlib.defaultCompressParams
          { Zlib.compressLevel = Zlib.compressionLevel (-1)
          , Zlib.compressMethod = Zlib.deflateMethod
          }
  compressedContent <-
    pure $!
      LazyByteString.toStrict (Zlib.compressWith compressionParams $ LazyByteString.fromStrict content)

  compressedSize <- do
    let len = ByteString.length compressedContent
    if len < 2 ^ (32 :: Int)
      then pure (fromIntegral len :: Word32)
      else error $ "ZIP file entry's compressed content is too large (ZIP64 not yet supported)"

  writeLocalFileHeader crc32 compressedSize uncompressedSize
  archiveWrite archive compressedContent

  let change = Added localHeaderOffset fileName
  modifyIORef' (archiveChanges archive) (`DList.snoc` change)
  where
    writeLocalFileHeader ::
      -- \| CRC-32 of uncompressed data
      Word32 ->
      -- \| Compressed size
      Word32 ->
      -- \| Uncompressed size
      Word32 ->
      IO ()
    writeLocalFileHeader crc32 compressedSize uncompressedSize = do
      -- local file header signature
      writeLE32 archive 0x04034b50

      -- version needed to extract
      writeLE16 archive 0xFF00

      -- general purpose bit flag
      writeLE16 archive $
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
          .|.
          {- (For Methods 8 and 9 - Deflating)

          Bit 2  Bit 1
            0      0    Normal (-en) compression option was used.
          -}
          -- Bit 2:
          (0 `shiftL` 2)
          .|.
          -- Bit 1:
          (0 `shiftL` 1)
          .|.
          -- Bit 0: If set, indicates that the file is encrypted.
          0

      -- compression method
      writeLE16 archive 8 -- The file is Deflated

      -- last mod file time
      writeLE16 archive 0

      -- last mod file date
      writeLE16 archive 0

      -- crc-32
      writeLE32 archive crc32

      -- compressed size
      writeLE32 archive compressedSize

      -- uncompressed size
      writeLE32 archive uncompressedSize

      fileNameLength <- do
        let len = ByteString.length fileName
        if len < 2 ^ (16 :: Int)
          then pure (fromIntegral len :: Word16)
          else error $ "ZIP file entry's file name is too large (ZIP64 not yet supported)"
      -- file name length
      writeLE16 archive fileNameLength

      -- extra field length
      writeLE16 archive 0

      -- file name
      archiveWrite archive fileName

      -- extra field
      pure ()

close :: Archive -> IO ()
close archive = do
  changes <- fmap DList.toList . readIORef $ archiveChanges archive
  for_ changes $ \change -> do
    case change of
      Added localHeaderOffset fileName -> writeCentralDirectoryHeader localHeaderOffset fileName
  writeEndOfCentralDirectoryRecord
  archiveClose archive
  where
    writeCentralDirectoryHeader ::
      -- \| Local header offset
      Word32 ->
      -- \| File name (precondition: @length < 2^32@)
      ByteString ->
      IO ()
    writeCentralDirectoryHeader localHeaderOffset fileName = do
      -- central file header signature
      writeLE32 archive 0x02014b50

      -- version made by
      writeLE16 archive 0

      -- version needed to extract
      writeLE16 archive 0

      -- general purpose bit flag
      writeLE16 archive 0

      -- compression method
      writeLE16 archive 0

      -- last mod file time
      writeLE16 archive 0

      -- last mod file date
      writeLE16 archive 0

      -- crc-32
      writeLE32 archive 0

      -- compressed size
      writeLE32 archive 0

      -- uncompressed size
      writeLE32 archive 0

      -- file name length
      writeLE16 archive . fromIntegral $ ByteString.length fileName

      -- extra field length
      writeLE16 archive 0

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
      archiveWrite archive fileName

      -- extra field
      pure ()

      -- file comment
      pure ()

    writeEndOfCentralDirectoryRecord = do
      -- end of central dir signature
      writeLE32 archive 0x06054b50

      -- number of this disk
      writeLE16 archive 0

      -- number of the disk with the start of the central directory
      writeLE16 archive 0

      -- total number of entries in the central directory on this disk
      writeLE16 archive 0

      -- size of the central directory
      writeLE32 archive 0

      -- offset of start of central directory with respect to the starting disk number
      writeLE32 archive 0

      -- .ZIP file comment length
      writeLE16 archive 0

      -- .ZIP file comment
      pure ()

-- Little-endian encoding

writeLE16 :: Archive -> Word16 -> IO ()
writeLE16 archive value = do
  archiveWrite archive . ByteString.singleton . fromIntegral $ 0x000000FF .&. value
  archiveWrite archive . ByteString.singleton . fromIntegral $ (0x0000FF00 .&. value) `shiftR` 1

writeLE32 :: Archive -> Word32 -> IO ()
writeLE32 archive value = do
  archiveWrite archive . ByteString.singleton . fromIntegral $ 0x000000FF .&. value
  archiveWrite archive . ByteString.singleton . fromIntegral $ (0x0000FF00 .&. value) `shiftR` 1
  archiveWrite archive . ByteString.singleton . fromIntegral $ (0x00FF0000 .&. value) `shiftR` 2
  archiveWrite archive . ByteString.singleton . fromIntegral $ (0xFF000000 .&. value) `shiftR` 3

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
