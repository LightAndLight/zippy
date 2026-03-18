module Zip.Streaming
  ( -- * Reading
    readByteStream

    -- * Writing
  , addByteStream
  )
where

import Codec.Compression.Zlib (CompressionLevel (..), WindowBits, defaultWindowBits)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Digest.CRC32 as CRC32
import Data.Streaming.Zlib (PopperRes (..))
import qualified Data.Streaming.Zlib as Streaming.Zlib
import Data.Word (Word32, Word64)
import Streaming.ByteString (toChunks)
import qualified Streaming.ByteString
import Streaming.ByteString.Internal (ByteStream (..), defaultChunkSize)
import qualified Streaming.Prelude as Streaming
import Zip (Archive, ArchiveBuilder)
import Zip.Archive
  ( Compression (..)
  , ContentSummary (..)
  , LocalFileHeaderData (..)
  , RepeatedEntryException (..)
  , add
  , archiveCentralDirectory
  , archiveFile
  , centralDirectoryHeaderLocalHeader
  , deflateModeCompressionLevel
  , fileRead
  , fileSeek
  , findInCentralDirectory
  , localFileHeaderCompressionMethod
  , localFileHeaderData
  )

{-| Stream data to an archive.

Unlike 'Zip.addContent', the 'Compression' method is chosen up-front to allow
proper streaming to the archive. 'Zip.addContent' keeps all the uncompressed and
compressed data in memory to decide which version should be written to the archive.
In contrast, by the time 'addByteStream' knows the sizes of the uncompressed and compressed data,
the data has already been written to the archive.
-}
addByteStream ::
  -- | File name
  ByteString ->
  -- | Compression method
  Compression ->
  -- | Uncompressed content
  ByteStream IO () ->
  ArchiveBuilder ->
  IO ()
addByteStream fileName compression content =
  add fileName $
    case compression of
      Stored -> writeStored
      Deflate mode -> writeDeflate mode
  where
    writeStored write = do
      (uncompressedSize, crc32) <- Streaming.mapM_ write . toChunks $ storing content
      pure $ ContentSummary uncompressedSize crc32 compression uncompressedSize

    writeDeflate mode write = do
      (uncompressedSize, crc32, compressedSize) <-
        Streaming.mapM_ write . toChunks $
          compressing (deflateModeCompressionLevel mode) defaultWindowBits content
      pure $ ContentSummary uncompressedSize crc32 compression compressedSize

{-| Stream data from an archive.

The resulting 'ByteStream' assumes exclusive access to the underlying file's seek position
until the stream has been consumed. If the file's seek position is changed partway through
processing the stream, then it will read from the incorrect position.
-}
readByteStream ::
  -- | File name
  ByteString ->
  Archive ->
  IO (Maybe (ByteStream IO ()))
readByteStream fileName archive = do
  mCentralDirectoryHeader <- findInCentralDirectory fileName (archiveCentralDirectory archive)
  case mCentralDirectoryHeader of
    [] -> pure Nothing
    [centralDirectoryHeader] -> do
      localFileHeader <- centralDirectoryHeaderLocalHeader centralDirectoryHeader
      compressedStream <- do
        LocalFileHeaderData dataOffset dataSize <- localFileHeaderData localFileHeader
        pure $ reading dataOffset dataSize
      compressionMethod <- localFileHeaderCompressionMethod localFileHeader
      uncompressedContent <-
        case compressionMethod of
          0 ->
            -- The file is stored (no compression)
            pure $ compressedStream
          8 ->
            -- The file is Deflated
            pure $ decompressing defaultWindowBits compressedStream
          _ ->
            error $ "Compression method " ++ show compressionMethod ++ " not yet supported"
      pure $ Just uncompressedContent
    entries ->
      throwIO $ RepeatedEntryException fileName (fromIntegral $ length entries)
  where
    reading ::
      -- \| File offset
      Word64 ->
      -- \| Compressed size
      Word64 ->
      ByteStream IO ()
    reading offset size =
      Go $ do
        fileSeek (archiveFile archive) offset
        pure $ loop size
      where
        loop remaining =
          unless (remaining == 0) . Go $ do
            let chunkSize = min remaining (fromIntegral defaultChunkSize)
            chunk <- fileRead (archiveFile archive) chunkSize
            remaining' <- pure $! remaining - chunkSize
            pure . Chunk chunk $ loop remaining'

mapAccum ::
  Monad m =>
  (r -> ByteString -> (ByteString, r)) ->
  r ->
  ByteStream m () ->
  ByteStream m r
mapAccum f z content = do
  let
    loop acc bs =
      case bs of
        Chunk chunk rest -> do
          (chunk', acc') <- pure $! f acc chunk
          Chunk chunk' $ loop acc' rest
        Go m -> Go (fmap (loop acc) m)
        Empty () -> return acc

  loop z content

storing ::
  -- | Uncompressed content
  ByteStream IO () ->
  {-| Compressed content

  Final value: @(uncompressed size, CRC-32)@
  -}
  ByteStream IO (Word64, Word32)
storing =
  mapAccum
    ( \(uncompressedSize, crc32) chunk ->
        let
          uncompressedSize' = uncompressedSize + fromIntegral (ByteString.length chunk)
          crc32' = CRC32.crc32Update crc32 chunk
        in
          uncompressedSize' `seq` crc32' `seq` (chunk, (uncompressedSize', crc32'))
    )
    (0, 0)

{-
The following code is adapted from `streaming-utils-0.2.6.0`.

---

Copyright (c) 2015, Michael Thompson, 2014 Gabriel Gonzalez, 2014 Renzo Carbonara

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of michaelt nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

compressing ::
  CompressionLevel ->
  WindowBits ->
  -- | Uncompressed content
  ByteStream IO () ->
  {-| Compressed content

  Final value: @(uncompressed size, CRC-32, compressed size)@
  -}
  ByteStream IO (Word64, Word32, Word64)
compressing (CompressionLevel clevel) wbits p0 = do
  def <- liftIO $ Streaming.Zlib.initDeflate clevel wbits

  let
    loop ::
      -- \| Current uncompressed size
      Word64 ->
      -- \| Current CRC-32
      Word32 ->
      -- \| Current compressed size
      Word64 ->
      ByteStream IO () ->
      -- \| Final value: @(new uncompressed size, CRC-32, new compressed size)@
      ByteStream IO (Word64, Word32, Word64)
    loop uncompressedSize crc32 compressedSize bs =
      case bs of
        Chunk chunk rest -> do
          uncompressedSize' <- pure $! uncompressedSize + fromIntegral (ByteString.length chunk)
          crc32' <- pure $! CRC32.crc32Update crc32 chunk
          popper <- liftIO (Streaming.Zlib.feedDeflate def chunk)
          compressedSize' <- fromPopper compressedSize popper
          loop uncompressedSize' crc32' compressedSize' rest
        Go m -> Go (fmap (loop uncompressedSize crc32 compressedSize) m)
        Empty () -> return (uncompressedSize, crc32, compressedSize)

  (uncompressedSize, crc32, compressedSize) <- loop 0 0 0 p0
  compressedSize' <- fromPopper compressedSize $ Streaming.Zlib.finishDeflate def
  return (uncompressedSize, crc32, compressedSize')

decompressing ::
  WindowBits ->
  ByteStream IO r ->
  ByteStream IO r
decompressing wbits p0 = do
  inf <- liftIO $ Streaming.Zlib.initInflate wbits
  r <- Streaming.ByteString.for p0 $ \bs -> do
    popper <- liftIO $ Streaming.Zlib.feedInflate inf bs
    fromPopper 0 popper
  bs <- liftIO $ Streaming.Zlib.finishInflate inf
  unless (ByteString.null bs) (Streaming.ByteString.chunk bs)
  return r

fromPopper ::
  -- | Current compressed size
  Word64 ->
  Streaming.Zlib.Popper ->
  -- | Final value: new compressed size
  ByteStream IO Word64
fromPopper currentSize pop = loop currentSize
  where
    loop ::
      -- \| Current compressed size
      Word64 ->
      -- \| Final value: new compressed size
      ByteStream IO Word64
    loop count = do
      mbs <- liftIO pop
      case mbs of
        PRDone -> Empty count
        PRError e -> Go (throwIO e)
        PRNext bs -> do
          count' <- pure $! count + fromIntegral (ByteString.length bs)
          Chunk bs (loop count')
