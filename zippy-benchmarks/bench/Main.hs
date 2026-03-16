module Main where

import Control.Exception (evaluate)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Maybe (fromJust)
import qualified Streaming.ByteString
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Weigh
import qualified Zip
import qualified Zip.Streaming

main :: IO ()
main =
  withSystemTempDirectory "zippy-benchmarks" $ \tmpDir -> do
    Zip.create (tmpDir </> "read100MBZeroes.zip") $ \archive -> do
      let input = Streaming.ByteString.take (100 * 1000 * 1000) $ Streaming.ByteString.repeat 0
      Zip.Streaming.addByteStream (ByteString.Char8.pack "file") (Zip.Deflate Zip.Normal) input archive

    Weigh.mainWith $ do
      Weigh.setColumns [Weigh.Case, Weigh.Allocated, Weigh.Live, Weigh.Max, Weigh.MaxOS, Weigh.GCs]

      Weigh.action "write 100MB of zeroes" $ do
        input <- evaluate $ ByteString.replicate (100 * 1000 * 1000) 0
        Zip.create (tmpDir </> "write100MBZeroes.zip") $
          Zip.addContent (ByteString.Char8.pack "file") input

      Weigh.action "write streaming 100MB of zeroes" $ do
        let input = Streaming.ByteString.take (100 * 1000 * 1000) $ Streaming.ByteString.repeat 0
        Zip.create (tmpDir </> "writeStreaming100MBZeroes.zip") $
          Zip.Streaming.addByteStream (ByteString.Char8.pack "file") (Zip.Deflate Zip.Normal) input

      Weigh.action "read 100MB of zeroes" $ do
        content <-
          Zip.withArchive (tmpDir </> "read100MBZeroes.zip") $
            Zip.readContent (ByteString.Char8.pack "file")
        pure $! ByteString.foldl' (+) 0 (fromJust content)

      Weigh.action "read streaming 100MB of zeroes" $ do
        Zip.withArchive (tmpDir </> "read100MBZeroes.zip") $ \archive -> do
          content <- Zip.Streaming.readByteStream (ByteString.Char8.pack "file") archive
          Streaming.ByteString.fold_ (+) 0 id (fromJust content)
