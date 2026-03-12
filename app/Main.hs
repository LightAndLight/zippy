module Main where

import Control.Applicative (many)
import Data.Foldable (for_)
import qualified Options.Applicative as Options
import qualified Zip

data Cli
  = Create
      -- | Input files
      [FilePath]
      -- | Output file
      FilePath

cliParser :: Options.Parser Cli
cliParser =
  Options.hsubparser (Options.command "create" $ Options.info createParser Options.fullDesc)
  where
    createParser =
      Create
        <$> many (Options.strArgument $ Options.metavar "FILE" <> Options.help "Input file(s)")
        <*> Options.strOption
          (Options.long "output" <> Options.short 'o' <> Options.metavar "FILE" <> Options.help "Output file")

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info cliParser Options.fullDesc
  case cli of
    Create inputs output -> create inputs output

create ::
  -- | Input files
  [FilePath] ->
  -- | Output file
  FilePath ->
  IO ()
create inputs output = do
  archive <- Zip.create output
  for_ inputs $ \input -> do
    putStrLn $ "Adding " ++ input ++ "..."
    Zip.addFile input archive
  Zip.close archive
  putStrLn $ "Created " ++ output
