{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

import System.FilePath ((</>))
import System.IO (hPutStrLn, withFile, IOMode(WriteMode))

versionString :: String
versionString =
  "ghost " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ shell
    ]
  &= summary versionString
  &= program "ghost-shell"

data Cmd = Shell { shellCommand :: String }
  deriving (Data, Typeable)

shell :: Cmd
shell = Shell
  { shellCommand = ""
    &= typ "COMMAND"
    &= help "non-interactive command to execute"
    &= explicit
    &= name "c"
  } &= help "Execute a restricted shell for Ghost operations."

processCmd :: Cmd -> IO ()
processCmd Shell{..} = do
  putStrLn "Ghost shell."
  withFile ("/home/ghost" </> "ghost-shell.txt") WriteMode $ \h -> do
    hPutStrLn h $
      "shellCommand:\n" ++ shellCommand
