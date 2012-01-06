{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

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

data Cmd = Shell
  deriving (Data, Typeable)

shell :: Cmd
shell = Shell
  &= help "Execute a restricted shell for Ghost operations."

processCmd :: Cmd -> IO ()
processCmd Shell{..} = putStrLn "Ghost shell."
