-- ghost-control is the server-side tool to add new repositories and users.
{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn, withFile, IOMode(WriteMode))

versionString :: String
versionString =
  "ghost " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdInit, cmdAddUser
    ]
  &= summary versionString
  &= program "ghost-control"

data Cmd = Init
    { initPublicKeyPath :: String
    , initSuperuserName :: String
    }
  | AddUser
    { addUserPublicKey :: String
    , addUserName :: String
    }
  deriving (Data, Typeable)

cmdInit :: Cmd
cmdInit = Init
  { initPublicKeyPath = ""
    &= typ "PUBLIC KEY PATH"
    &= help "path to Ghost superuser's public key"
    &= explicit
    &= name "k"
    &= name "key"
  , initSuperuserName = ""
    &= typ "USERNAME"
    &= help "superuser name"
    &= explicit
    &= name "u"
    &= name "user"
  } &= help "Initialize Ghost with a superuser name and public key."

cmdAddUser :: Cmd
cmdAddUser = AddUser
  { addUserPublicKey = ""
    &= typ "PUBLIC KEY"
    &= help "user's public key"
    &= explicit
    &= name "k"
    &= name "key"
  , addUserName = ""
    &= typ "USERNAME"
    &= help "user name"
    &= explicit
    &= name "u"
    &= name "user"
  } &= help "Add a user to Ghost."

processCmd :: Cmd -> IO ()
processCmd Init{..} = do
  b <- doesFileExist initPublicKeyPath
  if b
    then do
      content <- readFile initPublicKeyPath
      putStrLn content
      putStrLn initSuperuserName
    else putStrLn $ initPublicKeyPath ++ " not found."

processCmd AddUser{..} = do
  putStrLn $ authorizedKeysEntry addUserName addUserPublicKey

authorizedKeysEntry:: String -> String -> String
authorizedKeysEntry username key = concat
  [ "command='/home/ghost/bin/ghost-command --user=", username, "'"
  , ",no-port-forwarding"
  , ",no-X11-forwarding"
  , ",no-agent-forwarding"
  , ",no-pty "
  , key
  ]
