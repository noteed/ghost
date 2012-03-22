-- ghost-control is the server-side tool to add new repositories and users.
{-# Language DeriveDataTypeable #-}
{-# Language RecordWildCards #-}

module Main where

import Paths_ghost (version)
import Control.Monad (when)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

import System.Directory
  (copyFile, createDirectoryIfMissing, doesFileExist, getHomeDirectory)
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

data Cmd =
    Init
  | AddUser
    { addUserPublicKeyPath :: String
    , addUserName :: String
    }
  deriving (Data, Typeable)

cmdInit :: Cmd
cmdInit = Init
  &= help "Initialize Ghost (server-side)."

cmdAddUser :: Cmd
cmdAddUser = AddUser
  { addUserPublicKeyPath = ""
    &= typ "PUBLIC KEY PATH"
    &= help "path to user's public key"
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
    &= explicit
    &= name "add-user"

processCmd :: Cmd -> IO ()
processCmd Init{..} = do
  putStrLn "Ghost server-side initialization."
  home <- getHomeDirectory
  let authorizedKeys = home </> ".ssh" </> "authorized_keys"
      administratorKeys = home </> "administrator" </> "keys"

  _ <- createDirectoryIfMissing True $ home </> "administrator"
  _ <- createDirectoryIfMissing True $ home </> "user"
  _ <- createDirectoryIfMissing True $ home </> "gits"
  _ <- createDirectoryIfMissing True $ home </> "run/staging/nginx"
  _ <- createDirectoryIfMissing True $ home </> "run/production/nginx"
  withFile (home </> "run" </> "nginx.conf") WriteMode $ \h -> do
    hPutStrLn h $ "include " ++ home ++ "/run/staging/nginx/*.conf;"
    hPutStrLn h $ "include " ++ home ++ "/run/production/nginx/*.conf;"

  b <- doesFileExist $ authorizedKeys
  if b
    then do
      putStrLn $ "Prior authorized_keys file found, " ++
        "copying it as administrator key."
      -- TODO make sure there is a single key in it. For now, it assumes this
      -- is the key copied by setup-ghost-archlinux.sh.
      copyFile authorizedKeys administratorKeys
    else putStrLn $ "No prior authorized_keys file found."
  refreshAuthorizedKeys

  putStrLn "Ghost server-side initialization complete."
  putStrLn "Add the following line to your main Nginx configuration"
  putStrLn "file (inside an `http {..}` block.):"
  putStrLn $ "  include " ++ home ++ "/run/nginx.conf;"

processCmd AddUser{..} = do
  b <- doesFileExist addUserPublicKeyPath
  if b
    then do
      content <- readFile addUserPublicKeyPath
      putStrLn $ authorizedKeysEntry addUserName content
    else putStrLn $ addUserPublicKeyPath ++ " not found."

-- | Given a username and its public SSH key, return a string
-- that can be appended to a SSH authorized_keys file.
-- The entry establishes the binding between a username and its
-- SSH key, so that ghost-command 'knows' who is running it.
authorizedKeysEntry:: String -> String -> String
authorizedKeysEntry username key = concat
  [ "command=\"/home/ghost/bin/ghost-command --user=", username, "\""
  , ",no-port-forwarding"
  , ",no-X11-forwarding"
  , ",no-agent-forwarding"
  , ",no-pty "
  , key
  ]

-- | Similar to `authorizedKeysEntry` but for an administrator
-- role. The entry does nothing special (i.e. the user can log
-- in as the ghost user and has full access).
administratorAuthorizedKeysEntry:: String -> String -> String
administratorAuthorizedKeysEntry _ key = key

-- | Read keys from ~/administrator/keys and ~/user/ and generate
-- the ~/.ssh/authorized_keys file, pairing SSH keys and usernames.
refreshAuthorizedKeys :: IO ()
refreshAuthorizedKeys = do
  home <- getHomeDirectory
  let authorizedKeys = home </> ".ssh" </> "authorized_keys"
      administratorKeys = home </> "administrator" </> "keys"
  b <- doesFileExist $ administratorKeys
  when b $ do
    copyFile administratorKeys authorizedKeys
    -- TODO add user keys
