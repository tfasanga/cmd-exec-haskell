module Ssh
  ( SshCredentials (..),
    Username,
    Hostname,
    PortNum,
    SshCommand (..),
  )
where

import Executor (Command)

type Username = String

type Hostname = String

type PortNum = Int

data SshCredentials = SshCredentials
  { username :: Username,
    hostname :: Hostname,
    port :: Maybe PortNum,
    privateKeyFile :: Maybe FilePath
  }
  deriving (Show, Eq)

data SshCommand = Ssh SshCredentials Command

instance Show SshCommand where
  show (Ssh creds cmd) = "ssh " <> showSshOptions creds <> " '" ++ escapeQuotes cmd <> "'"

showSshOptions :: SshCredentials -> String
showSshOptions creds = showRemoteOpt creds <> showPortOpt creds <> showKeyOpt creds

showRemoteOpt :: SshCredentials -> String
showRemoteOpt (SshCredentials username hostname _ _) = username <> "@" <> hostname

showPortOpt :: SshCredentials -> String
showPortOpt (SshCredentials _ _ Nothing _) = ""
showPortOpt (SshCredentials _ _ (Just port) _) = " -p " <> show port

showKeyOpt :: SshCredentials -> String
showKeyOpt (SshCredentials _ _ _ Nothing) = ""
showKeyOpt (SshCredentials _ _ _ (Just keyFile)) = " -i " <> keyFile

escapeQuotes :: String -> String
escapeQuotes =
  let escapeOneChar c =
        case c of
          a@'\'' -> ['\\', a]
          _ -> [c]
   in concatMap escapeOneChar

-- Alternative

_escapeQuotes :: String -> String
_escapeQuotes = concatMap escapeChar

escapeChar :: Char -> String
escapeChar ch = case lookup ch escapesAlist of
  Nothing -> [ch]
  Just esc -> esc

escapesAlist :: [(Char, String)]
escapesAlist = map makePair charactersToEscape
  where
    makePair a = (a, ['\\', a])
    charactersToEscape = "'"
