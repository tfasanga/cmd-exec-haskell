module Rsync
  ( RsyncSource (..),
    RsyncDestination (..),
    RsyncOption (..),
    rsync,
    buildRsyncCmd,
  )
where

import Core.Common
import Data.List (intercalate)
import Executor
import Local.Executor
import Machine
import Ssh
import System.FilePath ((</>))

type RootDir = FilePath

type RelativeDir = FilePath

data RsyncSource = RsyncSrc RootDir RelativeDir
  deriving (Eq)

instance Show RsyncSource where
  show (RsyncSrc rootDir relativeDir) = rootDir </> "." </> relativeDir

data RsyncDestination = RsyncDst Machine FilePath
  deriving (Eq)

instance Show RsyncDestination where
  show (RsyncDst LocalMachine fp) = fp
  show (RsyncDst (RemoteMachine (SshCredentials username hostname _ _)) fp) = username <> "@" <> hostname <> ":" <> fp

data RsyncOption = Exclude String
  deriving (Eq)

instance Show RsyncOption where
  show (Exclude s) = "--exclude " <> s

-- TODO Custom SSH port:
-- scp -e 'ssh -p 2222'
-- scp -P 2222

rsync :: RsyncSource -> RsyncDestination -> [RsyncOption] -> IO ExitCode
rsync src dst opts = case buildRsyncCmd src dst opts of
  (Just cmd) -> runLocalShellCmdIO cmd
  Nothing -> skipRsyncIO

buildRsyncCmd :: RsyncSource -> RsyncDestination -> [RsyncOption] -> Maybe String
buildRsyncCmd _ (RsyncDst LocalMachine _) _ = Nothing
buildRsyncCmd src dst opts = Just ("rsync" <> showDstOpts dst <> showDefaultOpts <> showOpts opts <> " " <> show src <> " " <> show dst)

showDstOpts :: RsyncDestination -> String
showDstOpts (RsyncDst (RemoteMachine creds) _) = showPortOpt creds <> showKeyOpt creds
showDstOpts _ = ""

showPortOpt :: SshCredentials -> String
showPortOpt (SshCredentials _ _ (Just port) _) = " -P " <> show port
showPortOpt _ = ""

showKeyOpt :: SshCredentials -> String
showKeyOpt (SshCredentials _ _ _ (Just keyFile)) = " -i " <> keyFile
showKeyOpt _ = ""

showOpts :: [RsyncOption] -> String
showOpts [] = ""
showOpts opts = " " <> join list
  where
    join = unwords
    list = map show opts

showDefaultOpts :: String
showDefaultOpts = " --archive --relative --delete --verbose -o"

skipRsyncIO :: IO ExitCode
skipRsyncIO = do
  println "skipping rsync, both source and destination machines are local"
  return ExitSuccess
