module Scp (scp, ScpFilePath, scpFp) where

import Core.Common
import Executor
import Local.Executor
import Machine
import Ssh

data ScpFilePath = ScpFp Machine FilePath

instance Show ScpFilePath where
  show = showScpFp

-- | Constructor
scpFp :: Machine -> FilePath -> ScpFilePath
scpFp = ScpFp

-- | scp command
scp :: ScpFilePath -> ScpFilePath -> IO ExitCode
scp src dst = case buildScpCmd src dst of
  (Just cmd) -> runLocalShellCmdIO cmd
  Nothing -> skipScpIO

skipScpIO :: IO ExitCode
skipScpIO = do
  println "skipping scp, both source and destination machines are local"
  return ExitSuccess

buildScpCmd :: ScpFilePath -> ScpFilePath -> Maybe String
buildScpCmd (ScpFp LocalMachine _) (ScpFp LocalMachine _) = Nothing
buildScpCmd src dst = Just ("scp " <> show src <> " " <> show dst)

showScpFp :: ScpFilePath -> String
showScpFp (ScpFp LocalMachine fp) = fp
showScpFp (ScpFp (RemoteMachine creds) fp) = sshCredentialsToScpUri creds <> fp

sshCredentialsToScpUri :: SshCredentials -> String
sshCredentialsToScpUri (SshCredentials username hostname port _) = "scp://" <> username <> "@" <> hostname <> showPortOpt port

showPortOpt :: Maybe PortNum -> String
showPortOpt Nothing = ""
showPortOpt (Just port) = ":" <> show port
