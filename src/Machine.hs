module Machine (Machine (..)) where

import Executor
import Local.Executor
import Remote.Executor
import Ssh

data Machine = LocalMachine | RemoteMachine SshCredentials
  deriving (Show, Eq)

instance CommandExecutor Machine where
  executeCmdIO LocalMachine command = executeLocalShellCmdIO command
  executeCmdIO (RemoteMachine creds) command = executeRemoteShellCmdIO (Ssh creds command)

  runCmdIO LocalMachine command = runLocalShellCmdIO command
  runCmdIO (RemoteMachine creds) command = runRemoteShellCmdIO (Ssh creds command)
