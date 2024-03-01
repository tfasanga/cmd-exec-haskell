module Executor
  ( CommandExecutor (..),
    Command,
    ExitCode (..),
  )
where

type Command = String

data ExitCode
  = ExitSuccess
  | ExitFailure Int
  deriving (Eq, Ord, Read, Show)

class CommandExecutor a where
  executeCmdIO :: a -> Command -> IO (ExitCode, String)
  runCmdIO :: a -> Command -> IO ExitCode
