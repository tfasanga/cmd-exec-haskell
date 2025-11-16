# cmdexec - Command Execution Library for Haskell

## Overview

**cmdexec** is a Haskell library that provides a unified interface for executing shell commands on both local and remote machines. It abstracts the complexity of command execution, SSH connections, and file transfers, offering a type-safe and composable API for system administration and automation tasks.

## Features

- **Unified Command Execution**: Execute commands on local and remote machines using the same interface
- **SSH Support**: Connect to remote machines with flexible authentication options
- **File Transfer**: Built-in support for SCP and Rsync operations
- **Type Safety**: Leverages Haskell's type system for safe command construction
- **Error Handling**: Proper exit code handling and error propagation
- **Flexible Configuration**: Support for custom SSH ports and private key authentication

## Architecture

The library is built around a core abstraction layer with the following design principles:

### Core Abstractions

- **CommandExecutor Typeclass**: Defines a common interface for command execution
- **Machine Type**: Represents either a local or remote machine
- **Credential Management**: Type-safe SSH credentials with optional port and key file

### Key Components

1. **Executor Module**: Core typeclass defining the command execution interface
2. **Machine Module**: Unified representation of local and remote machines
3. **Local.Executor**: Implementation for local command execution
4. **Remote.Executor**: Implementation for remote SSH-based execution
5. **Ssh Module**: SSH command construction and credential management
6. **Scp Module**: Secure copy operations between machines
7. **Rsync Module**: Advanced file synchronization with options

## Module Documentation

### Executor

Defines the `CommandExecutor` typeclass with two primary operations:

```haskell
class CommandExecutor a where
  executeCmdIO :: a -> Command -> IO (ExitCode, String)  -- Execute and capture output
  runCmdIO :: a -> Command -> IO ExitCode                -- Execute without capturing output
```

### Machine

Provides a unified representation of execution targets:

```haskell
data Machine = LocalMachine | RemoteMachine SshCredentials
```

Automatically routes commands to the appropriate executor based on machine type.

### Ssh

Handles SSH connection details and command construction:

```haskell
data SshCredentials = SshCredentials
  { username :: Username
  , hostname :: Hostname
  , port :: Maybe PortNum
  , privateKeyFile :: Maybe FilePath
  }
```

Features:
- Automatic SSH command string generation
- Quote escaping for safe command execution
- Optional port specification
- Private key authentication support

### Scp

Secure file copy operations between local and remote machines:

```haskell
data ScpFilePath = ScpFp Machine FilePath

scp :: ScpFilePath -> ScpFilePath -> IO ExitCode
```

Features:
- Intelligent path resolution for local and remote files
- Automatic SCP URI generation for remote paths
- Optimization: skips operation when both source and destination are local

### Rsync

Advanced file synchronization with extensive options:

```haskell
data RsyncSource = RsyncSrc RootDir RelativeDir
data RsyncDestination = RsyncDst Machine FilePath
data RsyncOption = Exclude String

rsync :: RsyncSource -> RsyncDestination -> [RsyncOption] -> IO ExitCode
```

Features:
- Relative path handling for selective synchronization
- Archive mode with deletion support
- Exclude patterns for filtering files
- Automatic SSH configuration propagation
- Default flags: `--archive --relative --delete --verbose`

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler) 8.0 or later
- Stack build tool

### Building from Source

```bash
# Clone the repository
git clone https://github.com/tfasanga/cmd-exec-haskell.git
cd cmd-exec-haskell

# Build the project
stack build

# Run tests
stack test
```

### Adding as a Dependency

Add to your `package.yaml`:

```yaml
dependencies:
  - cmdexec
```

Or to your `.cabal` file:

```cabal
build-depends:
    base >=4.7 && <5
  , cmdexec
```

## Usage Examples

### Local Command Execution

```haskell
import Executor
import Machine
import Local.Executor

main :: IO ()
main = do
  -- Execute command and capture output
  (exitCode, output) <- executeCmdIO LocalMachine "ls -la"
  putStrLn output

  -- Execute command without capturing output
  exitCode <- runCmdIO LocalMachine "echo 'Hello, World!'"
  return ()
```

### Remote Command Execution

```haskell
import Executor
import Machine
import Ssh

main :: IO ()
main = do
  let credentials = SshCredentials
        { username = "user"
        , hostname = "example.com"
        , port = Just 2222
        , privateKeyFile = Just "/home/user/.ssh/id_rsa"
        }

  let remoteMachine = RemoteMachine credentials

  -- Execute remote command
  (exitCode, output) <- executeCmdIO remoteMachine "uname -a"
  putStrLn output
```

### File Transfer with SCP

```haskell
import Machine
import Scp
import Ssh

main :: IO ()
main = do
  let credentials = SshCredentials
        { username = "deploy"
        , hostname = "server.example.com"
        , port = Nothing
        , privateKeyFile = Just "/home/user/.ssh/deploy_key"
        }

  let source = scpFp LocalMachine "/local/path/file.txt"
  let destination = scpFp (RemoteMachine credentials) "/remote/path/file.txt"

  exitCode <- scp source destination
  return ()
```

### File Synchronization with Rsync

```haskell
import Machine
import Rsync
import Ssh

main :: IO ()
main = do
  let credentials = SshCredentials
        { username = "backup"
        , hostname = "backup.example.com"
        , port = Nothing
        , privateKeyFile = Nothing
        }

  let source = RsyncSrc "/local/workspace" "project/"
  let destination = RsyncDst (RemoteMachine credentials) "/backup/project/"
  let options = [Exclude "*.log", Exclude "node_modules"]

  exitCode <- rsync source destination options
  return ()
```

## Testing

The project includes a comprehensive test suite using the Tasty testing framework:

```bash
# Run all tests
stack test

# Run tests with verbose output
stack test --test-arguments="--verbose"
```

### Test Coverage

- **SshTest**: Tests SSH command construction and quote escaping
- **RsyncTest**: Tests rsync command building and options handling

## Project Structure

```
cmd-exec-haskell/
├── src/
│   ├── Core/
│   │   ├── Common.hs       # Common utilities
│   │   └── Err.hs          # Error handling
│   ├── Local/
│   │   └── Executor.hs     # Local command execution
│   ├── Remote/
│   │   └── Executor.hs     # Remote command execution
│   ├── Executor.hs         # Core executor interface
│   ├── Machine.hs          # Machine abstraction
│   ├── Ssh.hs              # SSH functionality
│   ├── Scp.hs              # SCP file transfer
│   └── Rsync.hs            # Rsync synchronization
├── test/
│   └── unit/
│       ├── Main.hs
│       ├── SshTest.hs
│       └── RsyncTest.hs
├── package.yaml            # Hpack configuration
├── stack.yaml              # Stack configuration
└── cmdexec.cabal           # Cabal package file
```

## Dependencies

### Core Dependencies

- `base` (>= 4.7 && < 5)
- `process` - For process execution
- `directory` - For file system operations
- `filepath` - For file path manipulation
- `split` - For string splitting utilities

### Test Dependencies

- `tasty` - Test framework
- `tasty-hunit` - HUnit integration
- `tasty-quickcheck` - QuickCheck integration
- `tasty-smallcheck` - SmallCheck integration
- `tasty-golden` - Golden tests
- `tasty-program` - Program testing

## Compiler Options

The project uses strict GHC warning flags for code quality:

- `-Wall` - Enable all warnings
- `-Wcompat` - Warn about compatibility issues
- `-Wincomplete-record-updates` - Warn about incomplete record updates
- `-Wincomplete-uni-patterns` - Warn about incomplete pattern matches
- `-Wmissing-export-lists` - Warn about missing export lists
- `-Wpartial-fields` - Warn about partial record fields
- `-Wredundant-constraints` - Warn about redundant constraints

## Language Extensions

The project uses the following GHC extensions:

- `DisambiguateRecordFields` - Allow field disambiguation
- `DuplicateRecordFields` - Allow duplicate record field names
- `RecordWildCards` - Enable record wildcards
- `GeneralizedNewtypeDeriving` - Derive instances for newtypes
- `NoFieldSelectors` - Disable automatic field selector generation

## License

This project is licensed under the **BSD-3-Clause License**.

Copyright (c) 2024 Tibor Fasanga

## Author

**Tibor Fasanga**
Email: tibor@fasanga.com
GitHub: [tfasanga](https://github.com/tfasanga)

## Repository

- Homepage: [https://github.com/tfasanga/cmd-exec-haskell](https://github.com/tfasanga/cmd-exec-haskell)
- Issue Tracker: [https://github.com/tfasanga/cmd-exec-haskell/issues](https://github.com/tfasanga/cmd-exec-haskell/issues)

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Future Enhancements

- Support for custom SSH ports in rsync operations
- Additional rsync options (bandwidth limiting, compression, etc.)
- Support for other file transfer protocols
- Asynchronous command execution
- Command timeout handling
- Better error types and error recovery
