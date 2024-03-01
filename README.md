# cmd-exec-haskell

# Shell auto-completion

## MacOS (zsh)
```shell
source <(clitool --zsh-completion-script `which clitool`)
```

## Linux (bash)
```shell
source <(clitool --bash-completion-script `which clitool`)
```

# Libraries

- https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html

# Building

```shell
stack build
```

# Testing

```shell
stack test
# same as
stack build --test

stack build clitool:clitool-unit-test
stack build clitool:clitool-unit-test --no-run-tests
stack build clitool:clitool-spec-test
```

```shell
clitool exec deployer 'cd /opt/build && echo "$(pwd)" && ls -l && test -f null'
```
