# Advent Of Code 2017

Doing the Advent of Code 2017 challenges in Haskell.

## Cabal Structure

We expose all the code as libraries, and then we specify in the `.cabal` file
how to build the executables/test suites using them. I settled on this
approach as:

- it works well with IDE integration (in particular, I have found that
  [HLS][hls] doesn't like being used in executable or test code, so we dodge
  that problem by exposing everything as library code).
- it has the added benefit that you can load up every bit of code (including
  test suites) in a REPL to play around with, which can be nice during
  development.
- using `mixins` in the cabal file we can reduce the boilerplate that might
  otherwise be needed to structure things this way.

## Usage

### Cabal

```shell
> cabal build
> cabal run day-01
> cabal run test-01
```

```shell
> cabal repl adventofcode2017
...
>>> :load Day01
```

### Stack

```shell
> stack build
> stack run day-01
> stack test :test-01
```

```shell
> stack repl adventofcode2017:lib
...
>>> :load Day01
```

### Nix

We provide a shell that has all tools etc. configured. Inside that, stack or
cabal can be used like normal. Also, opening an editor from that shell with
spawn it with $PATH etc. configured to provide matching versions of HLS
specified in `shell.nix`.

```shell
> nix-shell  
...
> [nix-shell]$ stack build        
> [nix-shell]$ cabal build        
> [nix-shell]$ preferred-editor . 
```

[hls]: https://github.com/haskell/haskell-language-server
