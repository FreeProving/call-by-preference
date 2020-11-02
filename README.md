# call-by-preference

<!-- Short description -->
The `call-by-preference` library is an algebraic effect framework.
It provides an effect called `Let` which enables control over evaluation strategies.
By choosing an appropriate interpretation of this effect, programs are evaluated in either *call-by-name*, *call-by-need*, or even *call-by-value* semantics.
Multiple occurrences of the `Let` effect can each be interpreted with different strategies.
For instance, while one part of a program can use call-by-name, another part can use call-by-value.
Programs written in this way are therefore flexible with respect to not only
the interpretation of their effects, but also with respect to the evaluation
strategy.

## Getting Started

### Required Software

To use this library, GHC and Cabal are required.
The library has been tested with the following software versions.

 - [GHC][software/ghc], version 8.6.5
 - [Cabal][software/cabal], version 3.2.0.0

### Installation

The library provided by this package is not yet available on Hackage.
In order to use the library in your own project, add the following stanza to your `cabal.project` file and add `call-by-preference` to your `build-depends`.

```cabal
source-repository-package
  type: git
  location: git://github.com/FreeProving/call-by-preference.git
  subdir: library
  tag: v0.1.0.0
```

### Running with GHCi

If you want to use the library interactively, run the following command in the root directory of the library to open a GHCi prompt.

```bash
cabal new-repl
```

By default the `Control.Prog` module is loaded.
Use the `:m` command to switch to or load another module.

### Testing

To run the test suite run the following command in the root directory of the library.

```bash
cabal new-run call-by-preference-tests
```

The test suite is built using the [Hspec][package/hspec] testing framework.
Command line options can be passed to the framework by adding two dashes after the command above.

```bash
cabal new-run call-by-preference-tests -- [OPTIONS...]
```

By default the package is configured such that compilation fails if GHC reports any warning.
Add the `-Wwarn` option during development to disable this behavior temporarily.
The `--ghc-option`s have to be specified before Hspec options.

```bash
cabal new-run call-by-preference-tests --ghc-option -Wwarn -- [OPTIONS...]
```

## Examples

There are additional examples for the usage of the library in the `./example` directory.
Some examples include Hspec test cases as well.
The tests from the examples can be executed with the following command.

```bash
cabal new-run call-by-preference-examples
```

## Usage

The library provides a `Control.Prog` module that exports the infrastructure for the `Prog` monad, combination of signatures as well as the`Embed`, `Let` and `None` effects.
All other effects are not exported by the `Control.Prog` module and have to be imported individually from the `Control.Prog.Effect.*` modules.

## License

This library is licensed under The 3-Clause BSD License.
See the [LICENSE][call-by-preference/LICENSE] file for details.

[call-by-preference/LICENSE]:
  https://github.com/FreeProving/call-by-preference/blob/main/library/LICENSE
  "call-by-preference — The 3-Clause BSD License"

[package/hspec]:
  https://hspec.github.io/
  "Hspec: A Testing Framework for Haskell"

[software/ghc]:
  https://www.haskell.org/ghc/
  "The Glasgow Haskell Compiler"
[software/cabal]:
  https://www.haskell.org/cabal/
  "Common Architecture for Building Applications and Libraries"
