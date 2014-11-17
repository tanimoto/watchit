# watchit

Watch a directory and run a command whenever a file changes.


## Examples

Without any arguments, `watchit` will watch all files in the current directory, recursively, and print which files have changed:

```
$ watchit
watchit started...
# Upon saving this README file
watchit/README.markdown
```

A more useful example is to watch a source code directory, and recompile whenever a file matching an extension changes:

```
$ watchit --path src --extension hs "cabal build"
watchit started...
------------------------------------------------------------------------
Building watchit-0.1.0.0...
Preprocessing library watchit-0.1.0.0...
In-place registering watchit-0.1.0.0...
Preprocessing executable 'watchit' for watchit-0.1.0.0...
[4 of 4] Compiling Main)
Linking dist/build/watchit/watchit ...
Preprocessing test suite 'Tests' for watchit-0.1.0.0...
ExitSuccess
```


## Installation

```
$ cabal sandbox init
$ cabal install --dependencies-only
$ cabal build
```


## Usage

```
$ watchit --help
watchit

Usage: watchit [--path PATH] [--extension EXTENSION] [-f|--force]
               [-j|--num-jobs JOBS] [--not-recursive] [COMMAND]
  Watch a directory and run a command whenever a file changes

Available options:
  -h,--help                Show this help text
  --path PATH              Directory to watch
  --extension EXTENSION    File extension to watch
  -f,--force               Force command to run right away
  -j,--num-jobs JOBS       Number of concurrent jobs
  --not-recursive          Do not watch directory recursively
  COMMAND                  Command to run
```


## Related Projects

- [Guard](https://github.com/guard/guard)
- [entr](http://entrproject.org/)
- [incron](http://inotify.aiken.cz/?section=incron&page=about&lang=en)


## Authors

- Paulo Tanimoto <ptanimoto@gmail.com>
