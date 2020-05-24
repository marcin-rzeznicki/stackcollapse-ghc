# stackcollapse-ghc

[![Build status](https://img.shields.io/travis/marcin-rzeznicki/stackcollapse-ghc.svg?logo=travis)](https://travis-ci.org/marcin-rzeznicki/stackcollapse-ghc)
[![Hackage](https://img.shields.io/hackage/v/stackcollapse-ghc.svg?logo=haskell)](https://hackage.haskell.org/package/stackcollapse-ghc)
[![Stackage Lts](http://stackage.org/package/stackcollapse-ghc/badge/lts)](http://stackage.org/lts/package/stackcollapse-ghc)
[![Stackage Nightly](http://stackage.org/package/stackcollapse-ghc/badge/nightly)](http://stackage.org/nightly/package/stackcollapse-ghc)
[![GPL-3.0-only license](https://img.shields.io/badge/license-GPL--3.0--only-blue.svg)](LICENSE)

Program to fold GHC prof files into flamegraph input

## Motivation

The reasons why this package exists despite other packages with similar functionality (not including the NIH syndrome) boil down to:
* it does only one thing (stack collapsing), so it's up to the user to install flamegraph scripts, pass options etc (in my eyes it's not a limitation, on the contrary),
* output control: annotations (color profiles), extending traces from a configured set of modules with the source locations or toggling qualified names,
* precise ticks and/or bytes with `-p` reports,
* it's fast

Table of Contents
-----------------
  * [Basic usage](#basic-usage)
    * [Ticks](#visualize-ticks)
    * [Allocations](#visualize-allocations)
  * [Cookbook](#cookbook)
    * [Colors](#using-distinct-colors-for-modules)
    * [Qualified names](#turning-off-qualified-names)
    * [Source locations](#adding-source-locations)
  * [All options](#all-options)
  * [Installation](#installation)

## Basic usage

(all the following examples assume you have installed `flamegraph` on your path)

### Visualize ticks

If you have a detailed prof file (`-P` RTS option)

```bash
stackcollapse-ghc prof_file | flamegraph.pl --title 'Example' --subtitle 'Time' --countname ticks > path_to_svg
```

If you have a standard prof file (`-p` RTS option)

```bash
stackcollapse-ghc -p prof_file | flamegraph.pl --title 'Example' --subtitle 'Time' --countname ticks > path_to_svg
```

![Ticks](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/basicTime.svg)

### Visualize allocations

If you have a detailed prof file (`-P` RTS option)

```bash
stackcollapse-ghc --alloc prof_file | flamegraph.pl --title 'Example' --subtitle 'Bytes allocated' --countname bytes > path_to_svg
```

If you have a standard prof file (`-p` RTS option)

```bash
stackcollapse-ghc --alloc -p prof_file | flamegraph.pl --title 'Example' --subtitle 'Bytes allocated' --countname bytes > path_to_svg
```

![Bytes](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/basicAlloc.svg)

## Cookbook

### Using distinct colors for modules

Works out of the box. You just need to tell the program which modules are "yours" (by default it only assumes `Main`) and pass the desired color scheme to the flamegraph script

```bash
stackcollapse-ghc -u Example prof_file | flamegraph.pl --title 'Example with colors' --subtitle 'Time' --countname ticks --color java > path_to_svg
```

(the traces coming from your modules are green under this palette)

![JavaColor](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/javaColor.svg)

Or you can use the `mem` pallette to track allocations

```bash
stackcollapse-ghc -u Example --alloc -p prof_file | flamegraph.pl --title 'Example with colors' --subtitle 'Bytes allocated' --countname bytes --color mem > path_to_svg
```

![MemColor](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/memColor.svg)

This is achieved by using a thing called _annotations_. `stackcollapse-ghc` adds the annotation `_[j]` to traces from modules in `-u`, `_[i]` to _CAFs_ and `_[k]` to the "kernel" modules, such as `GHC` or `System`

### Turning off qualified names

You might want to tone down the visual clutter in your flamegraph by turning off the qualified names for some modules. You can use the `-u` option for this just as in the color pallete examples. Traces from the modules that you have passed in `-u` will have unqualified names by default

```bash
stackcollapse-ghc -u Example -p prof_file | flamegraph.pl --title 'Example' --subtitle 'Time' --countname ticks > path_to_svg
```

![UserModules](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/userModules.svg)

If you want to use visual indications *and* retain qualified names you can use the following option

```bash
stackcollapse-ghc -Q -u Example -p prof_file | flamegraph.pl --title 'Example' --subtitle 'Time' --countname ticks --color java > path_to_svg
```

### Adding source locations

If you want to see the source of the traces on the generated flamegraph, you can do it selectively by combining `-u` options to mark the modules for which you want to include locations, with `-s`

```bash
stackcollapse-ghc -s -u Example prof_file | flamegraph.pl --title 'Example with sources' --subtitle 'Time' --countname ticks > path_to_svg
```

![UserSources](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/userSrc.svg)

Alternatively, you can simply include it for all traces (with or without `-u` depending on whether you need it for something else, such as colors or qualified names)

```bash
stackcollapse-ghc -S prof_file | flamegraph.pl --title 'Example with sources' --subtitle 'Time' --countname ticks > path_to_svg
```

![AllSources](https://github.com/marcin-rzeznicki/stackcollapse-ghc/tree/master/examples/allSrc.svg)

## All options

```
Usage: stackcollapse-ghc [OPTIONS] FILE
OPTIONS:
  -P                                            Process detailed prof file ('-P' or '-pa' options) (default)
  -p                                            Process standard prof file ('-p' option)
  -u module_name  --user-module=module_name     Name of an user module (may be repeated to add more than one). Matches the whole "hierarchy", so `MyModule` matches `MyModule` as well as `MyModule.Internal`
  -t              --time                        Collapse with respect to time (default)
  -a              --alloc                       Collapse with respect to allocations
  -S                                            Append source location to every function name
  -s                                            Append source location to functions defined in user modules
                  --no-source                   Do not append source location (default)
  -Q                                            Always use qualified functon names
                  --no-qualified                Do not use qualified function names
  -q                                            Use qualified names for functions not defined in user modules (default)
  -A              --annotations, --annot        Add annotations to output (j - user modules, k - GHC and System, i - CAFs)
                  --no-annotations, --no-annot  Do not add annotations to output

```

## Installation

You can install it from [Hackage](https://hackage.haskell.org/) or [Stackage](https://www.stackage.org/) by using either `cabal`:

```
cabal v2-update
cabal v2-install stackcollapse-ghc
```

or `stack`

```
stack install stackcollapse-ghc
```
