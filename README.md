# Seminal for Haskell

[![Build](https://github.com/Arthi-chaud/seminal-haskell/actions/workflows/build.yml/badge.svg)](https://github.com/Arthi-chaud/seminal-haskell/actions/workflows/build.yml)
[![Coverage](https://codecov.io/gh/Arthi-chaud/seminal-haskell/branch/main/graph/badge.svg?token=FHHJ86PCU9)](https://codecov.io/gh/Arthi-chaud/seminal-haskell)
[![Documentation](https://img.shields.io/badge/Documentation-Haddock-purple)](https://arthi-chaud.github.io/seminal-haskell/)

Seminal is a research project published in [2006](https://dl.acm.org/doi/pdf/10.1145/1159876.1159887) and [2007](https://dl.acm.org/doi/pdf/10.1145/1273442.1250783).

Compilers for programming languages that allow type inference and polymorphism are known for having cryptic (misleading, very verbose) type-related-error messages. These outputs from the compiler can be difficult to comprehend for the developers, especially if they are beginners.

The goal of Seminal is to overcome this weakness. To do so, Delta-debugging is leveraged: Seminal will go through the AST of the source code, and attempt some tweaks, and will try to type-check the new AST. The successful changes will be ranked and presented to the developer as suggestions to fix the type error.

This implementation of Seminal for Haskell is a Master's Degree Project for the [University of Kent](https://www.kent.ac.uk/).

---

```
Usage: seminal-haskell files... [-n|--lines N] [--lazy] [-q|--quiet] [-l|--minLevel LEVEL]

Available options:
  files...                 The paths to the Haskell source files
  -n,--lines N             Output the best N suggestions
  --lazy                   Stops searching at the first *good* change
  -q,--quiet               Hide the original type-check error
  -l,--minLevel LEVEL      The minimal level of suggestions to display.
                           Possible values: Wildcard, Removal, Wrapping, Terminal
                           (default: Wrapping)
  -h,--help                Show this help text
```
