# Logbook

## Date Entries

### May 14

- Create GitHub Repository
- Setup of the basic Stack project
- PoC: Parse File using GHC's API
- PoC: Parse File using `haskell-src-ext`, and retrieve imports and declarations
- PoC: Understand typing provided by `haskell-src-ext`.

### May 15

- PoC: Typecheck/Compile File using GHC's API

### May 16

- PoC: Pretty printing code using `haskell-src-ext`
- Improve PoC: Guess module names using the source's file path

### May 18

- Setup Continuous Integration using GitHub Actions

### May 20

- PoC: Retrieve Module AST using GHC's API

### May 21

- PoC: Diagnostic Compilation Error using GHC's API

### May 26

- PoC: Try to typecheck with GHC's API using `haskell-src-ext` data (inconclusive)
- Start to list possible changes to apply, using a list provided from the OCaml implementation
- Improve PoC: Parse module declarations and associated value's type

### May 27

- Improve PoC: On compilation error, guess if type or value is in scope

### May 28

- Unit Testing of Compiler's functions
- Add more possible changes to consider

### May 30

- Add more possible changes to consider
- Implement Seminal's entrypoint
- Define the sub-modules of the library

### May 31

- Start implementing enumerator structure
- Implement first change: singleton to item
  - Note: For declaration list, tried to use [this](https://dl.acm.org/doi/pdf/10.1145/3310232.3310243) method. But standalone type signature without accompanying declaration are not allowed
- Setup flow of: evaluates changes and go through followups changes

### June 1

- Bypass missing `main` function error
- Successful change suggestion (no ranking nor formatting)

### June 2

- Pretty printing suggested changes
- Implement Functional tests suite

### June 3

- Setup CI for Coverage report
- Split Enumerator into submodules: one for each type of node

### June 4

- Ranker: Ranks suggestion by order of discovery + position in code
- Printing Change: Now a single-line

### June 5

- First meeting w/ master supervisor
- Setup CI for build on multiple versions of the compiler
- Use GitHub Issues to track changes to consider
- Setup CLI Options Parser

### June 6

- Use options to select the number of changes to display
- Use options to do a lazy or eager search
- Re-organise the file architecture to be a standard Haskell Module/Library
- Use option to hide typechecker error message
- Enumerator goes through 'where' clause
- Enumerator goes through 'let' clause

### June 7

- Enumerator goes through 'If' conditions
- Enumeration for Expressions uses multiple types of wildcards: `undefined`, `[]`, `True`
- Enumeration goes through expressions in parenthesis

### June 8

- Enumerator goes through `case of` expressions
- Enumerating on Patterns: Instead of removing the pattern, replaces it with a wildcard

### June 9

- Create custom Change infix `<&&>` to allow fmap-ing on lists of arrays

### June 10

- Setup Linter
- Use `.hs-boot` files to allow circular imports and better code splitting

### June 11

- Implement Change Groups

### June 12

- ChangeType: Deriving from Data, to allow better help for CLI arguments

### June 15

- Allow Seminal to load multiple files

### June 17

- Enumeration goes through operations
- Attempt to suggest `cons` as change

### June 18

- Remove the Silencer, and use DynFlags to hide errors during the GHC Session
- Enumeration in Function Application: Try to remove parameter
- Enumeration in Function Application: Try to swap parameter
- Enumeration in Function Application: Try to add parameter

### June 19

- Refactorisation of the `Change` type. `execs` is always a list

### June 21

- Enumeration: Try replacing with `()`

## Difficulties encountered

- Removing declaration might lead to standalone type signature, which is not allowed in Haskell. The solution was to, along with removing the said declaration, set its value to undefined
- For overloaded functions, like `length`, replacing the parameters with `undefined` leads to type ambiguity. The solution was to use multiple kinds of wildcards
- In the case of multiple files loading: The runner only goes through the files that do not typecheck. This is not always what we want. (cf. `test/assets/invalid/modules`)
- Using built-in infixes (such as `(:)`) can be done in a straight-forward way.
