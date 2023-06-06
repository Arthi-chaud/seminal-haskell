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

## Difficulties encountered

- Removing declaration might lead to standalone type signature, which is not allowed in Haskell. The solution was to, along with removing the said declaration, set its value to undefined
- For overloaded functions, like `length`, replacing the parameters with `undefined` leads to type ambiguity (TO BE RESOLVED)