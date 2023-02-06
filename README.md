# Tic Tac Toe - Haskell

**TODOs**:
- [x] Find formatters for `.cabal` and `.hs` files
- [x] Add some (prefferably Haskell based) file watcher to execute the formatters
- [x] Read-up on Haskell linting
- [x] Read-up on testing in Haskell
- [ ] Find how to control development dependencies in .cabal 
- [x] Find out how to handle randomness properly in Haskell (IO Monads?)
- [x] Research if I can compose functions instead of using the disgusting assignments (thread first macros or such?)
- [x] Read up on [invertibility of Haskell functions](https://www-ps.informatik.uni-kiel.de/~fte/papers/Haskell-1.pdf)


Notes:
- `stylish-haskell` feels like it's not doing much - not too suitable for learners without a faintest idea on how to style Haskell

## Run

Game can be ran using
```
cabal run
```

- code is fromatted and linted using `ormolu`, `hlint` and `cabal-fmt`
- code is tested using `doctest`

There is config for steel-overseer in form of [`.watcherLint`](./.watcherLint) and [`.watcherTest`](./.watcherTest) which can be run using
```
sos --rcfile .watcherLint
# and
sos --rcfile .watcherTest
```
