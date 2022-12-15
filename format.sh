cabal-fmt tic-tac-toe.cabal > tic-tac-toe.cabal.formatted && mv tic-tac-toe.cabal.formatted tic-tac-toe.cabal
stylish-haskell -i $(find . -name '*.hs')