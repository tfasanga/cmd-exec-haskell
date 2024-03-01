{- cabal:
build-depends: base, split
-}
import Data.List.Split
main = print $ splitOn "x" "axbxc"
