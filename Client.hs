module Client where

import Set

main :: IO ()
main = print useSet

useSet :: Int
useSet =
    let set0 = emptySet
        set1 = insert set0 1
        set2 = insert set1 1
        set3 = insert set2 2
    in 17 + size set3 ()
