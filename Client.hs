module Client where

import Set

main :: IO ()
main = print useSet

useSet :: Int
useSet =
    let s0 = emptySet
        s1 = insert s0 1
        s2 = insert s1 1
        s3 = insert s2 2
    in 17 + size s3 ()
