module Set
    ( emptySet
    , insert
    , size
    ) where

data Set = Set
    { member :: Int -> Bool
    , insert :: Int -> Set
    , size :: () -> Int
    }

emptySet :: Set
emptySet =
    let makeSet set =
            let contains el = el `elem` set
            in Set
               { member = contains
               , size = \() -> length set
               , insert =
                     \el ->
                         if contains el
                             then makeSet set
                             else makeSet (el : set)
               }
    in makeSet []
