# Functional Objects

Experimental Repo: Creating an "object" using closures, datatypes, and functions
in Haskell.

## Motivation

This was an optional video in Part A of Dan Grossman's Programming Languages
course. The reason why this is an important video to me is that it starts to
peek at the intimate relationship between Object-Oriented languages and
Functional Programming.

## Making An Object

In this small repo, we created a set that acts like an object. What does it mean
to "act like an object"? In this case, what we are saying is that we've made a
set on which have **implicit** access to data. Part of the client code (in
`Client.hs`) looks like this:

```
1	module Client where
  
2	import Set
  
3	main :: IO ()
4	main = print useSet
  
5	useSet :: Int
6	useSet =
7	    let set0 = emptySet
8	        set1 = insert set0 1
9	        set2 = insert set1 1
10	        set3 = insert set2 2
11	    in 17 + size set3 ()
```

Conceptually, we can re-arrange `Line 8` to this: `set0.insert(1)`. And that is
starting to look very much like an object.


## Implementation

The implementation code is in the `Set.hs` file. Before getting into the
details, about how this works. The real hero here is `closures` and `lexical
scope`. Remember, lexical scope means that when we **execute** a function, the
environment that the function is executed in is the **same** environment in
which the function was **declared**. This is how we are able to access data that
is not explicitly passed into the function.


To simulate "updates" we call another function, with an updated state, which
then creates closures for us. `Recursion` is another important part of this,
since it is what allows us to "update" the state.

```
1	module Set
2	    ( emptySet
3	    , insert
4	    , size
5	    ) where
  
6	data Set = Set
7	    { member :: Int -> Bool
8	    , insert :: Int -> Set
9	    , size :: () -> Int
10	    }
  
11	emptySet :: Set
12	emptySet =
13	    let makeSet set =
14	            let contains el = el `elem` set
15	            in Set
16	               { member = contains
17	               , size = \() -> length set
18	               , insert =
19	                     \el ->
20	                         if contains el
21	                             then makeSet set
22	                             else makeSet (el : set)
23	               }
24	    in makeSet []
```

On `Lines 1 through 5`, we declare out module. Notice that we don't export the
`Set` type constant or data constructor. Since the client does not have access
to the data constructor, they cannot make values of type `Set`.

On `Lines 6 through 10`, we declare a `Set` type, which happens to be a record.
We made it a record to make it easy for the client to extract the functions from
the set. (Remember, they can't pattern match on the `Set` data constructor).

On `Lines 11 through 24`, we define a value / function `makeSet`. This is where
the rubber meets the road in our implementation. `makeSet` has the type `[Int]
-> Set`. (Note that we didn't have to specialize our set to `[Int]`, but let's
focus on our goal). `makeSet`'s entire purpose is to create a set in which the
fields of that set have access, via closure and lexical scope, to the value that
we called `makeSet` with. It creates an new scope / environment, in which set is
a "global" value.

Finally, let's take a closure look at `Lines 18 through 22`. Notice that we call
`makeSet` from the `insert` function. Because the results of calling `makeSet`
is to create a new "world" where the given value is "global" in that
environment, we have now successfully used `recursion` to simulate updating the
`set` "global" value.

## Recap

To recap, we used `closure`, `lexical scope`, `recursion`.

`Closure` and `lexical scope` allows us to define functions which, **from the
viewpoint of that function** have access to global state.

`Recursion` allows us to simulate updating that global state by passing a new
value to a function which creates a new scope, with that new value as "updated"
global state.

## Misc.

There is a lot that can be done better here, but we want to focus on the main
ideas. For example, we should not have hidden the type constant `Set`, though we
do want to hide the data constructor.

Also, the `size` field of the `Set` record doesn't have to be a function.
