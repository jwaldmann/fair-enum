Fair(er) Enumerations
=====================

# Motivation:

enumeration of tuples of fixed width and fixed total size
that is fair w.r.t. each component.
See https://github.com/rudymatela/leancheck/issues/14

# Definition:

A finite sequence  x  of elements from some set S
is called  d-fair  if for each prefix  p  of  x,
and each  a in  S, we have that

    abs (|p|_a / |p| - |x|_a / |x|) <=  d / |p|

Explanation: we compare the relative frequencies of  a
in  p (the prefix) and in  x (the full sequence),
and their difference should become smaller as  p  gets longer
(that's why we divide by |p|)

Alternative explanation: if we multiply by `|p|`, we get

    abs (|p|_a - |p| * |x|_a / |x|) <= d
    
Here, `|p| * |x|_a / |x|` is the number of `a`
that a prefix of length `|p|` should have,
and the actual value `|p|_a` should differ by at most `d`.
So in the best case, we expect `d=1` (roughly).

Definition: A finite sequence  x  of elements of  a cartesian product
S = S1 x .. x Sk  is called  d-fair  if each sequence obtained
by projecting to a component is  d-fair.


# Examples:


3/4-fair:

    [[1,1,1],[2,2,2],[3,3,3],[4,4,4],[1,1,2],[2,2,1],[3,3,4],[4,4,3]
    ,[1,1,3],[2,2,4],[3,3,1],[4,4,2],[1,1,4],[2,2,3],[3,3,2],[4,4,1]
    ,[1,2,1],[2,1,2],[3,4,3],[4,3,4],[1,2,2],[2,1,1],[3,4,4],[4,3,3]
    ,[1,2,3],[2,1,4],[3,4,1],[4,3,2],[1,2,4],[2,1,3],[3,4,2],[4,3,1]
    ,[1,3,1],[2,4,2],[3,1,3],[4,2,4],[1,3,2],[2,4,1],[3,1,4],[4,2,3]
    ,[1,3,3],[2,4,4],[3,1,1],[4,2,2],[1,3,4],[2,4,3],[3,1,2],[4,2,1]
    ,[1,4,1],[2,3,2],[3,2,3],[4,1,4],[1,4,2],[2,3,1],[3,2,4],[4,1,3]
    ,[1,4,3],[2,3,4],[3,2,1],[4,1,2],[1,4,4],[2,3,3],[3,2,2],[4,1,1]]

3/4-fair:

    [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1],[1,2,4,3],[2,1,3,4]
    ,[3,4,2,1],[4,3,1,2],[1,3,2,4],[2,4,1,3],[3,1,4,2],[4,2,3,1]
    ,[1,3,4,2],[2,4,3,1],[3,1,2,4],[4,2,1,3],[1,4,2,3],[2,3,1,4]
    ,[3,2,4,1],[4,1,3,2],[1,4,3,2],[2,3,4,1],[3,2,1,4],[4,1,2,3]]

13/15-fair:

    [[0,1,3],[1,2,1],[4,0,0],[2,0,2],[0,3,1],[2,2,0],[3,1,0],[1,1,2]
    ,[0,0,4],[1,3,0],[3,0,1],[0,2,2],[0,4,0],[1,0,3],[2,1,1]]



# Properties

For each sequence  s, there should be some  d
such that with high probability,
a random permutation of  s  is  d-fair.

Is  d  always equal to  1?

(Except perhaps maybe for such  s  that have too few
elements)

By the pigeonhole principle,
there are concrete permutations that are d-fair
(for the same d).

We can instantiate this for several families of
sequences.


* w-tuples of natural numbers [1 .. w],
* permutations of [1..w]
* w-tuples of natural numbers with sum h


# Challenge

find a deterministic online algorithm that prints a 1-fair permutation of
the respective sequence, i.e., with O(1) work for each element.


# Possible modifications:

* different scaling function (instead of `/ |p|`)
* project also to subsets of indices (e.g., 4-tuples to 2-tuples)
* do not check prefixes only, but also other subsequences

# Implementation

(Haskell source file in this project) 
Branch-and-bound tree search to find the fairest permutation of a sequence.
Arguments:

```
./FE tuples 3 4   -- achieves 3/4-fairness
./FE perms 4      -- alsoe 3/4-fair
./FE tier 3 4     -- 13/15-fair
```

Output see "Examples2 above.

It starts with the lexicographically increasing permutation (I think?)
and then searches for another one that is fairer (i.e., fulfills the
above requirement for smaller  d), until no more improvement is possible.

# Results

The lexicographically smallest less-than-1-fair arrangement of all permutations of [1..6] starts
```
[[1,2,3,4,5,6],[2,1,4,3,6,5],[3,4,5,6,1,2],[4,3,6,5,2,1],[5,6,1,2,3,4],[6,5,2,1,4,3]
,[1,2,3,4,6,5],[2,1,4,3,5,6],[3,4,5,6,2,1],[4,3,6,5,1,2],[5,6,1,2,4,3],[6,5,2,1,3,4]
,[1,2,3,5,4,6],[2,1,4,6,3,5],[3,4,5,1,6,2],[4,3,6,2,5,1],[5,6,1,3,2,4],[6,5,2,4,1,3]
,[1,2,3,5,6,4],[2,1,4,6,5,3],[3,4,5,1,2,6],[4,3,6,2,1,5],[5,6,1,3,4,2],[6,5,2,4,3,1]
,[1,2,3,6,4,5],[2,1,4,5,3,6],[3,4,5,2,6,1],[4,3,6,1,5,2],[5,6,1,4,2,3],[6,5,2,3,1,4]
,[1,2,3,6,5,4],[2,1,4,5,6,3],[3,4,5,2,1,6],[4,3,6,1,2,5],[5,6,1,4,3,2],[6,5,2,3,4,1]
,[1,2,4,3,5,6],[2,1,3,4,6,5],[3,4,6,5,1,2],[4,3,5,6,2,1],[5,6,2,1,3,4],[6,5,1,2,4,3]
,...
]
```

There is a less-than-1-fair arrangement of all permutations of [1..8] -

but possibly not for permutaions for 5, 7, .. elements.


