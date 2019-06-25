Fair(er) Enumerations
=====================

Motivation: enumeration of tuples of fixed width and fixed total size
that is fair w.r.t. each component.
See https://github.com/rudymatela/leancheck/issues/14

Definition: A finite sequence  x  of elements from some set S
is called  d-fair  if for each prefix  p  of  x,
and each  a in  S, we have that

    |p| * abs (|p|_a / |p| - |x|_a / |x|) <=  d

Explanation: we compare the relative frequencies of  a
in  p (the prefix) and in  x (the full sequence),
and their difference should become smaller as  p  gets longer
(that's why we multiply with |p|)

Definition: A finite sequence  x  of elements of  a cartesian product
S = S1 x .. x Sk  is called  d-fair  if each sequence obtained
by projecting to a component is  d-fair.

Examples:

The sequence

    [[0,0,1,1],[0,2,0,0],[1,1,0,0],[0,0,0,2],[1,0,1,0],[0,0,2,0],[0,1,0,1],[1,0,0,1],[2,0,0,0],[0,1,1,0]]

is 4/5-fair.

The sequence

    [[0,0,3],[1,1,1],[0,3,0],[1,0,2],[0,2,1],[2,1,0],[1,2,0],[2,0,1],[0,1,2],[3,0,0]]

is 1-fair.

Let S(w,h) = w-tuples of natural numbers with sum h.

Conjecture: S(w,h) has a 1-fair permutation.

Conjecture: With high probability, a random permutation of S(w,h) is 1-fair.

Modifications:

* project also to subsets of indices (e.g., 4-tuples to 2-tuples)
* do not check prefixes only, but also other subsequences
