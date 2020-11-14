---
title: A proof about the identity relation
---
While working through the exercises of «[How to Prove it](http://www.cambridge.org/us/academic/subjects/mathematics/logic-categories-and-sets/how-prove-it-structured-approach-2nd-edition)» by Daniel Velleman,
I stumbled upon the &para;&nbsp;5.1&nbsp;Functions and the exercise 5.1.8. which has more than one proof,
so I thought I'd post the one I imagined here to showcase my recently acquired proof writing skills.

> 8\. Suppose A is a set. Show that i<sub>A</sub> is the only relation on A that is both an
equivalence relation on A and also a function from A to A.

    eq(r) = reflexive(r) and symmetric(r) and transistive(r)
    eqendof(r) = eq(r) and r : A -> A

    Sidenote:
        eq   = equivalence relation
        endo = endomorphic function — from A to A
        f    = function
    thus
        eq-endo-f

    a. exists r: eqendof(r) and forall z: eqendof(z) -> z = r
    or equivalently
    b. exists r: eqendof(r) and forall z: z ≠ r -> not eqendof(z)

*a\.* As per usual, we first prove existence, which here is trivial. Uniqueness is a tiny bit trickier and
we can prove it by assuming f has the same property and equaling f to i<sub>A</sub>.

The chapter quickly presents Theorem 5.1.4., which claims ∀a ∈ A(f(a) = g(a)), then f = g. So if f is the function
from A to A, then by letting a ∈ A, f ⊆ i<sub>A</sub> and i<sub>A</sub> ⊆ f is obvious.

*b\.* There's another way, however:

Suppose z ≠ i<sub>A</sub>, then it should be that it's not an equivalence relation.

If that's so there must be an element inside one of them that's not in the other.

1. If i<sub>A</sub> has such element, then z is not reflexive, so it's not an equivalence relation, so *eqendof(z) is false*.
2. If z has such element, then there's a p in z that's a pair of different elements: &exist;&nbsp;p&nbsp;in&nbsp;z: p = (x, y) and x ≠ y.
    1. Then x is in A and x = z(x) because it's an equivalence relation, but also y = z(x) by 2.
    2. z(x) != z(x) so it's not a function and therefore doesn't have *eqendof(z) is false*.

□

Wasn't *that* fun!
