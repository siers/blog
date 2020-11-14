---
title: Python cannot even `uniq'
---
## Almost everywhere but...
It's ridiculous — ruby, haskell, even sh has it!

    λ: import Data.List # this is haskell
    λ: nub [1,2,3,4,5,6,1,2,3,6,4,3,5,3,2,4,5,3,1,3,5]
    [1,2,3,4,5,6]

    irb(main):001:0> (b = [4, 'a', 'b', 'c', 9, ['a'], 1]).uniq == b
    => True # this is ruby

Python doesn't have built-in code for removing duplicates from a list (without messing up the order of the list).

## Duty

I thought that it's any standard library's sacred duty
to provide useful utilities to us, the programmers.
Speed and code reuse should be our goal and could be achieved by
avoiding having tiny of oneliners all over the place!
(Which is a real problem given that `uniq' is a common function.)

Acknowledging the fact that python hasn't fulfilled its duty,
I present some of my own code.

## Code
Ok, here's what I have:

    b = [4, 'a', 'b', 'c', 9, ['a'], 1]
    c = [4, 'a', 'b', 'c', 9, 1]
    uniq1 = lambda a: (lambda b: [b.append(x) or x for x in a if x not in b])([])
    uniq2 = lambda a: list(set(a))

    print(uniq1(b) == b)      # line1
    print(uniq1(c) == c)      # line2
    print(uniq2(c) == c)      # line3
    try: print(uniq2(b) == b) # line4
    except TypeError: print('because lists are unhashable, set(b) cannot exist')

    True
    True
    False
    because lists are unhashable, set(b) cannot exist

    # BONUS! Uniq by key:
    from itertools import groupby
    groupsort = lambda l, key: [(k, list(g)) for k, g in groupby(sorted(l, key=key), key)
    uniqkey   = lambda by, a: [list(b)[0] for _, b in groupsort(a, by)]

Note: simplest example for getting a `TypeError` of this kind is `set([[]])`.

## Rebuttal
Supposedly, we can just use `list(set(a))` and I'm just whining about nothing.
And knowing `list(set([1,2,3,1])) == [1,2,3]` you'd guess that `line3 = True`! Well you just got tricked and were lucky that hash of a numbers is the number itself, `hash(1234) => 1234`. Their order won't therefore remain the same in general. `hash('foo') => -740391237` So 3. is false.

The orderless version `uniq2` *can't even* handle b. Due to reasons already mentioned in the note above.

Another outrageous argument that I'd like to refute is that
because the order is ambiguous we shouldn't write this function at all
since which one one should we keep in `[1,2,1]`.
Why not just decide on some default order
that would still be totally useful — first copy stays.

This should've been in the standard library. Weak, python, weak.
