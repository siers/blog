---
title: A simple exercise in lambda calculus
---
Today I was talking with my brother about [generic data types](https://wiki.haskell.org/Generics)
and the list type `[a]` with the intent of eventually showing him the generic sum type `Either`.
I mentioned that the definition of list doesn't care what it's storing,
while simultaneously showing the type of haskell's cons function `:`.

    :t (:)
    (:) :: a -> [a] -> [a]
    1 : [2]
    [1,2]

He sort of wondered could you do something of `[2] : 1` sort to get `[2,1]`.
Although he readily remembered about
[`(++)`](https://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:-43--43-),
a couple of minutes passed and I came up with a definition that did the same thing with `:` and friends.

    let f = (reverse . ) . (flip (:)) . reverse
    [1..5] `f` 6
    [1,2,3,4,5,6]

Being largely unaware of the horrors of [pointfree](https://wiki.haskell.org/Pointfree) tricks at the time,
he wondered why it worked. Knowing he had some knowledge of lambda calculus, I gave him the
challenge of unraveling the definition, finding the beta-equivalent version of it using
[beta reduction](https://en.wikipedia.org/wiki/Lambda_calculus#Reduction),
the definitions of `flip` and `.`[^definitions] while taking currying into account.

And then I watched him struggle! In the end he did, in fact, come up with the right one, surprisingly.
And now I thought — mustn't be too hard, should it? And so I set out to try it myself.

Here you can see the process extracted from the ghci session,
with some redundant whitespace for easier comprehension.

    let f = (reverse . ) . (flip (:)) . reverse
    :t f
    f :: [a] -> a -> [a]

    :t         (reverse . ) .        (flip (:)) . reverse
    :t         (reverse . ) . (\x -> (flip (:)) (reverse x))
    :t (\y ->  (reverse . )  ((\x -> (flip (:)) (reverse x)) y))
    :t (\y ->   reverse .    ((\x -> (flip (:)) (reverse x)) y))
    :t (\y z -> reverse     (((\x -> (flip (:)) (reverse x)) y) z))

Let's remove some of that whitespace now.

    :t (\y z -> reverse (((\x -> (flip (:)) (reverse x)) y) z))
    :t (\y z -> reverse (((\x ->  flip (:)  (reverse x)) y) z))
    :t (\y z -> reverse ((        flip (:)  (reverse y)   ) z))
    :t (\y z -> reverse ((flip (:) (reverse y)) z))   -- whitespace only
    :t (\y z -> reverse  (z : (reverse y)))           -- steps ommited

Now, we could use haskell's `$`[^dollar] to get rid of parentheses.
To do that, we first prove `f x = $ f x` with `z = id z` and eta conversion.

    f x = id f x = (\f -> f) f x
    (\f -> f) f x
    (\f -> (\x -> f x)) f x -- eta conversion
    (\f x -> f x) f x       -- shorthand
    ($) f x = f $ x         -- haskell allows infix syntax
    □

Using the above theorem **and** given that `$` is applied last:

    :t (\y z -> reverse   (z : (reverse y)))
    :t (\y z -> reverse $ (z : (reverse y)))
    :t (\y z -> reverse $  z : (reverse y))
    (\y z -> reverse $ z : (reverse y)) :: [a] -> a -> [a]

Does it work too?

    let g = (\y z -> reverse $ z : (reverse y))
    [1..5] `g` 6
    [1,2,3,4,5,6]

Great! That was a pretty lovely exercise.

[^definitions]: Definitions of `.` and `flip` along with anonymous versions.

        (f . g) x  = f (g x)
        . = λf g x → f (g x)
        . is also associative

        flip    f x y → f y x
        flip = λf x y → f y x

[^dollar]: Definition of `$` along with anonymous versions.

        f $ x    = f (g x)
        $ = λf x → f x
