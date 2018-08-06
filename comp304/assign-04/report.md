% COMP304 Assigment 4
% David Barnett (300313764)

# Design

The design for my Dr. Roberts is to only produce a single answer to a question,
this lead me down the path of using the `->` operator.

Before I had `translate` like `translate([i | Rest], [you], Rest)` were the
1st argument is the question being translated and the 2nd argument is the translated
portion and 3rd argument is the untranslated portion. This style worked by produced
additional solutions, for example the phrase `[i, am]` would produce the results:
`[you, are]`, `[you, am]`, `[you, me]`, `[i, me]` etc.

I thought this to be incorrect and tired to eliminate all other possible solutions.

# 1.3

My predicate cannot produce an input list given the answer.
I believe this is caused by my usage of `->` operator and forcing the logic
flow to be dependent on the input list. Naively I believe it is not possible to
reverse a `->` operator.

The changes I could make so my predicate could create the input list would be to remove
all usages of `->` and go back to my previous design and try to find different way to prevent
many answers or just accept them.
