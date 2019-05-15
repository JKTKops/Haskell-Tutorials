The goal of this tutorial series is to offer those interested in Haskell a way to learn enough of the language
to use it competently in hopefully as short as a few hours. In the first few sections of this tutorial, we will build
a lambda calculus interpreter just so we have a project to chase. Since Haskell is commonly used in 
interpreter/compiler design, I hope that this will be of general interest to those who want to learn the language.

Following that I'll continue with several more detailed tutorials on specific features of the language and writing cleaner,
better code in general. I'll also show how to use some common Haskell packages and how to use Stack to get new ones.
I won't cover using Stack as a build tool - that is straight forward enough to find on Stack's tutorial page and 
I feel that is sufficient.

If you're approaching Haskell for the first time, and you do not know much about it, this series can still be for you!
Haskell is what is known as a "lazy, pure functional language." "Lazy" means the language will wait to compute values 
until they are actually needed. This requires "purity," which means functions cannot have side effects.
"Functional" means that functions are values as much as numbers or lists are values. If you have experience with lambda
functions in other languages, you've been exposed to this idea before. Additionally, because functions can't have side effects, 
Haskell has a way to "lift" side effects into actual values, so that side effects can be arguments or results of functions, 
or themselves stored inside other data structures.

This "first class side effect" feature of the language, as well as its declarative style, makes Haskell a
very high level language, meaning that you get to spend more time focusing on how to model data and computations
and much less time focusing on the individual steps needed to accomplish your goals. And, because of laziness and purity,
this level of abstraction is no barrier to performance. Well-written Haskell code can perform with 10% of C, and sometimes
equally as fast. And Haskell's terse and expressive syntax means even optimized code is often shorter and easier to read
than an imperative equivalent, meaning Haskell code is easier to maintain, and often has fewer bugs. And of course,
Haskell has strong static typing and powerful type inference.
