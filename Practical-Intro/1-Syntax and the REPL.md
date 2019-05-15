For the first part of this section I'd suggest working inside Haskell's interactive environment,
called GHCi. It's a read-evaluate-print loop like you'd expect from a language like Python or Lisp.

Calling functions is the bread and butter of programming, so let's start there.
Here's an example of a simple function call:
```
square 5
```

Because Haskell is pure, the style of "do this, then do that" programming doesn't quite fit.
Statements like these inherently have an effect, which modifies the global state of the program.
Instead, Haskell encourages an _entirely_ functional style, where the program is a chain of function
calls. This in turn means that _a lot_ of functions get called, and so the syntax for function
calls is extremely short and quiet- it's whitespace.

So here we have a function, `square`, being applied to one argument, `5`. The syntax for calling a function
with multiple arguments is similar:
```
add 5 10
```
Here we have a function, `add`, beign applied to _two_ arguments, 5 and 10.

Notice that the function is always in what we call "prefix" position. The function comes before all the arguments.
For functions of two arguments, we can put the function in the "infix" position with backquotes:
```
5 `add` 10
```
Of course this would make more sense as an operator, and Haskell has those too:
```
5 + 10
```
But it helps to think of operators as also being functions of two arguments. In fact, we can put operators in
prefix form too if we wrap them in parenthesis:
```
(+) 5 10
```
is exactly identical to what we had before.

With this we could already use Haskell as a glorified calculator, but there are a couple more things we need to write
a real program. For one, we need to be able to define our own functions. Here's how we might have defined `square`:
```
square x = x * x
```
Here we say "the function square, applied to an argument x, is the same as x times itself."
And defining functions of multiple arguments looks the same:
```
add x y = x + y
```
Now you'll notice if we call these functions at the prompt, everything works perfectly. But Haskell is a statically
typed language. Where are the types? Haskell's _type inference_ could _infer_ the types of all these functions for
us, and checked that they matched when we called them. I'll come back to reading the types of errors you get when they
don't match later.

At this point we are switching to real source files. For those unaware, the Haskell source file extension is `.hs`.

When writing real code, we want to put the type signatures into the code, rather than letting the compiler infer them all.
This helps our code be more readable, and the type of a function will often help show you how to implement the function.
To specify a type, we use the `::` symbol. Here's a simple function with a type:
```Haskell
intId :: Int -> Int
intId x = x
```
Here, our function has type "Int to Int"; it's a function with one argument. But nowhere in the definition
of this function did we rely on `x` being an `Int`. To increase code reusability, we should generalize the function.
Rather than specifying that the argument _must_ be an int, we can use a _type variable_. By convention, we write
actual types beginning with a capital letter, and type variables as a single lowercase letter. So our generalization is:
```Haskell
id :: a -> a
id x = x
```
This function is now _polymorphic_; it works if I give it an argument of _any_ type. `id 5` is `5`, `id False` is `False`,
and even `id (+)` is `(+)`. Functions in Haskell are treated just like any other data, so they can be passed into functions
or returned from other functions. In fact, let's look at the type of `(+)`:
```Haskell
(+) :: Integer -> Integer -> Integer
```
What is this? What this says is that `(+)` takes an Integer, and evaluates to _another function_. That function
will then take an integer, and evaluate to an integer. This sort of argument-by-argument evaluation is called
"currying". Every Haskell function is curried. Therefore you can think of the call to `add` earlier in the section as:
```Haskell
add 5 10
-- is the same as
(add 5) 10
```
But be aware that this is _not_ the same as `add (5 10)`. Haskell will try to interpret this as "add applied to
the result of applying 5 to 10." But 5 isn't a function, and so it can't be applied to 10. The type checker
will point out this mistake.

In fact this is not the most general type for `(+)`. We'll get to that next time.

Here's how we might define a factorial function:
```Haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n - 1))
```
We're explicitly telling Haskell here to treat the `0` case specially. This is called "pattern matching," and it's
another of Haskell's flagship features. Matching against a variable name, as in the second case, will always succeed,
and patterns are tested top down. So if we try to call factorial of 0, we'll just get 1 immediately. Otherwise we'll recurse
as you might expect. With just these tools, we have enough to do anything. 

At this point we're ready to start on the tutorial project, a simple interpreter for the lambda calculus.
I've set this up as a modification of the interpreter Steven Diehl builds in Chapter 4 of his (incomplete) "Write Yourself a Haskell."
The lambda calculus is very closely related to Haskell itself. Every statement in lambda calculus is an "expression."
Expressions can be either variable names, functions, or function applications. We need a way to express this in Haskell:
```Haskell
data Expr = Var String | Lam String Expr | App Expr Expr
```
This reads exactly like our description: "An Expr is either a Var with a name, or a lambda with an argument name and a body,
or an application of an Expr to another Expr." This is very terse and there's a lot going on here.
Firstly, we've added `Expr` to the "type language." `Expr` is now a valid type that can appear in type signatures of functions.
We've also specified what types of things _are_ `Expr`s. `Var "bob"` is an Expr, as it matches the first case.
`Lam "x" (Var "y")` is also an `Expr`, as it matches the second case. The recursive structure is not a problem.

Another way to think about the right hand side of the data declaration is as declaring 3 new functions. Their types look like this:
```Haskell
Var :: String -> Expr
Lam :: String -> Expr -> Expr
App :: Expr -> Expr -> Expr
```
In fact, this is effectively what happens and we call these functions "Data constructors."

There are two things we can do to help make this data declaration more readable.
The first is to line up the different cases vertically, like this:
```Haskell
data Expr = Var String
          | Lam String Expr
          | App Expr Expr
```
This helps separate the cases visually and make it more apparent where the boundaries are. Much of good Haskell style
is based on trying to line things up visually where appropriate, while moving noise characters (like the `|` here) to the edges.

The other thing we can do is to try and explain what the `String`s being passed to `Var` and `Lam` represent. We can do
this with a "type synonym":
```Haskell
type Name = String
data Expr = Var Name
          | Lam Name Expr
          | App Expr Expr
```
At this point it's easier to see what the data structure represents. We could build an interpreter on top of this,
but we probably want special ways to represent some particular values, like ints and booleans. The true lambda
calculus would represent them as special functions, but we won't go to that extreme. All we need to do is add a new
data constructor to `Expr`:
```Haskell
data Expr = Var Name
          | Lit Lit
          | Lam Name Expr
          | App Expr Expr
```
and we'll want a data type to represent the possible literals:
```Haskell
data Lit = LInt Int | LBool Bool
```
`Bool` is the standard builtin Haskell type for booleans. In fact, it's not wired in like in other languages,
it's defined in the language itself:
```Haskell
data Bool = True | False
```

There are a couple nice features we want these two data types to have. First, we want a way to compare them for equality:
```Haskell
eqExpr :: Expr -> Expr -> Bool
eqExpr (Var name1) (Var name2)             = name1 == name2
eqExpr (Lit lit1) (Lit lit2)               = lit1  == lit2
eqExpr (Lam name1 body1) (Lam name2 body2) = name1 == name2 && body1 `eqExpr` body2
eqExpr (App a1 b1) (App a2 b2)             = a1 `eqExpr` a2 && b1 `eqExpr` b2
eqExpr _ _ = False

eqLit :: Lit -> Lit -> Bool
eqLit (LInt i) (LInt j)     = i  == j
eqLit (LBool b1) (LBool b2) = b1 == b2
eqLit _ _ = False
```
Notice the use of pattern matching to extract the data from inside the Exprs. We also use the wildcard pattern `_`.
The wildcard pattern always matches, but tells the compiler that we don't care about what the argument is; we ignore it.

This works, but it's a lot of boilerplate for two very mechanically-defined functions. It turns out these functions
are _so_ mechanical that the compiler can do it for us:
```Haskell
data Expr = Var Name
          | Lit Lit
          | Lam Name Expr
          | App Expr Expr
          deriving Eq

data Lit = LInt Int | LBool Bool deriving Eq
```
This will produce the exact same code, except our functions will both be named `==`.
We'll talk more about how this overloading works in the next section.

We also want a way to convert both types to Strings. Fortunately, there's also a very mechanical way to to do this.
All we need to do is add `Show` to the deriving clause. The final data declarations look like this:
```Haskell
data Expr = Var Name
          | Lit Lit
          | Lam Name Expr
          | App Expr Expr
          deriving (Eq, Show)
          
data Lit = LInt Int | LBool Bool deriving (Eq, Show)
```
The function to convert an `Expr` or `Lit` to a string is called `show`. Open your file in GHCi, either by starting
GHCi with the command `ghci <filename>` or by starting `ghci` and using the command `:l[oad] <filename>`, and give it a shot!
`Show` functions derived by the compiler are usually not the cleanest. But they work just fine in many cases. In the next
section we'll talk more about how to make better `show` functions.

One function we might want to define on the `Lit` type does absolute value for LInts:
```Haskell
absL :: Lit -> Lit
absL (LInt i) = undefined
absL lbool = lbool
```
`undefined` is a useful standin for when you need a function stub. However if you try and run Haskell code with `undefined` in it,
if Haskell tries to evaluate the `undefined`, it'll crash. `undefined` has type `a`, so it can stand in for literally anything.
People also commonly refer to `undefined` as "bottom."

But what do we do here? The first idea is to try Haskell's `if` construct:
```Haskell
absL :: Lit -> Lit
absL (LInt i) = if i < 0
                then LInt (negate i)
                else LInt i
absL lbool = lbool
```
See how we have what looks like a ternary expression in other languages: `if <condition> then <case1> else <case2>`.
In Haskell, every expression must produce a value, so there is no `if` without both the `then`, and the `else` cases.

Another option is to use _guards_. Guards are a way to do a second check on a pattern match:
```Haskell
absL :: Lit -> Lit
absL (LInt i) | i < 0     = LInt (negate i)
              | otherwise = LInt i
absL lbool = lbool
```
`otherwise` is a synonym for `True`.

We have some repitition here that we can avoid. We see `LInt i` twice. It would be better if we could tell Haskell
not to duplicate data we already have:
```Haskell
absL :: Lit -> Lit
absL prev@(LInt i) | i < 0     = LInt (negate i)
                   | otherwise = prev
absL lbool = lbool
```
Of course the compiler can perform this optimization for you, but when working with much larger data types it is much more readable.

We can also pattern match on the right hand side of a function definition, using `case` statements:
```Haskell
notL :: Lit -> Lit
notL lit = case lit of
    LInt _  -> lit
    LBool b -> LBool (not b)
```
Case statements are extremely common, so use them judiciously. Pattern matching is readable enough on its own, however one
other common method to improve readability is to try and line up the `->` arrows. Use best judgement; oftentimes this aids readability,
but not always.

Less commonly, we may want a way to double-check a pattern match in a case. Guards also work inside case statements, because guards
are a way to double-check _any_ pattern match, not just matches on the left hand side of function definitions.

We can also create local bindings inside functions, which is super helpful for readability.
These functions:
```Haskell
addL :: Lit -> Lit -> Lit
addL (LInt i) (LInt j) = LInt (i + j)
addL _ _ = LBool False

times4 :: Lit -> Lit
times4 lit = addL (addL lit lit) (addL lit lit)
```
are tricky to read. We can make it better in either of two ways. The first way is a `let` binding:
```Haskell
times4' :: Lit -> Lit
times4' lit = let times2 = addL lit lit 
              in addL times2 times2
```
The second way is a `where` binding:
```Haskell
times4'' lit = addL times2 times2
  where times2 = addL lit lit
```
Both of these are easier to read.

`let` and `where` bindings can also perform pattern matches on the left-hand side of equalities, just like function definitions.
When pattern matches like these are used to pull data out of a datatype, they are often refered to as "destructuring pattern matches" or just "destructuring."

We can also create multiple bindings with both constructs:
```Haskell
calculation1 :: Int -> Int -> Int
calculation1 x y = let a = x + y
                       b = x - y
                   in a * b

calculation2 :: Int -> Int -> Int
calculation2 x y = sum / diff
  where sum  = x + y
        diff = x - y
```

I want to end this section by discussing other miscellaneous points that were not easy to fit into the scope of the interpreter at this stage, but which we may see in the next couple of sections. However I consider them extremely important in practical Haskell code.

Firstly, the list type is special. We can imagine that it was declared as something like:
```Haskell
data [a] = [] | a : [a]
```
This looks quite different from the other datatypes we saw. What's going on here? The `[a]` on the left hand side is basically
using `[]` as a "circumfix" operator, which goes _around_ its argument. Haskell does not let us make our own circumfix operators!
This works like a "type-level function;" `[Int]` is a list of `Int`s, `[Bool]` is a list of `Bool`s, `[[Double]]` is a list of lists of `Double`s. On the right hand side, we have either an empty list `[]` or an _infix_ data constructor `:`, read "cons." The `:` operator
combines a "head" of a list, a single element, with the "tail" or rest of the elements.

Pattern matches on lists look like this:
```Haskell
head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs
```
(Note that these functions will both cause crashes if the list is empty, because the pattern match will fail).
This is basically the same as the pattern matching we've seen before; simply the data constructor is an infix operator.
You can make your own operator data constructors, but they _must_ start with the character `:`.

We also have list literals, exactly as you might expect from Javascript or Python:
```Haskell
[1, 2, 3, 4]
[1 .. 9] -- [1, 2, 3, 4, 5, 6, 7, 8, 9]
[1, 3 .. 9] -- [1, 3, 5, 7, 9]
[1 .. ] -- ALL positive integers
```
The last example works because Haskell is lazy. If you ask it to print that list, you're in trouble. But if you just use
it as the source for a computation that only needs, say, 500 of them, then that's no problem. Haskell will only evaluate the list
as far as it absolutely needs to.

Haskell is a higher order language, and that means our functions can take functions as arguments. Here's an extremely useful function
on lists, which applies a given function to every element of the list:
```Haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = (f x) : (map f xs)
```
Notice the pattern match, the recursion, and the base case. For the sake of demonstration, you can also match particular lists:
```Haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f [x]    = [f x]
map f (x:xs) = (f x) : (map f xs)
```
is identical, albiet a bit harder to understand quickly.

Another extremely useful function "filters" a list for elements that satisfy a given boolean-valued predicate:
```Haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x 
                  then x : tail 
                  else tail
  where tail = filter p xs
```
Here we make use of local binding with `where` to avoid duplicating `filter p xs`.

When working with higher order functions, it can be extremely annoying, tedious, and/or wasteful to have to write a named function
for every simple functionality. To this end, like any reasonable higher-order language, Haskell provides a syntax for lambda expressions.
```Haskell
Prelude> filter (\x -> x <= 5) [0..10]
[0, 1, 2, 3, 4, 5]
```
Here the `\x -> x <= 5` is a function. If you squint _really_ hard, `\` looks like a lambda. We can also write this simple
function with something called an "operator section," which looks like `(<= 5)`. This is interpreted as exactly the same lambda.
Similarly, `(5 <=)` would be interpreted as `\x -> 5 <= x`.

One last note is that Haskell supports tuples (and triples etc.). The syntax is exactly as you'd expect:
```Haskell
mkTuple :: a -> b -> (a, b)
mkTuple x y = (x, y)

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y
```
`fst` and `snd` get used fairly frequently, so they have very shortened names. You can also see here how to pattern match on tuples
and declare the type of a tuple.

That's almost all the major points of Haskell's syntax. Let's open this file in GHCi and poke around a bit.
Start up ghci and type `:l[oad]` followed by the filename. To see the help menu for commands, type `:?`.

The first thing that is useful when loading a file or package into GHCi is to check what functions are available. We do this
with the `:browse` command. This lists functions with their types. To see the type of a specific expression, use `:t`:
```Haskell
Prelude> :t addL
addL :: Lit -> Lit -> Lit
```
`:t` is probably the most useful command at the prompt.

What happens if we try and apply `addL` to the wrong type?
```Haskell
addL True False
```
     => produces the error "Couldn't match expected type Lit with actual type Bool" and location information.
The interpreter tries to help you as much as possible here. The important line is the first one: We said the type
of arguments to `addL` should be Lits, so the expected type was `Lit`. But we gave `addL` `Bool`s, and so the actual
type was `Bool`. These types don't match. The rest of the error tries to point you at exactly where the mismatch happened,
because the point where the interpreter finds the mismatch may not be exactly where the problem actually is, or where you
expect it to be. A good rule of thumb when reading Haskell's error messages is to focus on the first line for content,
and the rest for location.

What about just typing in `0 0` at the prompt? This looks like we're trying to apply `0` to `0`, but `0` isn't a function.
     => produces "Non type-variable argument in the constraint: Num (t1 -> t2)
This error message means Haskell thinks we're trying to use a function as a number. Really it's the other way around,
but it's not possible in general for the compiler to figure out what we meant, and this is the best it can do. The important
part of this error message is the rightmost part, in the constraint. Here we can see that we've somehow confused a Number for
a function.

What if we try to give a function too many arguments? `id True True`
     => produces "Couldn't match expected type 'Bool -> t' with actual type 'Bool'
In this case, Haskell tried to check the type of `id True` and found `Bool`. Then it tried to check the type of `(id True) True)` -
but the actual type, `Bool`, is not a function type, which is what the context expected. We also get some information that
the function `id` was applied to 2 arguments but it's type only has 1. This is usually enough to find problems with this error.

Sometimes we mess up when we needed parenthesis on a function argument. For example;
```Haskell
f x = f f x
```
     => produces "cannot construct infinite type"
This error message is trying to tell us that in order for the types to check out, we would need an "infinite type"
like `Int -> Int -> Int ...` which is not allowed. In this case, `x`s type was inferred as `t`, and `f`s type was inferred as
`f :: t -> t -> t1`, because on the left hand side we've said the first argument is of type `t`, and on the right hand side
we've said the second argument is also of type `t`. But this is a problem, because now the middle `f` needs to have type
`t`, and type `t -> t -> t1` at the same time. This is possible, but only if `t` is an infinite function type, which is
not allowed. What we probably meant was
```Haskell
f x = f (f x)
```
which is perfectly legal, even though trying to call it will cause an infinite recursion.

There's also a rare error that references "syntactic negation." This comes up because of the way Haskell treats literals like
`-1`. If you see it, just put parenthesis around it like `(-1)`. In fact, this is a best practice to do anyway, because it's
easier to distinguish from using `-` as an operator.

That's all for the first section! With that, we've covered enough of Haskell's syntax and basic ideas to write short programs.
Give it a whirl!
