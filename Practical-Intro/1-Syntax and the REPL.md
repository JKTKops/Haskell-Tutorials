Primary Goal: All major points of haskell syntax, barring do-notation and language extensions

Minor goals:
1) Basic function call syntax
    1) prefix form
    2) multiple arguments
    3) infix form (backquotes, operators)
2) Function definition syntax
    1) Type signatures
    2) pattern matching, case-of, guards (otherwise), let-in, where, if-then-else
    3) wildcard: `_`
    4) Operators
3) Data declarations
    1) Distinction between type and value languages
    2) Deriving Eq, Show, Read, Ord, Enum, Bounded (discussed in depth elsewhere)
    3) Recursive data types
4) Revisit function definition with a custom data type (a tree?)
5) Starting at using the REPL
    1) Reading error messages
    2) Loading/reloading source files and importing packages
    3) :browse, :q, :?
5) Mention syntactic negation and how to fix corresponding errors

For the first part of this video I'm going to be working inside Haskell's interactive environment,
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
And defining functions of multiple variables looks the same:
```
add x y = x + y
```
Now you'll notice if we call these functions at the prompt, everything works perfectly. But Haskell is a statically
typed language. Where are the types? Haskell's _type inference_ could _infer_ the types of all these functions for
us, and checked that they matched when we called them. I'll come back to reading the types of errors you get when they
don't match later.

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
"currying". Every Haskell function is curried. Therefore you can think of the call to `add` earlier in the video as:
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
is based on trying to line things up visually where appropriate, while moving noise characters (like `|` here) to the edges.

The other thing we can do is to try and explain what the `String`s being passed to `Var` and `Lam` represent. We can do
this with a "Type synonym":
```Haskell
type Name = String
data Expr = Var Name
          | Lam Name Expr
          | App Expr Expr
```
At this point it's easier to see what the data structure represents. We could build an interpreter on top of this,
but we probably want special ways to represent some particular values, like ints and booleans. The true lambda
calculus would represent them as special functions, but we won't go that extreme. All we need to do is add a new
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
We'll talk more about how this overloading works in the next video.

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

One function we might want to define on the `Lit` type does absolute value for LInts:
```Haskell
absL :: Lit -> Lit
absL (LInt i) = undefined
absL lbool = lbool
```
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
    LInt _ -> lit
    LBool b -> LBool (not b)
```
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

That's almost all the major points of Haskell's syntax. Let's open this file in GHCi and poke around a bit.
Start up ghci and type `:l` followed by the filename. To see the help menu for commands, type `:?`.

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

I want to end this video by discussing other miscellaneous points that were not easy to fit into the scope of the interpreter at this stage.
1) list types and pattern matching on lists, list literals, list (stepped, infinite) ranges
2) examples of higher order functions like `map`, `filter`, and `any`
3) lambda syntax
