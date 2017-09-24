# The Eff Monad

## Chapter Goals

In the last chapter, we introduced applicative functors, an abstraction which we used to deal with _side-effects_: optional values, error messages and validation. This chapter will introduce another abstraction for dealing with side-effects in a more expressive way: _monads_.

The goal of this chapter is to explain why monads are a useful abstraction, and their connection with _do notation_. We will build upon the address book example of the previous chapters, by using a particular monad to handle the side-effects of building a user interface in the browser. The monad we will use is an important monad in PureScript - the `Eff` monad - used to encapsulate so-called _native_ effects.

## Project Setup

The source code for this project builds on the source for the previous chapter. The modules from the previous project are included in the `src` directory for this project.

The project adds the following Bower dependencies:

- `purescript-eff`, which defines the `Eff` monad, the subject of the second half of the chapter.
- `purescript-react`, a set of bindings to the React user interface library, which we will use to build a user interface for our address book application.

In addition to the modules from the previous chapter, this chapter's project adds a `Main` module, which provides the entry point to the application, and functions to render the user interface.

To compile this project, first install React using `npm install`, and then build and bundle the JavaScript source with `pulp browserify --to dist/Main.js`. To run the project, open the `html/index.html` file in your web browser.

## Monads and Do Notation

Do notation was first introduced when we covered _array comprehensions_. Array comprehensions provide syntactic sugar for the `concatMap` function from the `Data.Array` module.

Consider the following example. Suppose we throw two dice and want to count the number of ways in which we can score a total of `n`. We could do this using the following non-deterministic algorithm:

- _Choose_ the value `x` of the first throw.
- _Choose_ the value `y` of the second throw.
- If the sum of `x` and `y` is `n` then return the pair `[x, y]`, else fail.

Array comprehensions allow us to write this non-deterministic algorithm in a natural way:

```haskell
import Prelude

import Control.Plus (empty)
import Data.Array ((..))

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [x, y]
    else empty
```

We can see that this function works in PSCi:

```text
> countThrows 10
[[4,6],[5,5],[6,4]]

> countThrows 12  
[[6,6]]
```

In the last chapter, we formed an intuition for the `Maybe` applicative functor, embedding PureScript functions into a larger programming language supporting _optional values_. In the same way, we can form an intuition for the _array monad_, embedding PureScript functions into a larger programming language supporting _non-deterministic choice_.

In general, a _monad_ for some type constructor `m` provides a way to use do notation with values of type `m a`. Note that in the array comprehension above, every line contains a computation of type `Array a` for some type `a`. In general, every line of a do notation block will contain a computation of type `m a` for some type `a` and our monad `m`. The monad `m` must be the same on every line (i.e. we fix the side-effect), but the types `a` can differ (i.e. individual computations can have different result types).

Here is another example of do notation, this type applied to the type constructor `Maybe`. Suppose we have some type `XML` representing XML nodes, and a function

```haskell
child :: XML -> String -> Maybe XML
```

which looks for a child element of a node, and returns `Nothing` if no such element exists.

In this case, we can look for a deeply-nested element by using do notation. Suppose we wanted to read a user's city from a user profile which had been encoded as an XML document:

```haskell
userCity :: XML -> Maybe XML
userCity root = do
  prof <- child root "profile"
  addr <- child prof "address"
  city <- child addr "city"
  pure city
```

The `userCity` function looks for a child element `profile`, an element `address` inside the `profile` element, and finally an element `city` inside the `address` element. If any of these elements are missing, the return value will be `Nothing`. Otherwise, the return value is constructed using `Just` from the `city` node.

Remember, the `pure` function in the last line is defined for every `Applicative` functor. Since `pure` is defined as `Just` for the `Maybe` applicative functor, it would be equally valid to change the last line to `Just city`.

## The Monad Type Class

The `Monad` type class is defined as follows:

```haskell
class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m
```

The key function here is `bind`, defined in the `Bind` type class. Just like for the `<$>` and `<*>` operators in the `Functor` and `Apply` type classes, the Prelude defines an infix alias `>>=` for the `bind` function.

The `Monad` type class extends `Bind` with the operations of the `Applicative` type class that we have already seen.

It will be useful to see some examples of the `Bind` type class. A sensible definition for `Bind` on arrays can be given as follows:

```haskell
instance bindArray :: Bind Array where
  bind xs f = concatMap f xs
```

This explains the connection between array comprehensions and the `concatMap` function that has been alluded to before.

Here is an implementation of `Bind` for the `Maybe` type constructor:

```haskell
instance bindMaybe :: Bind Maybe where
  bind Nothing  _ = Nothing
  bind (Just a) f = f a
```

This definition confirms the intuition that missing values are propagated through a do notation block.

Let's see how the `Bind` type class is related to do notation. Consider a simple do notation block which starts by binding a value from the result of some computation:

```haskell
do value <- someComputation
   whatToDoNext
```

Every time the PureScript compiler sees this pattern, it replaces the code with this:

```haskell
bind someComputation \value -> whatToDoNext
```

or, written infix:

```haskell
someComputation >>= \value -> whatToDoNext
```

The computation `whatToDoNext` is allowed to depend on `value`.

If there are multiple binds involved, this rule is applied multiple times, starting from the top. For example, the `userCity` example that we saw earlier gets desugared as follows:

```haskell
userCity :: XML -> Maybe XML
userCity root =
  child root "profile" >>= \prof ->
    child prof "address" >>= \addr ->
      child addr "city" >>= \city ->
        pure city
```

It is worth noting that code expressed using do notation is often much clearer than the equivalent code using the `>>=` operator. However, writing binds explicitly using `>>=` can often lead to opportunities to write code in _point-free_ form - but the usual warnings about readability apply.

## Monad Laws

The `Monad` type class comes equipped with three laws, called the _monad laws_. These tell us what we can expect from sensible implementations of the `Monad` type class.

It is simplest to explain these laws using do notation.

### Identity Laws

The _right-identity_ law is the simplest of the three laws. It tells us that we can eliminate a call to `pure` if it is the last expression in a do notation block:

```haskell
do
  x <- expr
  pure x
```

The right-identity law says that this is equivalent to just `expr`.

The _left-identity_ law states that we can eliminate a call to `pure` if it is the first expression in a do notation block:

```haskell
do
  x <- pure y
  next
```

This code is equivalent to `next`, after the name `x` has been replaced with the expression `y`.

The last law is the _associativity law_. It tells us how to deal with nested do notation blocks. It states that the following piece of code:

```haskell
c1 = do
  y <- do
    x <- m1
    m2
  m3
```

is equivalent to this code:

```haskell  
c2 = do
  x <- m1
  y <- m2
  m3
```

Each of these computations involves three monadic expression `m1`, `m2` and `m3`. In each case, the result of `m1` is eventually bound to the name `x`, and the result of `m2` is bound to the name `y`.

In `c1`, the two expressions `m1` and `m2` are grouped into their own do notation block.

In `c2`, all three expressions `m1`, `m2` and `m3` appear in the same do notation block.

The associativity law tells us that it is safe to simplify nested do notation blocks in this way.

_Note_ that by the definition of how do notation gets desugared into calls to `bind`, both of `c1` and `c2` are also equivalent to this code:

```haskell  
c3 = do
  x <- m1
  do
    y <- m2
    m3
```

## Folding With Monads

As an example of working with monads abstractly, this section will present a function which works with any type constructor in the `Monad` type class. This should serve to solidify the intuition that monadic code corresponds to programming "in a larger language" with side-effects, and also illustrate the generality which programming with monads brings.

The function we will write is called `foldM`. It generalizes the `foldl` function that we met earlier to a monadic context. Here is its type signature:

```haskell
foldM :: forall m a b
       . Monad m
      => (a -> b -> m a)
      -> a
      -> List b
      -> m a
```

Notice that this is the same as the type of `foldl`, except for the appearance of the monad `m`:

```haskell
foldl :: forall a b
       . (a -> b -> a)
      -> a
      -> List b
      -> a
```

Intuitively, `foldM` performs a fold over a list in some context supporting some set of side-effects.

For example, if we picked `m` to be `Maybe`, then our fold would be allowed to fail by returning `Nothing` at any stage - every step returns an optional result, and the result of the fold is therefore also optional.

If we picked `m` to be the `Array` type constructor, then every step of the fold would be allowed to return zero or more results, and the fold would proceed to the next step independently for each result. At the end, the set of results would consist of all folds over all possible paths. This corresponds to a traversal of a graph!

To write `foldM`, we can simply break the input list into cases.

If the list is empty, then to produce the result of type `a`, we only have one option: we have to return the second argument:

```haskell
foldM _ a Nil = pure a
```

Note that we have to use `pure` to lift `a` into the monad `m`.

What if the list is non-empty? In that case, we have a value of type `a`, a value of type `b`, and a function of type `a -> b -> m a`. If we apply the function, we obtain a monadic result of type `m a`. We can bind the result of this computation with a backwards arrow `<-`.

It only remains to recurse on the tail of the list. The implementation is simple:

```haskell
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs
```

Note that this implementation is almost identical to that of `foldl` on lists, with the exception of do notation.

We can define and test this function in PSCi. Here is an example - suppose we defined a "safe division" function on integers, which tested for division by zero and used the `Maybe` type constructor to indicate failure:

```haskell
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)
```

Then we can use `foldM` to express iterated safe division:  

```text
> import Data.List

> foldM safeDivide 100 (fromFoldable [5, 2, 2])
(Just 5)

> foldM safeDivide 100 (fromFoldable [2, 0, 4])
Nothing
```

The `foldM safeDivide` function returns `Nothing` if a division by zero was attempted at any point. Otherwise it returns the result of repeatedly dividing the accumulator, wrapped in the `Just` constructor.

## Monads and Applicatives

Every instance of the `Monad` type class is also an instance of the `Applicative` type class, by virtue of the superclass relationship between the two classes.

However, there is also an implementation of the `Applicative` type class which comes "for free" for any instance of `Monad`, given by the `ap` function:

```haskell
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  pure (f a)
```

If `m` is a law-abiding member of the `Monad` type class, then there is a valid `Applicative` instance for `m` given by `ap`.

The interested reader can check that `ap` agrees with `apply` for the monads we have already encountered: `Array`, `Maybe` and `Either e`.

If every monad is also an applicative functor, then we should be able to apply our intuition for applicative functors to every monad. In particular, we can reasonably expect a monad to correspond, in some sense, to programming "in a larger language" augmented with some set of additional side-effects. We should be able to lift functions of arbitrary arities, using `map` and `apply`, into this new language.

But monads allow us to do more than we could do with just applicative functors, and the key difference is highlighted by the syntax of do notation. Consider the `userCity` example again, in which we looked for a user's city in an XML document which encoded their user profile:

```haskell
userCity :: XML -> Maybe XML
userCity root = do
  prof <- child root "profile"
  addr <- child prof "address"
  city <- child addr "city"
  pure city
```

Do notation allows the second computation to depend on the result `prof` of the first, and the third computation to depend on the result `addr` of the second, and so on. This dependence on previous values is not possible using only the interface of the `Applicative` type class.

Try writing `userCity` using only `pure` and `apply`: you will see that it is impossible. Applicative functors only allow us to lift function arguments which are independent of each other, but monads allow us to write computations which involve more interesting data dependencies.

In the last chapter, we saw that the `Applicative` type class can be used to express parallelism. This was precisely because the function arguments being lifted were independent of one another. Since the `Monad` type class allows computations to depend on the results of previous computations, the same does not apply - a monad has to combine its side-effects in sequence.

X> ## Exercises
X>
X> 1. (Easy) Look up the types of the `head` and `tail` functions from the `Data.Array` module in the `purescript-arrays` package. Use do notation with the `Maybe` monad to combine these functions into a function `third` which returns the third element of an array with three or more elements. Your function should return an appropriate `Maybe` type.
X> 1. (Medium) Write a function `sums` which uses `foldM` to determine all possible totals that could be made using a set of coins. The coins will be specified as an array which contains the value of each coin. Your function should have the following result:
X>
X>     ```text
X>     > sums []
X>     [0]
X>
X>     > sums [1, 2, 10]
X>     [0,1,2,3,10,11,12,13]
X>     ```
X>
X>     _Hint_: This function can be written as a one-liner using `foldM`. You might want to use the `nub` and `sort` functions to remove duplicates and sort the result respectively.
X> 1. (Medium) Confirm that the `ap` function and the `apply` operator agree for the `Maybe` monad.
X> 1. (Medium) Verify that the monad laws hold for the `Monad` instance for the `Maybe` type, as defined in the `purescript-maybe` package.
X> 1. (Medium) Write a function `filterM` which generalizes the `filter` function on lists. Your function should have the following type signature:
X>
X>     ```haskell
X>     filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
X>     ```
X>
X>     Test your function in PSCi using the `Maybe` and `Array` monads.
X> 1. (Difficult) Every monad has a default `Functor` instance given by:
X>
X>     ```haskell
X>     map f a = do
X>       x <- a
X>       pure (f x)
X>     ```
X>
X>     Use the monad laws to prove that for any monad, the following holds:
X>
X>     ```haskell
X>     lift2 f (pure a) (pure b) = pure (f a b)
X>     ```
X>     
X>     where the `Applicative` instance uses the `ap` function defined above. Recall that `lift2` was defined as follows:
X>    
X>     ```haskell
X>     lift2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c
X>     lift2 f a b = f <$> a <*> b
X>     ```

## Native Effects

We will now look at one particular monad which is of central importance in PureScript - the `Eff` monad.

The `Eff` monad is defined in the Prelude, in the `Control.Monad.Eff` module. It is used to manage so-called _native_ side-effects.

What are native side-effects? They are the side-effects which distinguish JavaScript expressions from idiomatic PureScript expressions, which typically are free from side-effects. Some examples of native effects are:

- Console IO
- Random number generation
- Exceptions
- Reading/writing mutable state

And in the browser:

- DOM manipulation
- XMLHttpRequest / AJAX calls
- Interacting with a websocket
- Writing/reading to/from local storage

We have already seen plenty of examples of "non-native" side-effects:

- Optional values, as represented by the `Maybe` data type
- Errors, as represented by the `Either` data type
- Multi-functions, as represented by arrays or lists

Note that the distinction is subtle. It is true, for example, that an error message is a possible side-effect of a JavaScript expression, in the form of an exception. In that sense, exceptions do represent native side-effects, and it is possible to represent them using `Eff`. However, error messages implemented using `Either` are not a side-effect of the JavaScript runtime, and so it is not appropriate to implement error messages in that style using `Eff`. So it is not the effect itself which is native, but rather how it is implemented at runtime.

## Side-Effects and Purity

In a pure language like PureScript, one question which presents itself is: without side-effects, how can one write useful real-world code?

The answer is that PureScript does not aim to eliminate side-effects. It aims to represent side-effects in such a way that pure computations can be distinguished from computations with side-effects in the type system. In this sense, the language is still pure.

Values with side-effects have different types from pure values. As such, it is not possible to pass a side-effecting argument to a function, for example, and have side-effects performed unexpectedly.

The only way in which side-effects managed by the `Eff` monad will be presented is to run a computation of type `Eff eff a` from JavaScript.

The Pulp build tool (and other tools) provide a shortcut, by generating additional JavaScript to invoke the `main` computation when the application starts. `main` is required to be a computation in the `Eff` monad.

In this way, we know exactly what side-effects to expect: exactly those used by `main`. In addition, we can use the `Eff` monad to restrict what types of side-effects `main` is allowed to have, so that we can say with certainty for example, that our application will interact with the console, but nothing else.

## The Eff Monad

The goal of the `Eff` monad is to provide a well-typed API for computations with side-effects, while at the same time generating efficient Javascript. It is also called the monad of _extensible effects_, which will be explained shortly.

Here is an example. It uses the `purescript-random` package, which defines functions for generating random numbers:

```haskell
module Main where

import Prelude

import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (logShow)

main = do
  n <- random
  logShow n
```  

If this file is saved as `src/Main.purs`, then it can be compiled and run using Pulp:

```text
$ pulp run
```

Running this command, you will see a randomly chosen number between `0` and `1` printed to the console.

This program uses do notation to combine two types of native effects provided by the Javascript runtime: random number generation and console IO.

## Extensible Effects

We can inspect the type of main by opening the module in PSCi:

```text
> import Main

> :type main
forall eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit
```

This type looks quite complicated, but is easily explained by analogy with PureScript’s records.

Consider a simple function which uses a record type:

```haskell
fullName person = person.firstName <> " " <> person.lastName
```

This function creates a full name string from a record containing `firstName` and `lastName` properties. If you find the type of this function in PSCi as before, you will see this:

```haskell
forall r. { firstName :: String, lastName :: String | r } -> String
```

This type reads as follows: “`fullName` takes a record with `firstName` and `lastName` fields _and any other properties_ and returns a `String`”.

That is, `fullName` does not care if you pass a record with more fields, as long as the `firstName` and `lastName` properties are present:

```text
> firstName { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }
Phil Freeman
```

Similarly, the type of `main` above can be interpreted as follows: “`main` is a _computation with side-effects_, which can be run in any environment which supports random number generation and console IO, _and any other types of side effect_, and which returns a value of type `Unit`”.

This is the origin of the name “extensible effects”: we can always extend the set of side-effects, as long as we can support the set of effects that we need.

## Interleaving Effects

This extensibility allows code in the `Eff` monad to _interleave_ different types of side-effect.

The `random` function which we used has the following type:

```haskell
forall eff1. Eff (random :: RANDOM | eff1) Number
```

The set of effects `(random :: RANDOM | eff1)` here is _not_ the same as those appearing in `main`.

However, we can _instantiate_ the type of `random` in such a way that the effects do match. If we choose `eff1` to be `(console :: CONSOLE | eff)`, then the two sets of effects become equal, up to reordering.

Similarly, `logShow` has a type which can be specialized to match the effects of `main`:

```haskell
forall eff2. Show a => a -> Eff (console :: CONSOLE | eff2) Unit
```

This time we have to choose `eff2` to be `(random :: RANDOM | eff)`.

The point is that the types of `random` and `logShow` indicate the side-effects which they contain, but in such a way that other side-effects can be _mixed-in_, to build larger computations with larger sets of side-effects.

Note that we don't have to give a type for `main`. The compiler will find a most general type for `main` given the polymorphic types of `random` and `logShow`.

## The Kind of Eff

The type of `main` is unlike other types we've seen before. To explain it, we need to consider the _kind_ of `Eff`. Recall that types are classified by their kinds just like values are classified by their types. So far, we've only seen kinds built from `Type` (the kind of types) and `->` (which builds kinds for type constructors).

To find the kind of `Eff`, use the `:kind` command in PSCi:

```text
> import Control.Monad.Eff

> :kind Eff
# Control.Monad.Eff.Effect -> Type -> Type
```

There are two kinds here that we have not seen before.

`Control.Monad.Eff.Effect` is the kind of _effects_, which represents _type-level labels_ for different types of side-effects. To understand this, note that the two labels we saw in `main` above both have kind `Control.Monad.Eff.Effect`:

```text
> import Control.Monad.Eff.Console
> import Control.Monad.Eff.Random

> :kind CONSOLE
Control.Monad.Eff.Effect

> :kind RANDOM
Control.Monad.Eff.Effect
```

The `#` kind constructor is used to construct kinds for _rows_, i.e. unordered, labelled sets.

So `Eff` is parameterized by a row of effects, and its return type. That is, the first argument to `Eff` is an unordered, labelled set of effect types, and the second argument is the return type.

We can now read the type of `main` above:

```text
forall eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit
```

The first argument to `Eff` is `(console :: CONSOLE, random :: RANDOM | eff)`. This is a row which contains the `CONSOLE` effect and the `RANDOM` effect. The pipe symbol `|` separates the labelled effects from the _row variable_ `eff` which represents _any other side-effects_ we might want to mix in.

The second argument to `Eff` is `Unit`, which is the return type of the computation.

## Records And Rows

Considering the kind of `Eff` allows us to make a deeper connection between extensible effects and records.

Take the function we defined above:

```haskell
fullName :: forall r. { firstName :: String, lastName :: String | r } -> String
fullName person = person.firstName <> " " <> person.lastName
```

The kind of the type on the left of the function arrow must be `Type`, because only types of kind `Type` have values.

The curly braces are actually syntactic sugar, and the full type as understood by the PureScript compiler is as follows:

```haskell
fullName :: forall r. Record (firstName :: String, lastName :: String | r) -> String
```

Note that the curly braces have been removed, and there is an extra `Record` constructor. `Record` is a built-in type constructor defined in the `Prim` module. If we find its kind, we see the following:

```text
> :kind Record
# Type -> Type
```

That is, `Record` is a type constructor which takes a _row of types_ and constructs a type. This is what allows us to write row-polymorphic functions on records.

The type system uses the same machinery to handle extensible effects as is used for row-polymorphic records (or _extensible records_). The only difference is the _kind_ of the types appearing in the labels. Records are parameterized by a row of types, and `Eff` is parameterized by a row of effects.

The same type system feature could even be used to build other types which were parameterized on rows of type constructors, or even rows of other rows!

## Fine-Grained Effects

Type annotations are usually not required when using `Eff`, since rows of effects can be inferred, but they can be used to indicate to the compiler which effects are expected in a computation.

If we annotate the previous example with a _closed_ row of effects:

``` haskell
main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  n <- random
  print n
```

(note the lack of the row variable `eff` here), then we cannot accidentally include a subcomputation which makes use of a different type of effect. In this way, we can control the side-effects that our code is allowed to have.

## Handlers and Actions

Functions such as `print` and `random` are called _actions_. Actions have the `Eff` type on the right hand side of their functions, and their purpose is to _introduce_ new effects.

This is in contrast to _handlers_, in which the `Eff` type appears as the type of a function argument. While actions _add_ to the set of required effects, a handler usually _subtracts_ effects from the set.

As an example, consider the `purescript-exceptions` package. It defines two functions, `throwException` and `catchException`:

```haskell
throwException :: forall a eff
                . Error
               -> Eff (exception :: EXCEPTION | eff) a

catchException :: forall a eff
                . (Error -> Eff eff a)
               -> Eff (exception :: EXCEPTION | eff) a
               -> Eff eff a
```

`throwException` is an action. `Eff` appears on the right hand side, and introduces the new `EXCEPTION` effect.

`catchException` is a handler. `Eff` appears as the type of the second function argument, and the overall effect is to _remove_ the `EXCEPTION` effect.

This is useful, because the type system can be used to delimit portions of code which require a particular effect. That code can then be wrapped in a handler, allowing it to be embedded inside a block of code which does not allow that effect.

For example, we can write a piece of code which throws exceptions using the `Exception` effect, then wrap that code using `catchException` to embed the computation in a piece of code which does not allow exceptions.

Suppose we wanted to read our application's configuration from a JSON document. The process of parsing the document might result in an exception. The process of reading and parsing the configuration could be written as a function with this type signature:

``` haskell
readConfig :: forall eff. Eff (exception :: EXCEPTION | eff) Config
```

Then, in the `main` function, we could use `catchException` to handle the `EXCEPTION` effect, logging the error and returning a default configuration:

```haskell
main = do
    config <- catchException printException readConfig
    runApplication config
  where
    printException e = do
      log (message e)
      pure defaultConfig
```

The `purescript-eff` package also defines the `runPure` handler, which takes a computation with _no_ side-effects, and safely evaluates it as a pure value:

```haskell
type Pure a = Eff () a

runPure :: forall a. Pure a -> a
```

## Mutable State

There is another effect defined in the core libraries: the `ST` effect.

The `ST` effect is used to manipulate mutable state. As pure functional programmers, we know that shared mutable state can be problematic. However, the `ST` effect uses the type system to restrict sharing in such a way that only safe _local_ mutation is allowed.

The `ST` effect is defined in the `Control.Monad.ST` module. To see how it works, we need to look at the types of its actions:

```haskell
newSTRef :: forall a h eff. a -> Eff (st :: ST h | eff) (STRef h a)

readSTRef :: forall a h eff. STRef h a -> Eff (st :: ST h | eff) a

writeSTRef :: forall a h eff. STRef h a -> a -> Eff (st :: ST h | eff) a

modifySTRef :: forall a h eff. STRef h a -> (a -> a) -> Eff (st :: ST h | eff) a
```

`newSTRef` is used to create a new mutable reference cell of type `STRef h a`, which can be read using the `readSTRef` action, and modified using the `writeSTRef` and `modifySTRef` actions. The type `a` is the type of the value stored in the cell, and the type `h` is used to indicate a _memory region_ (or _heap_) in the type system.

Here is an example. Suppose we want to simulate the movement of a particle falling under gravity by iterating a simple update function over a large number of small time steps.

We can do this by creating a mutable reference cell to hold the position and velocity of the particle, and then using a for loop (using the `forE` action in `Control.Monad.Eff`) to update the value stored in that cell:

```haskell
import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef)

simulate :: forall eff h. Number -> Number -> Int -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    modifySTRef ref \o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }
    pure unit
  final <- readSTRef ref
  pure final.x
```

At the end of the computation, we read the final value of the reference cell, and return the position of the particle.

Note that even though this function uses mutable state, it is still a pure function, so long as the reference cell `ref` is not allowed to be used by other parts of the program. We will see that this is exactly what the `ST` effect disallows.

To run a computation with the `ST` effect, we have to use the `runST` function:

```haskell
runST :: forall a eff. (forall h. Eff (st :: ST h | eff) a) -> Eff eff a
```

The thing to notice here is that the region type `h` is quantified _inside the parentheses_ on the left of the function arrow. That means that whatever action we pass to `runST` has to work with _any region_ `h` whatsoever.

However, once a reference cell has been created by `newSTRef`, its region type is already fixed, so it would be a type error to try to use the reference cell outside the code delimited by `runST`.  This is what allows `runST` to safely remove the `ST` effect!

In fact, since `ST` is the only effect in our example, we can use `runST` in conjunction with `runPure` to turn `simulate` into a pure function:

```haskell
simulate' :: Number -> Number -> Number -> Number
simulate' x0 v0 time = runPure (runST (simulate x0 v0 time))
```

You can even try running this function in PSCi:

```text
> import Main

> simulate' 100.0 0.0 0.0
100.00

> simulate' 100.0 0.0 1.0
95.10

> simulate' 100.0 0.0 2.0
80.39

> simulate' 100.0 0.0 3.0
55.87

> simulate' 100.0 0.0 4.0
21.54
```

In fact, if we inline the definition of `simulate` at the call to `runST`, as follows:

```haskell
simulate :: Number -> Number -> Int -> Number
simulate x0 v0 time = runPure $ runST do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    modifySTRef ref \o ->  
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001  
      }
    pure unit  
  final <- readSTRef ref
  pure final.x
```

then the compiler will notice that the reference cell is not allowed to escape its scope, and can safely turn it into a `var`. Here is the generated JavaScript for the body of the call to `runST`:

```javascript
var ref = { x: x0, v: v0 };

Control_Monad_Eff.forE(0)(time * 1000 | 0)(function (i) {
  return function __do() {
    ref = (function (o) {
      return {
        v: o.v - 9.81 * 1.0e-3,
        x: o.x + o.v * 1.0e-3
      };
    })(ref);
    return Prelude.unit;
  };
})();

return ref.x;
```

The `ST` effect is a good way to generate short JavaScript when working with locally-scoped mutable state, especially when used together with actions like `forE`, `foreachE`, `whileE` and `untilE` which generate efficient loops in the `Eff` monad.

X> ## Exercises
X>
X> 1. (Medium) Rewrite the `safeDivide` function to throw an exception using `throwException` if the denominator is zero.
X> 1. (Difficult) The following is a simple way to estimate pi: randomly choose a large number `N` of points in the unit square, and count the number `n` which lie in the inscribed circle. An estimate for pi is `4n/N`. Use the `RANDOM` and `ST` effects with the `forE` function to write a function which estimates pi in this way.

## DOM Effects

In the final sections of this chapter, we will apply what we have learned about effects in the `Eff` monad to the problem of working with the DOM.

There are a number of PureScript packages for working directly with the DOM, or with open-source DOM libraries. For example:

- [`purescript-dom`](http://github.com/purescript-contrib/purescript-dom) is an extensive set of low-level bindings to the browser's DOM APIs.
- [`purescript-jquery`](http://github.com/paf31/purescript-jquery) is a set of bindings to the [jQuery](http://jquery.org) library.

There are also PureScript libraries which build abstractions on top of these libraries, such as

- [`purescript-thermite`](http://github.com/paf31/purescript-thermite), which builds on `purescript-react`, and
- [`purescript-halogen`](http://github.com/slamdata/purescript-halogen) which provides a type-safe set of abstractions on top of a custom virtual DOM library.

In this chapter, we will use the `purescript-react` library to add a user interface to our address book application, but the interested reader is encouraged to explore alternative approaches.

## An Address Book User Interface

Using the `purescript-react` library, we will define our application as a React _component_. React components describe HTML elements in code as pure data structures, which are then efficiently rendered to the DOM. In addition, components can respond to events like button clicks. The `purescript-react` library uses the `Eff` monad to describe how to handle these events.

A full tutorial for the React library is well beyond the scope of this chapter, but the reader is encouraged to consult its documentation where needed. For our purposes, React will provide a practical example of the `Eff` monad.

We are going to build a form which will allow a user to add a new entry into our address book. The form will contain text boxes for the various fields (first name, last name, city, state, etc.), and an area in which validation errors will be displayed. As the user types text into the text boxes, the validation errors will be updated.

To keep things simple, the form will have a fixed shape: the different phone number types (home, cell, work, other) will be expanded into separate text boxes.

The HTML file is essentially empty, except for the following line:

```html
<script type="text/javascript" src="../dist/Main.js"></script>
```

This line includes the JavaScript code which is generated by Pulp. We place it at the end of the file to ensure that the relevant elements are on the page before we try to access them. To rebuild the `Main.js` file, Pulp can be used with the `browserify` command. Make sure the `dist` directory exists first, and that you have installed React as an NPM dependency:

```text
$ npm install # Install React
$ mkdir dist/
$ pulp browserify --to dist/Main.js
```

The `Main` defines the `main` function, which creates the address book component, and renders it to the screen. The `main` function uses the `CONSOLE` and `DOM` effects only, as its type signature indicates:

```haskell
main :: Eff (console :: CONSOLE, dom :: DOM) Unit
```

First, `main` logs a status message to the console:

```haskell
main = void do
  log "Rendering address book component"
```

Later, `main` uses the DOM API to obtain a reference (`doc`) to the document body:

```haskell
  doc <- window >>= document
```

Note that this provides an example of interleaving effects: the `log` function uses the `CONSOLE` effect, and the `window` and `document` functions both use the `DOM` effect. The type of `main` indicates that it uses both effects.

`main` uses the `window` action to get a reference to the window object, and passes the result to the `document` function using `>>=`. `document` takes a window object and returns a reference to its document.

Note that, by the definition of do notation, we could have instead written this as follows:

```haskell
  w <- window
  doc <- document w
```

It is a matter of personal preference whether this is more or less readable. The first version is an example of _point-free_ form, since there are no function arguments named, unlike the second version which uses the name `w` for the window object.

The `Main` module defines an address book _component_, called `addressBook`. To understand its definition, we will need to first need to understand some concepts.

In order to create a React component, we must first create a React _class_, which acts like a template for a component. In `purescript-react`, we can create classes using the `createClass` function. `createClass` requires a _specification_ of our class, which is essentially a collection of `Eff` actions which are used to handle various parts of the component's lifecycle. The action we will be interested in is the `Render` action.

Here are the types of some relevant functions provided by the React library:

```haskell
createClass
  :: forall props state eff
   . ReactSpec props state eff
  -> ReactClass props

type Render props state eff
   = ReactThis props state
  -> Eff ( props :: ReactProps
         , refs :: ReactRefs Disallowed
         , state :: ReactState ReadOnly
         | eff
         ) ReactElement

spec
  :: forall props state eff
   . state
  -> Render props state eff
  -> ReactSpec props state eff
```

There are a few interesting things to note here:

- The `Render` type synonym is provided in order to simplify some type signatures, and it represents the rendering function for a component.
- A `Render` action takes a reference to the component (of type `ReactThis`), and returns a `ReactElement` in the `Eff` monad. A `ReactElement` is a data structure describing our intended state of the DOM after rendering.
- Every React component defines some type of state. The state can be changed in response to events like button clicks. In `purescript-react`, the initial state value is provided in the `spec` function.
- The effect row in the `Render` type uses some interesting effects to restrict access to the React component's state in certain functions. For example, during rendering, access to the "refs" object is `Disallowed`, and access to the component state is `ReadOnly`.

The `Main` module defines a type of states for the address book component, and an initial state:

```haskell
newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { person: examplePerson
  , errors: []
  }
```

The state contains a `Person` record (which we will make editable using form components), and a collection of errors (which will be populated using our existing validation code).

Now let's see the definition of our component:

```haskell
addressBook :: forall props. ReactClass props
```

As already indicated, `addressBook` will use `createClass` and `spec` to create a React class. To do so, it will provide our initial state value, and a `Render` action. However, what can we do in the `Render` action? To answer that, `purescript-react` provides some simple actions which can be used:

```haskell
readState
  :: forall props state access eff
   . ReactThis props state
  -> Eff ( state :: ReactState ( read :: Read
                               | access
                               )
         | eff
         ) state

writeState
  :: forall props state access eff
   . ReactThis props state
  -> state
  -> Eff ( state :: ReactState ( write :: Write
                               | access
                               )
         | eff
         ) state
```

The `readState` and `writeState` functions use extensible effects to ensure that we have access to the React state (via the `ReactState` effect), but note that read and write permissions are separated further, by parameterizing the `ReactState` effect on _another_ row!

This illustrates an interesting point about PureScript's row-based effects: effects appearing inside rows need not be simple singletons, but can have interesting structure, and this flexibility enables some useful restrictions at compile time. If the `purescript-react` library did not make this restriction then it would be possible to get exceptions at runtime if we tried to write the state in the `Render` action, for example. Instead, such mistakes are now caught at compile time.

Now we can read the definition of our `addressBook` component. It starts by reading the current component state:

```haskell
addressBook = createClass $ spec initialState \ctx -> do
  AppState { person: Person person@{ homeAddress: Address address }
           , errors
           } <- readState ctx
```

Note the following:

- The name `ctx` refers to the `ReactThis` reference, and can be used to read and write the state where appropriate.
- The record inside `AppState` is matched using a record binder, including a record pun for the _errors_ field. We explicitly name various parts of the state structure for convenience.

Recall that `Render` must return a `ReactElement` structure, representing the intended state of the DOM. The `Render` action is defined in terms of some helper functions. One such helper function is `renderValidationErrors`, which turns the `Errors` structure into an array of `ReactElement`s.

```haskell
renderValidationError :: String -> ReactElement
renderValidationError err = D.li' [ D.text err ]

renderValidationErrors :: Errors -> Array ReactElement
renderValidationErrors [] = []
renderValidationErrors xs =
  [ D.div [ P.className "alert alert-danger" ]
          [ D.ul' (map renderValidationError xs) ]
  ]
```

In `purescript-react`, `ReactElement`s are typically created by applying functions like `div`, which create single HTML elements. These functions usually take an array of attributes, and an array of child elements as arguments. However, names ending with a prime character (like `ul'` here) omit the attribute array, and use the default attributes instead.

Note that since we are simply manipulating regular data structures here, we can use functions like `map` to build up more interesting elements.

A second helper function is `formField`, which creates a `ReactElement` containing a text input for a single form field:

```haskell
formField
  :: String
  -> String
  -> String
  -> (String -> Person)
  -> ReactElement
formField name hint value update =
  D.div [ P.className "form-group" ]
        [ D.label [ P.className "col-sm-2 control-label" ]
                  [ D.text name ]
        , D.div [ P.className "col-sm-3" ]
                [ D.input [ P._type "text"
                          , P.className "form-control"
                          , P.placeholder hint
                          , P.value value
                          , P.onChange (updateAppState ctx update)
                          ] []
                ]
        ]
```

Again, note that we are composing more interesting elements from simpler elements, applying attributes to each element as we go. One attribute of note here is the `onChange` attribute applied to the `input` element. This is an _event handler_, and is used to update the component state when the user edits text in our text box. Our event handler is defined using a third helper function, `updateAppState`:

```haskell
updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Person)
  -> Event
  -> Eff ( console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
```

`updateAppState` takes a reference to the component in the form of our `ReactThis` value, a function to update the `Person` record, and the `Event` record we are responding to. First, it extracts the new value of the text box from the `change` event (using the `valueOf` helper function), and uses it to create a new `Person` state:

```haskell
  for_ (valueOf e) \s -> do
    let newPerson = update s
```

Then, it runs the validation function, and updates the component state (using `writeState`) accordingly:

```haskell
    log "Running validators"
    case validatePerson' newPerson of
      Left errors ->
        writeState ctx (AppState { person: newPerson
                                 , errors: errors
                                 })
      Right _ ->
        writeState ctx (AppState { person: newPerson
                                 , errors: []
                                 })
```

That covers the basics of our component implementation. However, you should read the source accompanying this chapter in order to get a full understanding of the way the component works.

Also try the user interface out by running `pulp browserify --to dist/Main.js` and then opening the `html/index.html` file in your web browser. You should be able to enter some values into the form fields and see the validation errors printed onto the page.

Obviously, this user interface can be improved in a number of ways. The exercises will explore some ways in which we can make the application more usable.

X> ## Exercises
X>
X> 1. (Easy) Modify the application to include a work phone number text box.
X> 1. (Medium) Instead of using a `ul` element to show the validation errors in a list, modify the code to create one `div` with the `alert` style for each error.
X> 1. (Difficult, Extended) One problem with this user interface is that the validation errors are not displayed next to the form fields they originated from. Modify the code to fix this problem.
X>   
X>   _Hint_: the error type returned by the validator should be extended to indicate which field caused the error. You might want to use the following modified `Errors` type:
X>   
X>   ```haskell
X>   data Field = FirstNameField
X>              | LastNameField
X>              | StreetField
X>              | CityField
X>              | StateField
X>              | PhoneField PhoneType
X>   
X>   data ValidationError = ValidationError String Field
X>   
X>   type Errors = Array ValidationError
X>   ```
X>
X>   You will need to write a function which extracts the validation error for a particular `Field` from the `Errors` structure.

## Conclusion

This chapter has covered a lot of ideas about handling side-effects in PureScript:

- We met the `Monad` type class, and its connection to do notation.
- We introduced the monad laws, and saw how they allow us to transform code written using do notation.
- We saw how monads can be used abstractly, to write code which works with different side-effects.
- We saw how monads are examples of applicative functors, how both allow us to compute with side-effects, and the differences between the two approaches.
- The concept of native effects was defined, and we met the `Eff` monad, which is used to handle native side-effects.
- We saw how the `Eff` monad supports extensible effects, and how multiple types of native effect can be interleaved into the same computation.
- We saw how effects and records are handled in the kind system, and the connection between extensible records and extensible effects.
- We used the `Eff` monad to handle a variety of effects: random number generation, exceptions, console IO, mutable state, and DOM manipulation using React.

The `Eff` monad is a fundamental tool in real-world PureScript code. It will be used in the rest of the book to handle side-effects in a number of other use-cases.
