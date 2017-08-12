# Monadic Adventures

## Chapter Goals

The goal of this chapter will be to learn about _monad transformers_, which provide a way to combine side-effects provided by different monads. The motivating example will be a text adventure game which can be played on the console in NodeJS. The various side-effects of the game (logging, state, and configuration) will all be provided by a monad transformer stack.

## Project Setup

This module's project introduces the following new Bower dependencies:

- `purescript-maps`, which provides a data type for immutable maps
- `purescript-sets`, which provides a data type for immutable sets
- `purescript-transformers`, which provides implementations of standard monad transformers
- `purescript-node-readline`, which provides FFI bindings to the [`readline`](http://nodejs.org/api/readline.html) interface provided by NodeJS
- `purescript-yargs`, which provides an applicative interface to the [`yargs`](https://www.npmjs.org/package/yargs) command line argument processing library

It is also necessary to install the `yargs` module using NPM:

```text
npm install
```

## How To Play The Game

To run the project, use `pulp run`

By default you will see a usage message:

```text
node ./dist/Main.js -p <player name>

Options:
  -p, --player  Player name  [required]
  -d, --debug   Use debug mode

Missing required arguments: p
The player name is required
```

Provide the player name using the `-p` option:

```text
pulp run -- -p Phil
>
```

From the prompt, you can enter commands like `look`, `inventory`, `take`, `use`, `north`, `south`, `east`, and `west`. There is also a `debug` command, which can be used to print the game state when the `--debug` command line option is provided.

The game is played on a two-dimensional grid, and the player moves by issuing commands `north`, `south`, `east`, and `west`. The game contains a collection of items which can either be in the player's possession (in the user's _inventory_), or on the game grid at some location. Items can be picked up by the player, using the `take` command.

For reference, here is a complete walkthrough of the game:

```text
$ pulp run -- -p Phil

> look
You are at (0, 0)
You are in a dark forest. You see a path to the north.
You can see the Matches.

> take Matches
You now have the Matches

> north
> look
You are at (0, 1)
You are in a clearing.
You can see the Candle.

> take Candle
You now have the Candle

> inventory
You have the Candle.
You have the Matches.

> use Matches
You light the candle.
Congratulations, Phil!
You win!
```

The game is very simple, but the aim of the chapter is to use the `purescript-transformers` package to build a library which will enable rapid development of this type of game.

## The State Monad

We will start by looking at some of the monads provided by the `purescript-transformers` package.

The first example is the `State` monad, which provides a way to model _mutable state_ in pure code. We have already seen two approaches to mutable state provided by the `Eff` monad, namely the `REF` and `ST` effects. `State` provides a third alternative, but it is not implemented using the `Eff` monad.

The `State` type constructor takes two type parameters: the type `s` of the state, and the return type `a`. Even though we speak of the "`State` monad", the instance of the `Monad` type class is actually provided for the `State s` type constructor, for any type `s`.

The `Control.Monad.State` module provides the following API:

```haskell
get    :: forall s.             State s s
put    :: forall s. s        -> State s Unit
modify :: forall s. (s -> s) -> State s Unit
```

This looks very similar to the API provided by the `REF` and `ST` effects. However, notice that we do not pass a mutable reference cell such as a `Ref` or `STRef` to the actions. The difference between `State` and the solutions provided by the `Eff` monad is that the `State` monad only supports a single piece of state which is implicit - the state is implemented as a function argument hidden by the `State` monad's data constructor, so there is no explicit reference to pass around.

Let's see an example. One use of the `State` monad might be to add the values in an array of numbers to the current state. We could do that by choosing `Number` as the state type `s`, and using `traverse_` to traverse the array, with a call to `modify` for each array element:

```haskell
import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n
```

The `Control.Monad.State` module provides three functions for running a computation in the `State` monad:

```haskell
evalState :: forall s a. State s a -> s -> a
execState :: forall s a. State s a -> s -> s
runState  :: forall s a. State s a -> s -> Tuple a s
```

Each of these functions takes an initial state of type `s` and a computation of type `State s a`. `evalState` only returns the return value, `execState` only returns the final state, and `runState` returns both, expressed as a value of type `Tuple a s`.

Given the `sumArray` function above, we could use `execState` in PSCi to sum the numbers in several arrays as follows:

```text
> :paste
… execState (do
…   sumArray [1, 2, 3]
…   sumArray [4, 5]
…   sumArray [6]) 0
… ^D
21
```

X> ## Exercises
X>
X> 1. (Easy) What is the result of replacing `execState` with `runState` or `evalState` in our example above?
X> 1. (Medium) A string of parentheses is _balanced_ if it is obtained by either concatenating zero-or-more shorter balanced
X>     strings, or by wrapping a shorter balanced string in a pair of parentheses.
X>     
X>     Use the `State` monad and the `traverse_` function to write a function
X>
X>     ```haskell
X>     testParens :: String -> Boolean
X>     ```
X>
X>     which tests whether or not a `String` of parentheses is balanced, by keeping track of the number of opening parentheses
X>     which have not been closed. Your function should work as follows:
X>
X>     ```text
X>     > testParens ""
X>     true
X>     
X>     > testParens "(()(())())"
X>     true
X>     
X>     > testParens ")"
X>     false
X>     
X>     > testParens "(()()"
X>     false
X>     ```
X>
X>     _Hint_: you may like to use the `toCharArray` function from the `Data.String` module to turn the input string into an array of characters.

## The Reader Monad

Another monad provided by the `purescript-transformers` package is the `Reader` monad. This monad provides the ability to read from a global configuration. Whereas the `State` monad provides the ability to read and write a single piece of mutable state, the `Reader` monad only provides the ability to read a single piece of data.

The `Reader` type constructor takes two type arguments: a type `r` which represents the configuration type, and the return type `a`.

The `Control.Monad.Reader` module provides the following API:

```haskell
ask   :: forall r. Reader r r
local :: forall r a. (r -> r) -> Reader r a -> Reader r a
```

The `ask` action can be used to read the current configuration, and the `local` action can be used to run a computation with a modified configuration.

For example, suppose we were developing an application controlled by permissions, and we wanted to use the `Reader` monad to hold the current user's permissions object. We might choose the type `r` to be some type `Permissions` with the following API:

```haskell
hasPermission :: String -> Permissions -> Boolean
addPermission :: String -> Permissions -> Permissions
```

Whenever we wanted to check if the user had a particular permission, we could use `ask` to retrieve the current permissions object. For example, only administrators might be allowed to create new users:

```haskell
createUser :: Reader Permissions (Maybe User)
createUser = do
  permissions <- ask
  if hasPermission "admin" permissions
    then map Just newUser
    else pure Nothing
```

To elevate the user's permissions, we might use the `local` action to modify the `Permissions` object during the execution of some computation:

```haskell
runAsAdmin :: forall a. Reader Permissions a -> Reader Permissions a
runAsAdmin = local (addPermission "admin")
```

Then we could write a function to create a new user, even if the user did not have the `admin` permission:

```haskell
createUserAsAdmin :: Reader Permissions (Maybe User)
createUserAsAdmin = runAsAdmin createUser
```

To run a computation in the `Reader` monad, the `runReader` function can be used to provide the global configuration:

```haskell
runReader :: forall r a. Reader r a -> r -> a
```

X> ## Exercises
X>
X> In these exercises, we will use the `Reader` monad to build a small library for rendering documents with indentation. The "global configuration" will be a number indicating the current indentation level:
X>
X>    ```haskell
X>    type Level = Int
X>    
X>    type Doc = Reader Level String
X>    ```
X>
X> 1. (Easy) Write a function `line` which renders a function at the current indentation level. Your function should have the following type:
X>
X>     ```haskell
X>     line :: String -> Doc
X>     ```
X>
X>     _Hint_: use the `ask` function to read the current indentation level.
X> 1. (Easy) Use the `local` function to write a function
X>
X>     ```haskell
X>     indent :: Doc -> Doc
X>     ```
X>
X>     which increases the indentation level for a block of code.
X> 1. (Medium) Use the `sequence` function defined in `Data.Traversable` to write a function
X>
X>     ```haskell
X>     cat :: Array Doc -> Doc
X>     ```
X>
X>     which concatenates a collection of documents, separating them with new lines.
X> 1. (Medium) Use the `runReader` function to write a function
X>
X>     ```haskell
X>     render :: Doc -> String
X>     ```
X>
X>     which renders a document as a String.
X>
X> You should now be able to use your library to write simple documents, as follows:
X>
X> ```haskell
X> render $ cat
X>   [ line "Here is some indented text:"
X>   , indent $ cat
X>       [ line "I am indented"
X>       , line "So am I"
X>       , indent $ line "I am even more indented"
X>       ]
X>   ]
X> ```

## The Writer Monad

The `Writer` monad provides the ability to accumulate a secondary value in addition to the return value of a computation.

A common use case is to accumulate a log of type `String` or `Array String`, but the `Writer` monad is more general than this. It can actually be used to accumulate a value in any monoid, so it might be used to keep track of an integer total using the `Additive Int` monoid, or to track whether any of several intermediate `Boolean` values were true, using the `Disj Boolean` monoid.

The `Writer` type constructor takes two type arguments: a type `w` which should be an instance of the `Monoid` type class, and the return type `a`.

The key element of the `Writer` API is the `tell` function:

```haskell
tell :: forall w a. Monoid w => w -> Writer w Unit
```

The `tell` action appends the provided value to the current accumulated result.

As an example, let's add a log to an existing function by using the `Array String` monoid. Consider our previous implementation of the _greatest common divisor_ function:

```haskell
gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)
```

We could add a log to this function by changing the return type to `Writer (Array String) Int`:

```haskell
import Control.Monad.Writer
import Control.Monad.Writer.Class

gcdLog :: Int -> Int -> Writer (Array String) Int
```

We only have to change our function slightly to log the two inputs at each step:

```haskell
    gcdLog n 0 = pure n
    gcdLog 0 m = pure m
    gcdLog n m = do
      tell ["gcdLog " <> show n <> " " <> show m]
      if n > m
        then gcdLog (n - m) m
        else gcdLog n (m - n)
```

We can run a computation in the `Writer` monad by using either of the `execWriter` or `runWriter` functions:

```haskell
execWriter :: forall w a. Writer w a -> w
runWriter  :: forall w a. Writer w a -> Tuple a w
```

Just like in the case of the `State` monad, `execWriter` only returns the accumulated log, whereas `runWriter` returns both the log and the result.

We can test our modified function in PSCi:

```text
> import Control.Monad.Writer
> import Control.Monad.Writer.Class

> runWriter (gcdLog 21 15)
Tuple 3 ["gcdLog 21 15","gcdLog 6 15","gcdLog 6 9","gcdLog 6 3","gcdLog 3 3"]
```

X> ## Exercises
X>
X> 1. (Medium) Rewrite the `sumArray` function above using the `Writer` monad and the `Additive Int` monoid from the `purescript-monoid` package.
X> 1. (Medium) The _Collatz_ function is defined on natural numbers `n` as `n / 2` when `n` is even, and `3 * n + 1` when `n` is odd. For example, the iterated Collatz sequence starting at `10` is as follows:
X>
X>     ```text
X>     10, 5, 16, 8, 4, 2, 1, ...
X>     ```
X>
X>     It is conjectured that the iterated Collatz sequence always reaches `1` after some finite number of applications of the Collatz function.
X>
X>     Write a function which uses recursion to calculate how many iterations of the Collatz function are required before the sequence reaches `1`.
X>
X>     Modify your function to use the `Writer` monad to log each application of the Collatz function.

## Monad Transformers

Each of the three monads above: `State`, `Reader` and `Writer`, are also examples of so-called _monad transformers_. The equivalent monad transformers are called `StateT`, `ReaderT`, and `WriterT` respectively.

What is a monad transformer? Well, as we have seen, a monad augments PureScript code with some type of side effect, which can be interpreted in PureScript by using the appropriate handler (`runState`, `runReader`, `runWriter`, etc.) This is fine if we only need to use _one_ side-effect. However, it is often useful to use more than one side-effect at once. For example, we might want to use `Reader` together with `Maybe` to express _optional results_ in the context of some global configuration. Or we might want the mutable state provided by the `State` monad together with the pure error tracking capability of the `Either` monad. This is the problem solved by _monad transformers_.

Note that we have already seen that the `Eff` monad provides a partial solution to this problem, since native effects can be interleaved using the approach of _extensible effects_. Monad transformers provide another solution, and each approach has its own benefits and limitations.

A monad transformer is a type constructor which is parameterized not only by a type, but by another type constructor. It takes one monad and turns it into another monad, adding its own variety of side-effects.

Let's see an example. The monad transformer version of the `State` monad is `StateT`, defined in the `Control.Monad.State.Trans` module. We can find the kind of `StateT` using PSCi:

```text
> import Control.Monad.State.Trans
> :kind StateT
Type -> (Type -> Type) -> Type -> Type
```

This looks quite confusing, but we can apply `StateT` one argument at a time to understand how to use it.

The first type argument is the type of the state we wish to use, as was the case for `State`. Let's use a state of type `String`:

```text
> :kind StateT String
(Type -> Type) -> Type -> Type
```

The next argument is a type constructor of kind `Type -> Type`. It represents the underlying monad, which we want to add the effects of `StateT` to. For the sake of an example, let's choose the `Either String` monad:

```text
> :kind StateT String (Either String)
Type -> Type
```

We are left with a type constructor. The final argument represents the return type, and we might instantiate it to `Number` for example:

```text
> :kind StateT String (Either String) Number
Type
```

Finally we are left with something of kind `Type`, which means we can try to find values of this type.

The monad we have constructed - `StateT String (Either String)` - represents computations which can fail with an error, and which can use mutable state.

We can use the actions of the outer `StateT String` monad (`get`, `put`, and `modify`) directly, but in order to use the effects of the wrapped monad (`Either String`), we need to "lift" them over the monad transformer. The `Control.Monad.Trans` module defines the `MonadTrans` type class, which captures those type constructors which are monad transformers, as follows:

```haskell
class MonadTrans t where
  lift :: forall m a. Monad m => m a -> t m a
```

This class contains a single member, `lift`, which takes computations in any underlying monad `m` and lifts them into the wrapped monad `t m`. In our case, the type constructor `t` is `StateT String`, and `m` is the `Either String` monad, so `lift` provides a way to lift computations of type `Either String a` to computations of type `StateT String (Either String) a`. This means that we can use the effects of `StateT String` and `Either String` side-by-side, as long as we use `lift` every time we use a computation of type `Either String a`.

For example, the following computation reads the underlying state, and then throws an error if the state is the empty string:

```haskell
import Data.String (drop, take)

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty string"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)
```

If the state is not empty, the computation uses `put` to update the state to `drop 1 s` (that is, `s` with the first character removed), and returns `take 1 s` (that is, the first character of `s`).

Let's try this in PSCi:

```text
> runStateT split "test"
Right (Tuple "t" "est")

> runStateT split ""
Left "Empty string"
```

This is not very remarkable, since we could have implemented this without `StateT`. However, since we are working in a monad, we can use do notation or applicative combinators to build larger computations from smaller ones. For example, we can apply `split` twice to read the first two characters from a string:

```text
> runStateT ((<>) <$> split <*> split) "test"
(Right (Tuple "te" "st"))
```

We can use the `split` function with a handful of other actions to build a basic parsing library. In fact, this is the approach taken by the `purescript-parsing` library. This is the power of monad transformers - we can create custom-built monads for a variety of problems, choosing the side-effects that we need, and keeping the expressiveness of do notation and applicative combinators.

## The ExceptT Monad Transformer

The `purescript-transformers` package also defines the `ExceptT e` monad transformer, which is the transformer corresponding to the `Either e` monad. It provides the following API:

```haskell
class MonadError e m where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a

instance monadErrorExceptT :: Monad m => MonadError e (ExceptT e m)

runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)
```

The `MonadError` class captures those monads which support throwing and catching of errors of some type `e`, and an instance is provided for the `ExceptT e` monad transformer. The `throwError` action can be used to indicate failure, just like `Left` in the `Either e` monad. The `catchError` action allows us to continue after an error is thrown using `throwError`.

The `runExceptT` handler is used to run a computation of type `ExceptT e m a`.

This API is similar to that provided by the `purescript-exceptions` package and the `Exception` effect. However, there are some important differences:

- `Exception` uses actual JavaScript exceptions, whereas `ExceptT` models errors as a pure data structure.
- The `Exception` effect only supports exceptions of one type, namely JavaScript's `Error` type, whereas `ExceptT` supports errors of type. In particular, we are free to define new error types.

Let's try out `ExceptT` by using it to wrap the `Writer` monad. Again, we are free to use actions from the monad transformer `ExceptT e` directly, but computations in the `Writer` monad should be lifted using `lift`:

```haskell
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Except.Trans

writerAndExceptT :: ExceptT String (Writer (Array String)) String
writerAndExceptT = do
  lift $ tell ["Before the error"]
  throwError "Error!"
  lift $ tell ["After the error"]
  pure "Return value"
```

If we test this function in PSCi, we can see how the two effects of accumulating a log and throwing an error interact. First, we can run the outer `ExceptT` computation of type by using `runExceptT`, leaving a result of type `Writer String (Either String String)`. We can then use `runWriter` to run the inner `Writer` computation:

```text
> runWriter $ runExceptT writerAndExceptT
Tuple (Left "Error!") ["Before the error"]
```

Note that only those log messages which were written before the error was thrown actually get appended to the log.

## Monad Transformer Stacks

As we have seen, monad transformers can be used to build new monads on top of existing monads. For some monad transformer `t1` and some monad `m`, the application `t1 m` is also a monad. That means that we can apply a _second_ monad transformer `t2` to the result `t1 m` to construct a third monad `t2 (t1 m)`. In this way, we can construct a _stack_ of monad transformers, which combine the side-effects provided by their constituent monads.

In practice, the underlying monad `m` is either the `Eff` monad, if native side-effects are required, or the `Identity` monad, defined in the `Data.Identity` module. The `Identity` monad adds no new side-effects, so transforming the `Identity` monad only provides the effects of the monad transformer. In fact, the `State`, `Reader` and `Writer` monads are implemented by transforming the `Identity` monad with `StateT`, `ReaderT` and `WriterT` respectively.

Let's see an example in which three side effects are combined. We will use the `StateT`, `WriterT` and `ExceptT` effects, with the `Identity` monad on the bottom of the stack. This monad transformer stack will provide the side effects of mutable state, accumulating a log, and pure errors.

We can use this monad transformer stack to reproduce our `split` action with the added feature of logging.

```haskell
type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)
```

If we test this computation in PSCi, we see that the state is appended to the log for every invocation of `split`.

Note that we have to remove the side-effects in the order in which they appear in the monad transformer stack: first we use `runStateT` to remove the `StateT` type constructor, then `runWriterT`, then `runExceptT`. Finally, we run the computation in the `Identity` monad by using `runIdentity`.

```text
> runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

> runParser split "test"
(Right (Tuple (Tuple "t" "est") ["The state is test"]))

> runParser ((<>) <$> split <*> split) "test"
(Right (Tuple (Tuple "te" "st") ["The state is test", "The state is est"]))
```

However, if the parse is unsuccessful because the state is empty, then no log is printed at all:

```text
> runParser split ""
(Left ["Empty string"])
```

This is because of the way in which the side-effects provided by the `ExceptT` monad transformer interact with the side-effects provided by the `WriterT` monad transformer. We can address this by changing the order in which the monad transformer stack is composed. If we move the `ExceptT` transformer to the top of the stack, then the log will contain all messages written up until the first error, as we saw earlier when we transformed `Writer` with `ExceptT`.

One problem with this code is that we have to use the `lift` function multiple times to lift computations over multiple monad transformers: for example, the call to `throwError` has to be lifted twice, once over `WriterT` and a second time over `StateT`. This is fine for small monad transformer stacks, but quickly becomes inconvenient.

Fortunately, as we will see, we can use the automatic code generation provided by type class inference to do most of this "heavy lifting" for us.

X> ## Exercises
X>
X> 1. (Easy) Use the `ExceptT` monad transformer over the `Identity` functor to write a function `safeDivide` which divides two numbers, throwing an error if the denominator is zero.
X> 1. (Medium) Write a parser
X>
X>     ```haskell
X>     string :: String -> Parser String
X>     ```
X>
X>     which matches a string as a prefix of the current state, or fails with an error message.
X>
X>     Your parser should work as follows:
X>
X>     ```text
X>     > runParser (string "abc") "abcdef"
X>     (Right (Tuple (Tuple "abc" "def") ["The state is abcdef"]))
X>     ```
X>
X>     _Hint_: you can use the implementation of `split` as a starting point. You might find the `stripPrefix` function useful.
X> 1. (Difficult) Use the `ReaderT` and `WriterT` monad transformers to reimplement the document printing library which we wrote earlier using the `Reader` monad.
X>
X>     Instead of using `line` to emit strings and `cat` to concatenate strings, use the `Array String` monoid with the `WriterT` monad transformer, and `tell` to append a line to the result.

## Type Classes to the Rescue!

When we looked at the `State` monad at the start of this chapter, I gave the following types for the actions of the `State` monad:

```haskell
get    :: forall s.             State s s
put    :: forall s. s        -> State s Unit
modify :: forall s. (s -> s) -> State s Unit
```

In reality, the types given in the `Control.Monad.State.Class` module are more general than this:

```haskell
get    :: forall m s. MonadState s m =>             m s
put    :: forall m s. MonadState s m => s        -> m Unit
modify :: forall m s. MonadState s m => (s -> s) -> m Unit
```

The `Control.Monad.State.Class` module defines the `MonadState` (multi-parameter) type class, which allows us to abstract over "monads which support pure mutable state". As one would expect, the `State s` type constructor is an instance of the `MonadState s` type class, but there are many more interesting instances of this class.

In particular, there are instances of `MonadState` for the `WriterT`, `ReaderT` and `ExceptT` monad transformers, provided in the `purescript-transformers` package. Each of these monad transformers has an instance for `MonadState` whenever the underlying `Monad` does. In practice, this means that as long as `StateT` appears _somewhere_ in the monad transformer stack, and everything above `StateT` is an instance of `MonadState`, then we are free to use `get`, `put` and `modify` directly, without the need to use `lift`.

Indeed, the same is true of the actions we covered for the `ReaderT`, `WriterT`, and `ExceptT` transformers. `purescript-transformers` defines a type class for each of the major transformers, allowing us to abstract over monads which support their operations.

In the case of the `split` function above, the monad stack we constructed is an instance of each of the `MonadState`, `MonadWriter` and `MonadError` type classes. This means that we don't need to call `lift` at all! We can just use the actions `get`, `put`, `tell` and `throwError` as if they were defined on the monad stack itself:

```haskell
split :: Parser String
split = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> throwError "Empty string"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)
```

This computation really looks like we have extended our programming language to support the three new side-effects of mutable state, logging and error handling. However, everything is still implemented using pure functions and immutable data under the hood.

## Alternatives

The `purescript-control` package defines a number of abstractions for working with computations which can fail. One of these is the `Alternative` type class:

```haskell
class Functor f <= Alt f where
  alt :: forall a. f a -> f a -> f a

class Alt f <= Plus f where
  empty :: forall a. f a

class (Applicative f, Plus f) <= Alternative f
```

`Alternative` provides two new combinators: the `empty` value, which provides a prototype for a failing computation, and the `alt` function (and its alias, `<|>`) which provides the ability to fall back to an _alternative_ computation in the case of an error.

The `Data.List` module provides two useful functions for working with type constructors in the `Alternative` type class:

```haskell
many :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
some :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
```

The `many` combinator uses the `Alternative` type class to repeatedly run a computation _zero-or-more_ times. The `some` combinator is similar, but requires at least the first computation to succeed.

In the case of our `Parser` monad transformer stack, there is an instance of `Alternative` induced by the `ExceptT` component, which supports failure by composing errors in different branches using a `Monoid` instance (this is why we chose `Array String` for our `Errors` type). This means that we can use the `many` and `some` functions to run a parser multiple times:

```text
> import Split
> import Control.Alternative

> runParser (many split) "test"
(Right (Tuple (Tuple ["t", "e", "s", "t"] "")
              [ "The state is \"test\""
              , "The state is \"est\""
              , "The state is \"st\""
              , "The state is \"t\""
              ]))
```

Here, the input string `"test"` has been repeatedly split to return an array of four single-character strings, the leftover state is empty, and the log shows that we applied the `split` combinator four times.

Other examples of `Alternative` type constructors are `Maybe` and `Array`.

## Monad Comprehensions

The `Control.MonadPlus` module defines a subclass of the `Alternative` type class, called `MonadPlus`. `MonadPlus` captures those type constructors which are both monads and instances of `Alternative`:

```haskell
class (Monad m, Alternative m) <= MonadZero m

class MonadZero m <= MonadPlus m
```

In particular, our `Parser` monad is an instance of `MonadPlus`.

When we covered array comprehensions earlier in the book, we introduced the `guard` function, which could be used to filter out unwanted results. In fact, the `guard` function is more general, and can be used for any monad which is an instance of `MonadPlus`:

```haskell
guard :: forall m. MonadZero m => Boolean -> m Unit
```

The `<|>` operator allows us to backtrack in case of failure. To see how this is useful, let's define a variant of the `split` combinator which only matches upper case characters:

```haskell
upper :: Parser String
upper = do
  s <- split
  guard $ toUpper s == s
  pure s
```

Here, we use a `guard` to fail if the string is not upper case. Note that this code looks very similar to the array comprehensions we saw earlier - using `MonadPlus` in this way, we sometimes refer to constructing _monad comprehensions_.

## Backtracking

We can use the `<|>` operator to backtrack to another alternative in case of failure. To demonstrate this, let's define one more parser, which matches lower case characters:

```haskell
lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  pure s
```

With this, we can define a parser which eagerly matches many upper case characters if the first character is upper case, or many lower case character if the first character is lower case:

```text
> upperOrLower = some upper <|> some lower
```

This parser will match characters until the case changes:

```text
> runParser upperOrLower "abcDEF"
(Right (Tuple (Tuple ["a","b","c"] ("DEF"))
              [ "The state is \"abcDEF\""
              , "The state is \"bcDEF\""
              , "The state is \"cDEF\""
              ]))
```

We can even use `many` to fully split a string into its lower and upper case components:

```text
> components = many upperOrLower

> runParser components "abCDeFgh"
(Right (Tuple (Tuple [["a","b"],["C","D"],["e"],["F"],["g","h"]] "")
              [ "The state is \"abCDeFgh\""
              , "The state is \"bCDeFgh\""
              , "The state is \"CDeFgh\""
              , "The state is \"DeFgh\""
              , "The state is \"eFgh\""
              , "The state is \"Fgh\""
              , "The state is \"gh\""
              , "The state is \"h\""
              ]))
```

Again, this illustrates the power of reusability that monad transformers bring - we were able to write a backtracking parser in a declarative style with only a few lines of code, by reusing standard abstractions!

X> ## Exercises
X>
X> 1. (Easy) Remove the calls to the `lift` function from your implementation of the `string` parser. Verify that the new implementation type checks, and convince yourself that it should.
X> 1. (Medium) Use your `string` parser with the `many` combinator to write a parser which recognizes strings consisting of several copies of the string `"a"` followed by several copies of the string `"b"`.
X> 1. (Medium) Use the `<|>` operator to write a parser which recognizes strings of the letters `a` or `b` in any order.
X> 1. (Difficult) The `Parser` monad might also be defined as follows:
X>
X>     ```haskell
X>     type Parser = ExceptT Errors (StateT String (WriterT Log Identity))
X>     ```
X>
X>     What effect does this change have on our parsing functions?

## The RWS Monad

One particular combination of monad transformers is so common that it is provided as a single monad transformer in the `purescript-transformers` package. The `Reader`, `Writer` and `State` monads are combined into the _reader-writer-state_ monad, or more simply the `RWS` monad. This monad has a corresponding monad transformer called the `RWST` monad transformer.

We will use the `RWS` monad to model the game logic for our text adventure game.

The `RWS` monad is defined in terms of three type parameters (in addition to its return type):

```haskell
type RWS r w s = RWST r w s Identity
```

Notice that the `RWS` monad is defined in terms of its own monad transformer, by setting the base monad to `Identity` which provides no side-effects.

The first type parameter, `r`, represents the global configuration type. The second, `w`, represents the monoid which we will use to accumulate a log, and the third, `s` is the type of our mutable state.

In the case of our game, our global configuration is defined in a type called `GameEnvironment` in the `Data.GameEnvironment` module:

```haskell
type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName
  , debugMode     :: Boolean
  }
```

It defines the player name, and a flag which indicates whether or not the game is running in debug mode. These options will be set from the command line when we come to run our monad transformer.

The mutable state is defined in a type called `GameState` in the `Data.GameState` module:

```haskell
import qualified Data.Map as M
import qualified Data.Set as S

newtype GameState = GameState
  { items       :: M.Map Coords (S.Set GameItem)
  , player      :: Coords
  , inventory   :: S.Set GameItem
  }
```

The `Coords` data type represents points on a two-dimensional grid, and the `GameItem` data type is an enumeration of the items in the game:

```haskell
data GameItem = Candle | Matches
```

The `GameState` type uses two new data structures: `Map` and `Set`, which represent sorted maps and sorted sets respectively. The `items` property is a mapping from coordinates of the game grid to sets of game items at that location. The `player` property stores the current coordinates of the player, and the `inventory` property stores a set of game items currently held by the player.

The `Map` and `Set` data structures are sorted by their keys, can be used with any key type in the `Ord` type class. This means that the keys in our data structures should be totally ordered.

We will see how the `Map` and `Set` structures are used as we write the actions for our game.

For our log, we will use the `List String` monoid. We can define a type synonym for our `Game` monad, implemented using `RWS`:

```haskell
type Log = L.List String

type Game = RWS GameEnvironment Log GameState
```

## Implementing Game Logic

Our game is going to be built from simple actions defined in the `Game` monad, by reusing the actions from the `Reader`, `Writer` and `State` monads. At the top level of our application, we will run the pure computations in the `Game` monad, and use the `Eff` monad to turn the results into observable side-effects, such as printing text to the console.

One of the simplest actions in our game is the `has` action. This action tests whether the player's inventory contains a particular game item. It is defined as follows:

```haskell
has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  pure $ item `S.member` state.inventory
```

This function uses the `get` action defined in the `MonadState` type class to read the current game state, and then uses the `member` function defined in `Data.Set` to test whether the specified `GameItem` appears in the `Set` of inventory items.

Another action is the `pickUp` action. It adds a game item to the player's inventory if it appears in the current room. It uses actions from the `MonadWriter` and `MonadState` type classes. First of all, it reads the current game state:

```haskell
pickUp :: GameItem -> Game Unit
pickUp item = do
  GameState state <- get
```

Next, `pickUp` looks up the set of items in the current room. It does this by using the `lookup` function defined in `Data.Map`:

```haskell
  case state.player `M.lookup` state.items of
```

The `lookup` function returns an optional result indicated by the `Maybe` type constructor. If the key does not appear in the map, the `lookup` function returns `Nothing`, otherwise it returns the corresponding value in the `Just` constructor.

We are interested in the case where the corresponding item set contains the specified game item. Again we can test this using the `member` function:

```haskell
    Just items | item `S.member` items -> do
```

In this case, we can use `put` to update the game state, and `tell` to add a message to the log:

```haskell
      let newItems = M.update (Just <<< S.delete item) state.player state.items
          newInventory = S.insert item state.inventory
      put $ GameState state { items     = newItems
                            , inventory = newInventory
                            }
      tell (L.singleton ("You now have the " <> show item))
```

Note that there is no need to `lift` either of the two computations here, because there are appropriate instances for both `MonadState` and `MonadWriter` for our `Game` monad transformer stack.

The argument to `put` uses a record update to modify the game state's `items` and `inventory` fields. We use the `update` function from `Data.Map` which modifies a value at a particular key. In this case, we modify the set of items at the player's current location, using the `delete` function to remove the specified item from the set. `inventory` is also updated, using `insert` to add the new item to the player's inventory set.

Finally, the `pickUp` function handles the remaining cases, by notifying the user using `tell`:

```haskell
    _ -> tell (L.singleton "I don't see that item here.")
```

As an example of using the `Reader` monad, we can look at the code for the `debug` command. This command allows the user to inspect the game state at runtime if the game is running in debug mode:

```haskell
  GameEnvironment env <- ask
  if env.debugMode
    then do
      state <- get
      tell (L.singleton (show state))
    else tell (L.singleton "Not running in debug mode.")
```

Here, we use the `ask` action to read the game configuration. Again, note that we don't need to `lift` any computation, and we can use actions defined in the `MonadState`, `MonadReader` and `MonadWriter` type classes in the same do notation block.

If the `debugMode` flag is set, then the `tell` action is used to write the state to the log. Otherwise, an error message is added.

The remainder of the `Game` module defines a set of similar actions, each using only the actions defined by the `MonadState`, `MonadReader` and `MonadWriter` type classes.

## Running the Computation

Since our game logic runs in the `RWS` monad, it is necessary to run the computation in order to respond to the user's commands.

The front-end of our game is built using two packages: `purescript-yargs`, which provides an applicative interface to the `yargs` command line parsing library, and `purescript-node-readline`, which wraps NodeJS' `readline` module, allowing us to write interactive console-based applications.

The interface to our game logic is provided by the function `game` in the `Game` module:

```haskell
game :: Array String -> Game Unit
```

To run this computation, we pass a list of words entered by the user as an array of strings, and run the resulting `RWS` computation using `runRWS`:

```haskell
data RWSResult state result writer = RWSResult state result writer

runRWS :: forall r w s a. RWS r w s a -> r -> s -> RWSResult s a w
```

`runRWS` looks like a combination of `runReader`, `runWriter` and `runState`. It takes a global configuration and an initial state as an argument, and returns a data structure containing the log, the result and the final state.

The front-end of our application is defined by a function `runGame`, with the following type signature:

```haskell
runGame
  :: forall eff
   . GameEnvironment
  -> Eff ( exception :: EXCEPTION
         , readline :: RL.READLINE
         , console :: CONSOLE
         | eff
         ) Unit
```

The `CONSOLE` effect indicates that this function interacts with the user via the console (using the `purescript-node-readline` and `purescript-console` packages). `runGame` takes the game configuration as a function argument.

The `purescript-node-readline` package provides the `LineHandler` type, which represents actions in the `Eff` monad which handle user input from the terminal. Here is the corresponding API:

```haskell
type LineHandler eff a = String -> Eff eff a

setLineHandler
  :: forall eff a
   . Interface
  -> LineHandler (readline :: READLINE | eff) a
  -> Eff (readline :: READLINE | eff) Unit
```

The `Interface` type represents a handle for the console, and is passed as an argument to the functions which interact with it. An `Interface` can be created using the `createConsoleInterface` function:

```haskell
runGame env = do
  interface <- createConsoleInterface noCompletion
```

The first step is to set the prompt at the console. We pass the `interface` handle, and provide the prompt string and indentation level:

```haskell
  setPrompt "> " 2 interface
```

In our case, we are interested in implementing the line handler function. Our line handler is defined using a helper function in a `let` declaration, as follows:

```haskell
lineHandler
  :: GameState
  -> String
  -> Eff ( exception :: EXCEPTION
         , console :: CONSOLE
         , readline :: RL.READLINE
         | eff
         ) Unit
lineHandler currentState input = do
  case runRWS (game (split " " input)) env currentState of
    RWSResult state _ written -> do
      for_ written log
      setLineHandler interface $ lineHandler state
  prompt interface
  pure unit
```

The let binding is closed over both the game configuration, named `env`, and the console handle, named `interface`.

Our handler takes an additional first argument, the game state. This is required since we need to pass the game state to `runRWS` to run the game's logic.

The first thing this action does is to break the user input into words using the `split` function from the `Data.String` module. It then uses `runRWS` to run the `game` action (in the `RWS` monad), passing the game environment and current game state.

Having run the game logic, which is a pure computation, we need to print any log messages to the screen and show the user a prompt for the next command. The `for_` action is used to traverse the log (of type `List String`) and print its entries to the console. Finally, `setLineHandler` is used to update the line handler function to use the updated game state, and the prompt is displayed again using the `prompt` action.

The `runGame` function finally attaches the initial line handler to the console interface, and displays the initial prompt:

```haskell
  setLineHandler interface $ lineHandler initialGameState
  prompt interface
```

X> ## Exercises
X>
X> 1. (Medium) Implement a new command `cheat`, which moves all game items from the game grid into the user's inventory.
X> 1. (Difficult) The `Writer` component of the `RWS` monad is currently used for two types of messages: error messages and informational messages. Because of this, several parts of the code use case statements to handle error cases.
X>
X>     Refactor the code to use the `ExceptT` monad transformer to handle the error messages, and `RWS` to handle informational messages.

## Handling Command Line Options

The final piece of the application is responsible for parsing command line options and creating the `GameEnvironment` configuration record. For this, we use the `purescript-yargs` package.

`purescript-yargs` is an example of _applicative command line option parsing_. Recall that an applicative functor allows us to lift functions of arbitrary arity over a type constructor representing some type of side-effect. In the case of the `purescript-yargs` package, the functor we are interested in is the `Y` functor, which adds the side-effect of reading from command line options. It provides the following handler:

```haskell
runY :: forall a eff.
          YargsSetup ->
          Y (Eff (exception :: EXCEPTION, console :: CONSOLE | eff) a) ->
             Eff (exception :: EXCEPTION, console :: CONSOLE | eff) a
```

This is best illustrated by example. The application's `main` function is defined using `runY` as follows:

```haskell
main = runY (usage "$0 -p <player name>") $ map runGame env
```

The first argument is used to configure the `yargs` library. In our case, we simply provide a usage message, but the `Node.Yargs.Setup` module provides several other options.

The second argument uses the `map` function to lift the `runGame` function over the `Y` type constructor. The argument `env` is constructed in a `where` declaration using the applicative operators `<$>` and `<*>`:

```haskell
  where
  env :: Y GameEnvironment
  env = gameEnvironment
          <$> yarg "p" ["player"]
                   (Just "Player name")
                   (Right "The player name is required")
                   false
          <*> flag "d" ["debug"]
                   (Just "Use debug mode")
```

Here, the `gameEnvironment` function, which has the type `PlayerName -> Boolean -> GameEnvironment`, is lifted over `Y`. The two arguments specify how to read the player name and debug flag from the command line options. The first argument describes the player name option, which is specified by the `-p` or `--player` options, and the second describes the debug mode flag, which is turned on using the `-d` or `--debug` options.

This demonstrates two basic functions defined in the `Node.Yargs.Applicative` module: `yarg`, which defines a command line option which takes an optional argument (of type `String`, `Number` or `Boolean`), and `flag` which defines a command line flag of type `Boolean`.

Notice how we were able to use the notation afforded by the applicative operators to give a compact, declarative specification of our command line interface. In addition, it is simple to add new command line arguments, simply by adding a new function argument to `runGame`, and then using `<*>` to lift `runGame` over an additional argument in the definition of `env`.

X> ## Exercises
X>
X> 1. (Medium) Add a new Boolean-valued property `cheatMode` to the `GameEnvironment` record. Add a new command line flag `-c` to the `yargs` configuration which enables cheat mode. The `cheat` command from the previous exercise should be disallowed if cheat mode is not enabled.

## Conclusion

This chapter was a practical demonstration of the techniques we've learned so far, using monad transformers to build a pure specification of our game, and the `Eff` monad to build a front-end using the console.

Because we separated our implementation from the user interface, it would be possible to create other front-ends for our game. For example, we could use the `Eff` monad to render the game in the browser using the Canvas API or the DOM.

We have seen how monad transformers allow us to write safe code in an imperative style, where effects are tracked by the type system. In addition, type classes provide a powerful way to abstract over the actions provided by a monad, enabling code reuse. We were able to use standard abstractions like `Alternative` and `MonadPlus` to build useful monads by combining standard monad transformers.

Monad transformers are an excellent demonstration of the sort of expressive code that can be written by relying on advanced type system features such as higher-kinded polymorphism and multi-parameter type classes.

In the next chapter, we will see how monad transformers can be used to give an elegant solution to a common complaint when working with asynchronous JavaScript code - the problem of _callback hell_.
