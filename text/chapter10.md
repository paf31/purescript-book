# The Foreign Function Interface

## Chapter Goals

This chapter will introduce PureScript's _foreign function interface_ (or _FFI_), which enables communication from PureScript code to JavaScript code, and vice versa. We will cover the following:

- How to call pure JavaScript functions from PureScript,
- How to create new effect types and actions for use with the `Eff` monad, based on existing JavaScript code,
- How to call PureScript code from JavaScript,
- How to understand the representation of PureScript values at runtime,
- How to work with untyped data using the `purescript-foreign` package.

Towards the end of this chapter, we will revisit our recurring address book example. The goal of the chapter will be to add the following new functionality to our application using the FFI:

- Alerting the user with a popup notification,
- Storing the serialized form data in the browser's local storage, and reloading it when the application restarts.

## Project Setup

The source code for this module is a continuation of the source code from chapters 3, 7 and 8. As such, the source tree includes the appropriate source files from those chapters.

This chapter adds two new Bower dependencies:

1. The `purescript-foreign` library, which provides a data type and functions for working with _untyped data_.
1. The `purescript-foreign-generic` library, which adds support for _datatype generic programming_ to the `purescript-foreign` library.

_Note_: to avoid browser-specific issues with local storage when the webpage is served from a local file, it might be necessary to run this chapter's project over HTTP.

## A Disclaimer

PureScript provides a straightforward foreign function interface to make working with JavaScript as simple as possible. However, it should be noted that the FFI is an _advanced_ feature of the language. To use it safely and effectively, you should have an understanding of the runtime representation of the data you plan to work with. This chapter aims to impart such an understanding as pertains to code in PureScript's standard libraries.

PureScript's FFI is designed to be very flexible. In practice, this means that developers have a choice, between giving their foreign functions very simple types, or using the type system to protect against accidental misuses of foreign code. Code in the standard libraries tends to favor the latter approach.

As a simple example, a JavaScript function makes no guarantees that its return value will not be `null`. Indeed, idiomatic JavaScript code returns `null` quite frequently! However, PureScript's types are usually not inhabited by a null value. Therefore, it is the responsibility of the developer to handle these corner cases appropriately when designing their interfaces to JavaScript code using the FFI.

## Calling PureScript from JavaScript

Calling a PureScript function from JavaScript is very simple, at least for functions with simple types.

Let's take the following simple module as an example:

```haskell
module Test where

gcd :: Int -> Int -> Int
gcd 0 m = m
gcd n 0 = n
gcd n m
  | n > m     = gcd (n - m) m
  | otherwise = gcd (m - n) n
```

This function finds the greatest common divisor of two numbers by repeated subtraction. It is a nice example of a case where you might like to use PureScript to define the function, but have a requirement to call it from JavaScript: it is simple to define this function in PureScript using pattern matching and recursion, and the implementor can benefit from the use of the type checker.

To understand how this function can be called from JavaScript, it is important to realize that PureScript functions always get turned into JavaScript functions of a single argument, so we need to apply its arguments one-by-one:

```javascript
var Test = require('Test');
Test.gcd(15)(20);
```

Here, I am assuming that the code was compiled with `pulp build`, which compiles PureScript modules to CommonJS modules. For that reason, I was able to reference the `gcd` function on the `Test` object, after importing the `Test` module using `require`.

You might also like to bundle JavaScript code for the browser, using `pulp build -O --to file.js`. In that case, you would access the `Test` module from the global PureScript namespace, which defaults to `PS`:

```javascript
var Test = PS.Test;
Test.gcd(15)(20);
```

## Understanding Name Generation

PureScript aims to preserve names during code generation as much as possible. In particular, most identifiers which are neither PureScript nor Javascript keywords can be expected to be preserved, at least for names of top-level declarations.

If you decide to use a Javascript keyword as an identifier, the name will be escaped with a double dollar symbol. For example,

```haskell
null = []
```

generates the following Javascript:

```javascript
var $$null = [];
```

In addition, if you would like to use special characters in your identifier names, they will be escaped using a single dollar symbol. For example,

```haskell
example' = 100
```

generates the following Javascript:

```javascript
var example$prime = 100;
```

Where compiled PureScript code is intended to be called from JavaScript, it is recommended that identifiers only use alphanumeric characters, and avoid JavaScript keywords. If user-defined operators are provided for use in PureScript code, it is good practice to provide an alternative function with an alphanumeric name for use in JavaScript.

## Runtime Data Representation

Types allow us to reason at compile-time that our programs are "correct" in some sense - that is, they will not break at runtime. But what does that mean? In PureScript, it means that the type of an expression should be compatible with its representation at runtime.

For that reason, it is important to understand the representation of data at runtime to be able to use PureScript and JavaScript code together effectively. This means that for any given PureScript expression, we should be able to understand the behavior of the value it will evaluate to at runtime.

The good news is that PureScript expressions have particularly simple representations at runtime. It should always be possible to understand the runtime data representation of an expression by considering its type.

For simple types, the correspondence is almost trivial. For example, if an expression has the type `Boolean`, then its value `v` at runtime should satisfy `typeof v === 'boolean'`. That is, expressions of type `Boolean` evaluate to one of the (JavaScript) values `true` or `false`. In particular, there is no PureScript expression of type `Boolean` which evaluates to `null` or `undefined`.

A similar law holds for expressions of type `Int` `Number` and `String` - expressions of type `Int` or `Number` evaluate to non-null JavaScript numbers, and expressions of type `String` evaluate to non-null JavaScript strings. Expressions of type `Int` will evaluate to integers at runtime, even though they cannot not be distinguished from values of type `Number` by using `typeof`.

What about some more complex types?

As we have already seen, PureScript functions correspond to JavaScript functions of a single argument. More precisely, if an expression `f` has type `a -> b` for some types `a` and `b`, and an expression `x` evaluates to a value with the correct runtime representation for type `a`, then `f` evaluates to a JavaScript function, which when applied to the result of evaluating `x`, has the correct runtime representation for type `b`. As a simple example, an expression of type `String -> String` evaluates to a function which takes non-null JavaScript strings to non-null JavaScript strings.

As you might expect, PureScript's arrays correspond to JavaScript arrays. But remember - PureScript arrays are homogeneous, so every element has the same type. Concretely, if a PureScript expression `e` has type `Array a` for some type `a`, then `e` evaluates to a (non-null) JavaScript array, all of whose elements have the correct runtime representation for type `a`.

We've already seen that PureScript's records evaluate to JavaScript objects. Just as for functions and arrays, we can reason about the runtime representation of data in a record's fields by considering the types associated with its labels. Of course, the fields of a record are not required to be of the same type.

## Representing ADTs

For every constructor of an algebraic data type, the PureScript compiler creates a new JavaScript object type by defining a function. Its constructors correspond to functions which create new JavaScript objects based on those prototypes.

For example, consider the following simple ADT:

```haskell
data ZeroOrOne a = Zero | One a
```

The PureScript compiler generates the following code:

```javascript
function One(value0) {
    this.value0 = value0;
};

One.create = function (value0) {
    return new One(value0);
};

function Zero() {
};

Zero.value = new Zero();
```

Here, we see two JavaScript object types: `Zero` and `One`. It is possible to create values of each type by using JavaScript's `new` keyword. For constructors with arguments, the compiler stores the associated data in fields called `value0`, `value1`, etc.

The PureScript compiler also generates helper functions. For constructors with no arguments, the compiler generates a `value` property, which can be reused instead of using the `new` operator repeatedly. For constructors with one or more arguments, the compiler generates a `create` function, which takes arguments with the appropriate representation and applies the appropriate constructor.

What about constructors with more than one argument? In that case, the PureScript compiler also creates a new object type, and a helper function. This time, however, the helper function is curried function of two arguments. For example, this algebraic data type:

```haskell
data Two a b = Two a b
```

generates this JavaScript code:

```javascript
function Two(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
};

Two.create = function (value0) {
    return function (value1) {
        return new Two(value0, value1);
    };
};
```

Here, values of the object type `Two` can be created using the `new` keyword, or by using the `Two.create` function.

The case of newtypes is slightly different. Recall that a newtype is like an algebraic data type, restricted to having a single constructor taking a single argument. In this case, the runtime representation of the newtype is actually the same as the type of its argument.

For example, this newtype representing telephone numbers:

```haskell
newtype PhoneNumber = PhoneNumber String
```

is actually represented as a JavaScript string at runtime. This is useful for designing libraries, since newtypes provide an additional layer of type safety, but without the runtime overhead of another function call.

## Representing Quantified Types

Expressions with quantified (polymorphic) types have restrictive representations at runtime. In practice, this means that there are relatively few expressions with a given quantified type, but that we can reason about them quite effectively.

Consider this polymorphic type, for example:

```haskell
forall a. a -> a
```

What sort of functions have this type? Well, there is certainly one function with this type - namely, the identity function `id`, defined in the `Prelude`:

```haskell
id :: forall a. a -> a
id a = a
```

In fact, the `id` function is the _only_ (total) function with this type! This certainly seems to be the case (try writing an expression with this type which is not observably equivalent to `id`), but how can we be sure? We can be sure by considering the runtime representation of the type.

What is the runtime representation of a quantified type `forall a. t`? Well, any expression with the runtime representation for this type must have the correct runtime representation for the type `t` for any choice of type `a`. In our example above, a function of type `forall a. a -> a` must have the correct runtime representation for the types `String -> String`, `Number -> Number`, `Array Boolean -> Array Boolean`, and so on. It must take strings to strings, numbers to numbers, etc.

But that is not enough - the runtime representation of a quantified type is more strict than this. We require any expression to be _parametrically polymorphic_ - that is, it cannot use any information about the type of its argument in its implementation. This additional condition prevents problematic implementations such as the following JavaScript function from inhabiting a polymorphic type:

```javascript
function invalid(a) {
    if (typeof a === 'string') {
        return "Argument was a string.";
    } else {
        return a;
    }
}
```

Certainly, this function takes strings to strings, numbers to numbers, etc. but it does not meet the additional condition, since it inspects the (runtime) type of its argument, so this function would not be a valid inhabitant of the type `forall a. a -> a`.

Without being able to inspect the runtime type of our function argument, our only option is to return the argument unchanged, and so `id` is indeed the only inhabitant of the type `forall a. a -> a`.

A full discussion of _parametric polymorphism_ and _parametricity_ is beyond the scope of this book. Note however, that since PureScript's types are _erased_ at runtime, a polymorphic function in PureScript _cannot_ inspect the runtime representation of its arguments (without using the FFI), and so this representation of polymorphic data is appropriate.

## Representing Constrained Types

Functions with a type class constraint have an interesting representation at runtime. Because the behavior of the function might depend on the type class instance chosen by the compiler, the function is given an additional argument, called a _type class dictionary_, which contains the implementation of the type class functions provided by the chosen instance.

For example, here is a simple PureScript function with a constrained type which uses the `Show` type class:

```haskell
shout :: forall a. Show a => a -> String
shout a = show a <> "!!!"
```

The generated JavaScript looks like this:

```javascript
var shout = function (dict) {
    return function (a) {
        return show(dict)(a) + "!!!";
    };
};
```

Notice that `shout` is compiled to a (curried) function of two arguments, not one. The first argument `dict` is the type class dictionary for the `Show` constraint. `dict` contains the implementation of the `show` function for the type `a`.

We can call this function from JavaScript by passing an explicit type class dictionary from the Prelude as the first parameter:

```javascript
shout(require('Prelude').showNumber)(42);
```

X> ## Exercises
X>
X> 1. (Easy) What are the runtime representations of these types?
X>
X>     ```haskell
X>     forall a. a
X>     forall a. a -> a -> a
X>     forall a. Ord a => Array a -> Boolean
X>     ```
X>
X>     What can you say about the expressions which have these types?
X> 1. (Medium) Try using the functions defined in the `purescript-arrays` package, calling them from JavaScript, by compiling the library using `pulp build` and importing modules using the `require` function in NodeJS. _Hint_: you may need to configure the output path so that the generated CommonJS modules are available on the NodeJS module path.

## Using JavaScript Code From PureScript

The simplest way to use JavaScript code from PureScript is to give a type to an existing JavaScript value using a _foreign import_ declaration. Foreign import
declarations should have a corresponding Javascript declaration in a _foreign Javascript module_.

For example, consider the `encodeURIComponent` function, which can be used from JavaScript to encode a component of a URI by escaping special characters:

```text
$ node

node> encodeURIComponent('Hello World')
'Hello%20World'
```

This function has the correct runtime representation for the function type `String -> String`, since it takes non-null strings to non-null strings, and has no other side-effects.

We can assign this type to the function with the following foreign import declaration:

```haskell
module Data.URI where

foreign import encodeURIComponent :: String -> String
```

We also need to write a foreign Javascript module. If the module above is saved as `src/Data/URI.purs`, then the foreign Javascript module should be saved as
`src/Data/URI.js`:

```javascript
"use strict";

exports.encodeURIComponent = encodeURIComponent;
```

Pulp will find `.js` files in the `src` directory, and provide them to the compiler as foreign Javascript modules.

Javascript functions and values are exported from foreign Javascript modules by assigning them to the `exports` object just like a regular CommonJS module. The `psc` compiler treats this module like a regular CommonJS module, and simply adds it as a dependency to the compiled
PureScript module. However, when bundling code for the browser with `psc-bundle` or `pulp build -O --to`, it is very important to follow the
pattern above, assigning exports to the `exports` object using a property assignment. This is because `psc-bundle` recognizes this format,
allowing unused Javascript exports to be removed from bundled code.

With these two pieces in place, we can now use the `encodeURIComponent` function from PureScript like any function written in PureScript.
For example, if this declaration is saved as a module and loaded into PSCi, we can reproduce the calculation above:

```text
$ pulp repl

> import Data.URI
> encodeURIComponent "Hello World"
"Hello%20World"
```

This approach works well for simple JavaScript values, but is of limited use for more complicated examples. The reason is that most idiomatic JavaScript code does not meet the strict criteria imposed by the runtime representations of the basic PureScript types. In those cases, we have another option - we can _wrap_ the JavaScript code in such a way that we can force it to adhere to the correct runtime representation.

## Wrapping JavaScript Values

We might want to wrap Javascript values and functions for a number of reasons:

- A function takes multiple arguments, but we want to call it like a curried function.
- We might want to use the `Eff` monad to keep track of any JavaScript side-effects.
- It might be necessary to handle corner cases like `null` or `undefined`, to give a function the correct runtime representation.

For example, suppose we wanted to recreate the `head` function on arrays by using a foreign declaration. In JavaScript, we might write the function as follows:

```javascript
function head(arr) {
    return arr[0];
}
```

However, there is a problem with this function. We might try to give it the type `forall a. Array a -> a`, but for empty arrays, this function returns `undefined`. Therefore, this function does not have the correct runtime representation, and we should use a wrapper function to handle this corner case.

To keep things simple, we can throw an exception in the case of an empty array. Strictly speaking, pure functions should not throw exceptions, but it will suffice for demonstration purposes, and we can indicate the lack of safety in the function name:

```haskell
foreign import unsafeHead :: forall a. Array a -> a
```

In our foreign Javascript module, we can define `unsafeHead` as follows:

```javascript
exports.unsafeHead = function(arr) {
  if (arr.length) {
    return arr[0];
  } else {
    throw new Error('unsafeHead: empty array');
  }
};
```

## Defining Foreign Types

Throwing an exception in the case of failure is less than ideal - idiomatic PureScript code uses the type system to represent side-effects such as missing values. One example of this approach is the `Maybe` type constructor. In this section, we will build another solution using the FFI.

Suppose we wanted to define a new type `Undefined a` whose representation at runtime was like that for the type `a`, but also allowing the `undefined` value.

We can define a _foreign type_ using the FFI using a _foreign type declaration_. The syntax is similar to defining a foreign function:

```haskell
foreign import data Undefined :: Type -> Type
```

Note that the `data` keyword here indicates that we are defining a type, not a value. Instead of a type signature, we give the _kind_ of the new type. In this case, we declare the kind of `Undefined` to be `Type -> Type`. In other words, `Undefined` is a type constructor.

We can now simplify our original definition for `head`:

```javascript
exports.head = function(arr) {
  return arr[0];
};
```

And in the PureScript module:

```haskell
foreign import head :: forall a. Array a -> Undefined a
```

Note the two changes: the body of the `head` function is now much simpler, and returns `arr[0]` even if that value is undefined, and the type signature has been changed to reflect the fact that our function can return an undefined value.

This function has the correct runtime representation for its type, but is quite useless since we have no way to use a value of type `Undefined a`. But we can fix that by writing some new functions using the FFI!

The most basic function we need will tell us whether a value is defined or not:

```haskell
foreign import isUndefined :: forall a. Undefined a -> Boolean
```

This is easily defined in our foreign Javascript module as follows:

```javascript
exports.isUndefined = function(value) {
  return value === undefined;
};
```

We can now use `isUndefined` and `head` together from PureScript to define a useful function:

```haskell
isEmpty :: forall a. Array a -> Boolean
isEmpty = isUndefined <<< head
```

Here, the foreign functions we defined were very simple, which meant that we were able to benefit from the use of PureScript's typechecker as much as possible. This is good practice in general: foreign functions should be kept as small as possible, and application logic moved into PureScript code wherever possible.

## Functions of Multiple Arguments

PureScript's Prelude contains an interesting set of examples of foreign types. As we have covered already, PureScript's function types only take a single argument, and can be used to simulate functions of multiple arguments via _currying_. This has certain advantages - we can partially apply functions, and give type class instances for function types - but it comes with a performance penalty. For performance critical code, it is sometimes necessary to define genuine JavaScript functions which accept multiple arguments. The Prelude defines foreign types which allow us to work safely with such functions.

For example, the following foreign type declaration is taken from the Prelude in the `Data.Function.Uncurried` module:

```haskell
foreign import data Fn2 :: Type -> Type -> Type -> Type
```

This defines the type constructor `Fn2` which takes three type arguments. `Fn2 a b c` is a type representing JavaScript functions of two arguments of types `a` and `b`, and with return type `c`.

The `purescript-functions` package defines similar type constructors for function arities from 0 to 10.

We can create a function of two arguments by using the `mkFn2` function, as follows:

```haskell
import Data.Function.Uncurried

divides :: Fn2 Int Int Boolean
divides = mkFn2 \n m -> m % n == 0
```

and we can apply a function of two arguments by using the `runFn2` function:

```haskell
> runFn2 divides 2 10
true

> runFn2 divides 3 10
false
```

The key here is that the compiler _inlines_ the `mkFn2` and `runFn2` functions whenever they are fully applied. The result is that the generated code is very compact:

```javascript
exports.divides = function(n, m) {
    return m % n === 0;
};
```

## Representing Side Effects

The `Eff` monad is also defined as a foreign type in the Prelude. Its runtime representation is quite simple - an expression of type `Eff eff a` should evaluate to a JavaScript function of no arguments, which performs any side-effects and returns a value with the correct runtime representation for type `a`.

The definition of the `Eff` type constructor is given in the `Control.Monad.Eff` module as follows:

```haskell
foreign import data Eff :: # Effect -> Type -> Type
```

Recall that the `Eff` type constructor is parameterized by a row of effects and a return type, which is reflected in its kind.

As a simple example, consider the `random` function defined in the `purescript-random` package. Recall that its type was:

```haskell
foreign import random :: forall eff. Eff (random :: RANDOM | eff) Number
```

The definition of the `random` function is given here:

```javascript
exports.random = function() {
  return Math.random();
};
```

Notice that the `random` function is represented at runtime as a function of no arguments. It performs the side effect of generating a random number, and returns it, and the return value matches the runtime representation of the `Number` type: it is a non-null JavaScript number.

As a slightly more interesting example, consider the `log` function defined by the `Control.Monad.Eff.Console` module in the `purescript-console` package. The `log` function has the following type:

```haskell
foreign import log :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
```

And here is its definition:

```javascript
exports.log = function (s) {
  return function () {
    console.log(s);
  };
};
```

The representation of `log` at runtime is a JavaScript function of a single argument, returning a function of no arguments. The inner function performs the side-effect of writing a message to the console.

The effects `RANDOM` and `CONSOLE` are also defined as foreign types. Their kinds are defined to be `Effect`, the kind of effects. For example:

```haskell
foreign import data RANDOM :: Effect
```

In fact, it is possible to define new effects in this way, as we will soon see.

Expressions of type `Eff eff a` can be invoked from JavaScript like regular JavaScript methods. For example, since the `main` function is required to have type `Eff eff a` for some set of effects `eff` and some type `a`, it can be invoked as follows:

```javascript
require('Main').main();
```

When using `pulp build -O --to` or `pulp run`, this call to `main` is generated automatically, whenever the `Main` module is defined.

## Defining New Effects

The source code for this chapter defines two new effects. The simplest is the `ALERT` effect, defined in the `Control.Monad.Eff.Alert` module. It is used to indicate that a computation might alert the user using a popup window.

The effect is defined first, using a foreign type declaration:

```haskell
foreign import data ALERT :: Effect
```

`ALERT` is given the kind `Effect`, indicating that it represents an effect, as opposed to a type.

Next, the `alert` action is defined. The `alert` action displays a popup, and adds the `ALERT` effect to the row of effects:

```haskell
foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit
```

The foreign Javascript module is straightforward, defining the `alert` function by assigning it to the `exports` variable:

```javascript
"use strict";

exports.alert = function(msg) {
    return function() {
        window.alert(msg);
    };
};

```

The `alert` action is very similar to the `log` action from the `Control.Monad.Eff.Console` module. The only difference is that the `alert` action uses the `window.alert` method, whereas the `log` action uses the `console.log` method. As such, `alert` can only be used in environments where `window.alert` is defined, such as a web browser.

Note that, as in the case of `log`, the `alert` function uses a function of no arguments to represent the computation of type `Eff (alert :: ALERT | eff) Unit`.

The second effect defined in this chapter is the `STORAGE` effect, which is defined in the `Control.Monad.Eff.Storage` module. It is used to indicate that a computation might read or write values using the Web Storage API.

The effect is defined in the same way:

```haskell
foreign import data STORAGE :: Effect
```

The `Control.Monad.Eff.Storage` module defines two actions: `getItem`, which retrieves a value from local storage, and `setItem` which inserts or updates a value in local storage. The two functions have the following types:

```haskell
foreign import getItem
  :: forall eff
   . String
  -> Eff (storage :: STORAGE | eff) Foreign

foreign import setItem
  :: forall eff
   . String
  -> String
  -> Eff (storage :: STORAGE | eff) Unit
```

The interested reader can inspect the source code for this module to see the definitions of these actions.

`setItem` takes a key and a value (both strings), and returns a computation which stores the value in local storage at the specified key.

The type of `getItem` is more interesting. It takes a key, and attempts to retrieve the associated value from local storage. However, since the `getItem` method on `window.localStorage` can return `null`, the return type is not `String`, but `Foreign` which is defined by the `purescript-foreign` package in the `Data.Foreign` module.

`Data.Foreign` provides a way to work with _untyped data_, or more generally, data whose runtime representation is uncertain.

X> ## Exercises
X>
X> 1. (Medium) Write a wrapper for the `confirm` method on the JavaScript `Window` object, and add your foreign function to the `Control.Monad.Eff.Alert` module.
X> 1. (Medium) Write a wrapper for the `removeItem` method on the `localStorage` object, and add your foreign function to the `Control.Monad.Eff.Storage` module.

## Working With Untyped Data

In this section, we will see how we can use the `Data.Foreign` library to turn untyped data into typed data, with the correct runtime representation for its type.

The code for this chapter builds on the address book example from chapter 8, by adding a Save button at the bottom of the form. When the Save button is clicked, the state of the form is serialized to JSON and stored in local storage. When the page is reloaded, the JSON document is retrieved from local storage and parsed.

The `Main` module defines a type for the saved form data:

```haskell
newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  , street     :: String
  , city       :: String
  , state      :: String
  , homePhone  :: String
  , cellPhone  :: String
  }
```

The problem is that we have no guarantee that the JSON will have the correct form. Put another way, we don't know that the JSON represents the correct type of data at runtime. This is the sort of problem that is solved by the `purescript-foreign` library. Here are some other examples:

- A JSON response from a web service
- A value passed to a function from JavaScript code

Let's try the `purescript-foreign` and `purescript-foreign-generic` libraries in PSCi.

Start by importing some modules:

```text
> import Data.Foreign
> import Data.Foreign.Generic
> import Data.Foreign.JSON
```

A good way to obtain a `Foreign` value is to parse a JSON document. `purescript-foreign-generic` defines the following two functions:

```haskell
parseJSON :: String -> F Foreign
decodeJSON :: forall a. Decode a => String -> F a
```

The type constructor `F` is actually just a type synonym, defined in `Data.Foreign`:

```haskell
type F = Except (NonEmptyList ForeignError)
```

Here, `Except` is an monad for handling exceptions in pure code, much like `Either`. We can convert a value in the `F` monad into a value in the `Either` monad by using the `runExcept` function.

Most of the functions in the `purescript-foreign` and `purescript-foreign-generic` libraries return a value in the `F` monad, which means that we can use do notation and the applicative functor combinators to build typed values.

The `Decode` type class represents those types which can be obtained from untyped data. There are type class instances defined for the primitive types and arrays, and we can define our own instances as well.

Let's try parsing some simple JSON documents using `decodeJSON` in PSCi (remembering to use `runExcept` to unwrap the results):

```text
> import Control.Monad.Except

> runExcept (decodeJSON "\"Testing\"" :: F String)
Right "Testing"

> runExcept (decodeJSON "true" :: F Boolean)
Right true

> runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
Right [1, 2, 3]
```

Recall that in the `Either` monad, the `Right` data constructor indicates success. Note however, that invalid JSON, or an incorrect type leads to an error:

```text
> runExcept (decodeJSON "[1, 2, true]" :: F (Array Int))
(Left (NonEmptyList (NonEmpty (ErrorAtIndex 2 (TypeMismatch "Int" "Boolean")) Nil)))
```

The `purescript-foreign-generic` library tells us where in the JSON document the type error occurred.

## Handling Null and Undefined Values

Real-world JSON documents contain null and undefined values, so we need to be able to handle those too.

`purescript-foreign-generic` defines a type constructors which solves this problem: `NullOrUndefined`. It serves a similar purpose to the `Undefined` type constructor that we defined earlier, but uses the `Maybe` type constructor internally to represent missing values.

The module also provides a function `unNullOrUndefined` to unwrap the inner value. We can lift the appropriate function over the `decodeJSON` action to parse JSON documents which permit null values:

```text
> import Prelude
> import Data.Foreign.NullOrUndefined

> runExcept (unNullOrUndefined <$> decodeJSON "42" :: F (NullOrUndefined Int))
(Right (Just 42))

> runExcept (unNullOrUndefined <$> decodeJSON "null" :: F (NullOrUndefined Int))
(Right Nothing)
```

In each case, the type annotation applies to the term to the right of the `<$>` operator. For example, `decodeJSON "42"` has the type `F (NullOrUndefined Int)`. The `unNullOrUndefined` function is then lifted over `F` to give the final type `F (Maybe Int)`.

The type `NullOrUndefined Int` represents values which are either integers, or null. What if we wanted to parse more interesting values, like arrays of integers, where each element might be `null`? In that case, we could lift the function `map unNullOrUndefined` over the `decodeJSON` action, as follows:

```text
> runExcept (map unNullOrUndefined <$> decodeJSON "[1, 2, null]" :: F (Array (NullOrUndefined Int)))
(Right [(Just 1),(Just 2),Nothing])
```

In general, using newtypes to wrap an existing type is a good way to provide different serialization strategies for the same type. The `NullOrUndefined` type is defined as a newtype around the `Maybe` type constructor.

## Generic JSON Serialization

In fact, we rarely need to write instances for the `Decode` class, since the `purescript-foreign-generic` class allows us to _derive_ instances using a technique called _datatype-generic programming_. A full explanation of this technique is beyond the scope of this book, but it allows us to write functions once, and reuse them over many different data types, based on the structure of a the types themselves.

To derive a `Decode` instance for our `FormData` type (so that we may deserialize it from its JSON representation), we first use the `derive` keyword to derive an instance of the `Generic` type class, which looks like this:

```haskell
derive instance genericFormData :: Generic FormData _
```

Next, we simply define the `decode` function using the `genericDecode` function, as follows:

```haskell
instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
```

In fact, we can also derive an _encoder_ in the same way:

```haskell
instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
```

It is important that we use the same options in the decoder and encoder, otherwise our encoded JSON documents might not get decoded correctly.

Now, when the Save button is clicked, a value of type `FormData` is passed to the `encode` function, serializing it as a JSON document. The `FormData` type is a newtype for a record, so a value of type `FormData` passed to `encode` will be serialized as a JSON _object_. This is because we used the `unwrapSingleConstructors` option when defining our JSON encoder.

Our `Decode` type class instance is used with `decodeJSON` to parse the JSON document when it is retrieved from local storage, as follows:

```haskell
loadSavedData = do
  item <- getItem "person"

  let
    savedData :: Either (NonEmptyList ForeignError) (Maybe FormData)
    savedData = runExcept do
      jsonOrNull <- traverse readString =<< readNullOrUndefined item
      traverse decodeJSON jsonOrNull
```

The `savedData` action reads the `FormData` structure in two steps: first, it parses the `Foreign` value obtained from `getItem`. The type of `jsonOrNull` is inferred by the compiler to be `Maybe String` (exercise for the reader - how is this type inferred?). The `traverse` function is then used to apply `decodeJSON` to the (possibly missing) element of the result of type `Maybe String`. The type class instance inferred for `decodeJSON` is the one we just wrote, resulting in a value of type `F (Maybe FormData)`.

We need to use the monadic structure of `F`, since the argument to `traverse` uses the result `jsonOrNull` obtained in the first line.

There are three possibilities for the result of `FormData`:

- If the outer constructor is `Left`, then there was an error parsing the JSON string, or it represented a value of the wrong type. In this case, the application displays an error using the `alert` action we wrote earlier.
- If the outer constructor is `Right`, but the inner constructor is `Nothing`, then `getItem` also returned `Nothing` which means that the key did not exist in local storage. In this case, the application continues quietly.
- Finally, a value matching the pattern `Right (Just _)` indicates a successfully parsed JSON document. In this case, the application updates the form fields with the appropriate values.

Try out the code, by running `pulp build -O --to dist/Main.js`, and then opening the browser to `html/index.html`. You should be able to save the form fields' contents to local storage by clicking the Save button, and then see the fields repopulated when the page is refreshed.

_Note_: You may need to serve the HTML and Javascript files from a HTTP server locally in order to avoid certain browser-specific issues.

X> ## Exercises
X>
X> 1. (Easy) Use `decodeJSON` to parse a JSON document representing a two-dimensional JavaScript array of integers, such as `[[1, 2, 3], [4, 5], [6]]`. What if the elements are allowed to be null? What if the arrays themselves are allowed to be null?
X> 1. (Medium) Convince yourself that the implementation of `savedData` should type-check, and write down the inferred types of each subexpression in the computation.
X> 1. (Medium) The following data type represents a binary tree with values at the leaves:
X>
X>     ```haskell
X>     data Tree a = Leaf a | Branch (Tree a) (Tree a)
X>     ```
X>
X>     Derive `Encode` and `Decode` instances for this type using `purescript-foreign-generic`, and verify that encoded values can correctly be decoded in PSCi.
X> 1. (Difficult) The following `data` type should be represented directly in JSON as either an integer or a string:
X>
X>     ```haskell
X>     data IntOrString
X>       = IntOrString_Int Int
X>       | IntOrString_String String
X>     ```
X>
X>     Write instances for `Encode` and `Decode` for the `IntOrString` data type which implement this behavior, and verify that encoded values can correctly be decoded in PSCi.

## Conclusion

In this chapter, we've learned how to work with foreign JavaScript code from PureScript, and vice versa, and we've seen the issues involved with writing trustworthy code using the FFI:

- We've seen the importance of the _runtime representation_ of data, and ensuring that foreign functions have the correct representation.
- We learned how to deal with corner cases like null values and other types of JavaScript data, by using foreign types, or the `Foreign` data type.
- We looked at some common foreign types defined in the Prelude, and how they can be used to interoperate with idiomatic JavaScript code. In particular, the representation of side-effects in the `Eff` monad was introduced, and we saw how to use the `Eff` monad to capture new side effects.
- We saw how to safely deserialize JSON data using the `Decode` type class.

For more examples, the `purescript`, `purescript-contrib` and `purescript-node` GitHub organizations provide plenty of examples of libraries which use the FFI. In the remaining chapters, we will see some of these libraries put to use to solve real-world problems in a type-safe way.
