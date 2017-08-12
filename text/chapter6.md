# Type Classes

## Chapter Goals

This chapter will introduce a powerful form of abstraction which is enabled by PureScript's type system - type classes.

This motivating example for this chapter will be a library for hashing data structures. We will see how the machinery of type classes allow us to hash complex data structures without having to think directly about the structure of the data itself.

We will also see a collection of standard type classes from PureScript's Prelude and standard libraries. PureScript code leans heavily on the power of type classes to express ideas concisely, so it will be beneficial to familiarize yourself with these classes.

## Project Setup

The source code for this chapter is defined in the file `src/Data/Hashable.purs`. 

The project has the following Bower dependencies:

- `purescript-maybe`, which defines the `Maybe` data type, which represents optional values.
- `purescript-tuples`, which defines the `Tuple` data type, which represents pairs of values.
- `purescript-either`, which defines the `Either` data type, which represents disjoint unions.
- `purescript-strings`, which defines functions which operate on strings.
- `purescript-functions`, which defines some helper functions for defining PureScript functions.

The module `Data.Hashable` imports several modules provided by these Bower packages.

## Show Me!

Our first simple example of a type class is provided by a function we've seen several times already: the `show` function, which takes a value and displays it as a string.

`show` is defined by a type class in the `Prelude` module called `Show`, which is defined as follows:

```haskell
class Show a where
  show :: a -> String
```

This code declares a new _type class_ called `Show`, which is parameterized by the type variable `a`.

A type class _instance_ contains implementations of the functions defined in a type class, specialized to a particular type.

For example, here is the definition of the `Show` type class instance for `Boolean` values, taken from the Prelude:

```haskell
instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"
```

This code declares a type class instance called `showBoolean` - in PureScript, type class instances are named to aid the readability of the generated JavaScript. We say that the `Boolean` type _belongs to the `Show` type class_.

We can try out the `Show` type class in PSCi, by showing a few values with different types:

```text
> import Prelude

> show true
"true"

> show 1.0
"1.0"

> show "Hello World"
"\"Hello World\""
```

These examples demonstrate how to `show` values of various primitive types, but we can also `show` values with more complicated types:

```text
> import Data.Tuple

> show (Tuple 1 true)
"(Tuple 1 true)"

> import Data.Maybe

> show (Just "testing")
"(Just \"testing\")"
```

If we try to show a value of type `Data.Either`, we get an interesting error message:

```text
> import Data.Either
> show (Left 10)

The inferred type

    forall a. Show a => String

has type variables which are not mentioned in the body of the type. Consider adding a type annotation.
```

The problem here is not that there is no `Show` instance for the type we intended to `show`, but rather that PSCi was unable to infer the type. This is indicated by the _unknown type_ `a` in the inferred type.

We can annotate the expression with a type, using the `::` operator, so that PSCi can choose the correct type class instance:

```text
> show (Left 10 :: Either Int String)
"(Left 10)"
```

Some types do not have a `Show` instance defined at all. One example of this is the function type `->`. If we try to `show` a function from `Int` to `Int`, we get an appropriate error message from the type checker:

```text
> import Prelude
> show $ \n -> n + 1

No type class instance was found for

  Data.Show.Show (Int -> Int)
```

X> ## Exercises
X>
X> 1. (Easy) Use the `showShape` function from the previous chapter to define a `Show` instance for the `Shape` type.

## Common Type Classes

In this section, we'll look at some standard type classes defined in the Prelude and standard libraries. These type classes form the basis of many common patterns of abstraction in idiomatic PureScript code, so a basic understanding of their functions is highly recommended.

### Eq

The `Eq` type class defines the `eq` function, which tests two values for equality. The `==` operator is actually just an alias for `eq`.

```haskell
class Eq a where
  eq :: a -> a -> Boolean
```

Note that in either case, the two arguments must have the same type: it does not make sense to compare two values of different types for equality.

Try out the `Eq` type class in PSCi:

```text
> 1 == 2
false

> "Test" == "Test"
true
```

### Ord

The `Ord` type class defines the `compare` function, which can be used to compare two values, for types which support ordering. The comparison operators `<` and `>` along with their non-strict companions `<=` and `>=`, can be defined in terms of `compare`.

```haskell
data Ordering = LT | EQ | GT

class Eq a <= Ord a where
  compare :: a -> a -> Ordering
```

The `compare` function compares two values, and returns an `Ordering`, which has three alternatives:

- `LT` - if the first argument is less than the second.
- `EQ` - if the first argument is equal to the second.
- `GT` - if the first argument is greater than the second.

Again, we can try out the `compare` function in PSCi:

```text
> compare 1 2
LT

> compare "A" "Z"
LT
```

### Field

The `Field` type class identifies those types which support numeric operators such as addition, subtraction, multiplication and division. It is provided to abstract over those operators, so that they can be reused where appropriate.

_Note_: Just like the `Eq` and `Ord` type classes, the `Field` type class has special support in the PureScript compiler, so that simple expressions such as `1 + 2 * 3` get translated into simple JavaScript, as opposed to function calls which dispatch based on a type class implementation.

```haskell
class EuclideanRing a <= Field a
```

The `Field` type class is composed from several more general _superclasses_. This allows us to talk abstractly about types which support some but not all of the `Field` operations. For example, a type of natural numbers would be closed under addition and multiplication, but not necessarily under subtraction, so that type might have an instance of the `Semiring` class (which is a superclass of `Num`), but not an instance of `Ring` or `Field`.

Superclasses will be explained later in this chapter, but the full numeric type class hierarchy is beyond the scope of this chapter. The interested reader is encouraged to read the documentation for the superclasses of `Field` in `purescript-prelude`.

### Semigroups and Monoids

The `Semigroup` type class identifies those types which support an `append` operation to combine two values:

```haskell
class Semigroup a where
  append :: a -> a -> a
```

Strings form a semigroup under regular string concatenation, and so do arrays. Several other standard instances are provided by the `purescript-monoid` package.

The `<>` concatenation operator, which we have already seen, is provided as an alias for `append`.

The `Monoid` type class (provided by the `purescript-monoid` package) extends the `Semigroup` type class with the concept of an empty value, called `mempty`:

```haskell
class Semigroup m <= Monoid m where
  mempty :: m
```

Again, strings and arrays are simple examples of monoids.

A `Monoid` type class instance for a type describes how to _accumulate_ a result with that type, by starting with an "empty" value, and combining new results. For example, we can write a function which concatenates an array of values in some monoid by using a fold. In PSCi:

```haskell
> import Data.Monoid
> import Data.Foldable

> foldl append mempty ["Hello", " ", "World"]  
"Hello World"

> foldl append mempty [[1, 2, 3], [4, 5], [6]]
[1,2,3,4,5,6]
```

The `purescript-monoid` package provides many examples of monoids and semigroups, which we will use in the rest of the book.

### Foldable

If the `Monoid` type class identifies those types which act as the result of a fold, then the `Foldable` type class identifies those type constructors which can be used as the source of a fold.

The `Foldable` type class is provided in the `purescript-foldable-traversable` package, which also contains instances for some standard containers such as arrays and `Maybe`.

The type signatures for the functions belonging to the `Foldable` class are a little more complicated than the ones we've seen so far:

```haskell
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
```

It is instructive to specialize to the case where `f` is the array type constructor. In this case, we can replace `f a` with `Array a` for any a, and we notice that the types of `foldl` and `foldr` become the types that we saw when we first encountered folds over arrays.

What about `foldMap`? Well, that becomes `forall a m. Monoid m => (a -> m) -> Array a -> m`. This type signature says that we can choose any type `m` for our result type, as long as that type is an instance of the `Monoid` type class. If we can provide a function which turns our array elements into values in that monoid, then we can accumulate over our array using the structure of the monoid, and return a single value.

Let's try out `foldMap` in PSCi:

```text
> import Data.Foldable

> foldMap show [1, 2, 3, 4, 5]
"12345"
```

Here, we choose the monoid for strings, which concatenates strings together, and the `show` function which renders an `Int` as a `String`. Then, passing in an array of integers, we see that the results of `show`ing each integer have been concatenated into a single `String`.

But arrays are not the only types which are foldable. `purescript-foldable-traversable` also defines `Foldable` instances for types like `Maybe` and `Tuple`, and other libraries like `purescript-lists` define `Foldable` instances for their own data types. `Foldable` captures the notion of an _ordered container_.

### Functor, and Type Class Laws

The Prelude also defines a collection of type classes which enable a functional style of programming with side-effects in PureScript: `Functor`, `Applicative` and `Monad`. We will cover these abstractions later in the book, but for now, let's look at the definition of the `Functor` type class, which we have seen already in the form of the `map` function:

```haskell
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

The `map` function (and its alias `<$>`) allows a function to be "lifted" over a data structure. The precise definition of the word "lifted" here depends on the data structure in question, but we have already seen its behavior for some simple types:

```text
> import Prelude

> map (\n -> n < 3) [1, 2, 3, 4, 5]
[true, true, false, false, false]

> import Data.Maybe
> import Data.String (length)

> map length (Just "testing")
(Just 7)
```

How can we understand the meaning of the `map` function, when it acts on many different structures, each in a different way?

Well, we can build an intuition that the `map` function applies the function it is given to each element of a container, and builds a new container from the results, with the same shape as the original. But how do we make this concept precise?

Type class instances for `Functor` are expected to adhere to a set of _laws_, called the _functor laws_:

- `map id xs = xs`
- `map g (map f xs) = map (g <<< f) xs`

The first law is the _identity law_. It states that lifting the identity function (the function which returns its argument unchanged) over a structure just returns the original structure. This makes sense since the identity function does not modify its input.

The second law is the _composition law_. It states that mapping one function over a structure, and then mapping a second, is the same thing as mapping the composition of the two functions over the structure.

Whatever "lifting" means in the general sense, it should be true that any reasonable definition of lifting a function over a data structure should obey these rules.

Many standard type classes come with their own set of similar laws. The laws given to a type class give structure to the functions of that type class and allow us to study its instances in generality. The interested reader can research the laws ascribed to the standard type classes that we have seen already.

X> ## Exercises
X>
X> 1. (Easy) The following newtype represents a complex number:
X>
X>     ```haskell
X>     newtype Complex = Complex
X>       { real :: Number
X>       , imaginary :: Number
X>       }
X>     ```
X>       
X>     Define `Show` and `Eq` instances for `Complex`.

## Type Annotations

Types of functions can be constrained by using type classes. Here is an example: suppose we want to write a function which tests if three values are equal, by using equality defined using an `Eq` type class instance.

```haskell
threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3
```

The type declaration looks like an ordinary polymorphic type defined using `forall`. However, there is a type class constraint `Eq a`, separated from the rest of the type by a double arrow `=>`.

This type says that we can call `threeAreEqual` with any choice of type `a`, as long as there is an `Eq` instance available for `a` in one of the imported modules.

Constrained types can contain several type class instances, and the types of the instances are not restricted to simple type variables. Here is another example which uses `Ord` and `Show` instances to compare two values:

```haskell
showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2 | a1 < a2 =
  show a1 <> " is less than " <> show a2
showCompare a1 a2 | a1 > a2 =
  show a1 <> " is greater than " <> show a2
showCompare a1 a2 =
  show a1 <> " is equal to " <> show a2
```

Note that multiple constraints can be specified by using the `=>` symbol multiple times, just like we specify curried functions
of multiple arguments. But remember not to confuse the two symbols:

- `a -> b` denotes the type of functions from _type_ `a` to _type_ `b`, whereas
- `a => b` applies the _constraint_ `a` to the type `b`.

The PureScript compiler will try to infer constrained types when a type annotation is not provided. This can be useful if we want to use the most general type possible for a function.

To see this, try using one of the standard type classes like `Semiring` in PSCi:

```text
> import Prelude

> :type \x -> x + x
forall a. Semiring a => a -> a
```

Here, we might have annotated this function as `Int -> Int`, or `Number -> Number`, but PSCi shows us that the most general type works for any `Semiring`, allowing us to use our function with both `Int`s and `Number`s.

## Overlapping Instances

PureScript has another rule regarding type class instances, called the _overlapping instances rule_. Whenever a type class instance is required at a function call site, PureScript will use the information inferred by the type checker to choose the correct instance. At that time, there should be exactly one appropriate instance for that type. If there are multiple valid instances, the compiler will issue a warning.

To demonstrate this, we can try creating two conflicting type class instances for an example type. In the following code, we create two overlapping `Show` instances for the type `T`:

```haskell
module Overlapped where

import Prelude

data T = T

instance showT1 :: Show T where
  show _ = "Instance 1"

instance showT2 :: Show T where
  show _ = "Instance 2"
```

This module will compile with no warnings. However, if we _use_ `show` at type `T` (requiring the compiler to to find a `Show` instance), the overlapping instances rule will be enforced, resulting in a warning:

```text
Overlapping instances found for Prelude.Show T
```

The overlapping instances rule is enforced so that automatic selection of type class instances is a predictable process. If we allowed two type class instances for a type to exist, then either could be chosen depending on the order of module imports, and that could lead to unpredictable behavior of the program at runtime, which is undesirable.

If it is truly the case that there are two valid type class instances for a type, satisfying the appropriate laws, then a common approach is to define newtypes which wrap the existing type. Since different newtypes are allowed to have different type class instances under the overlapping instances rule, there is no longer an issue. This approach is taken in PureScript's standard libraries, for example in `purescript-maybe`, where the `Maybe a` type has multiple valid instances for the `Monoid` type class.

## Instance Dependencies

Just as the implementation of functions can depend on type class instances using constrained types, so can the implementation of type class instances depend on other type class instances. This provides a powerful form of program inference, in which the implementation of a program can be inferred using its types.

For example, consider the `Show` type class. We can write a type class instance to `show` arrays of elements, as long as we have a way to `show` the elements themselves:

```haskell
instance showArray :: Show a => Show (Array a) where
  ...
```

If a type class instance depends on multiple other instances, those instances should be grouped in parentheses and separated by
commas on the left hand side of the `=>` symbol:

```haskell
instance showEither :: (Show a, Show b) => Show (Either a b) where
  ...
```

These two type class instances are provided in the `purescript-prelude` library.

When the program is compiled, the correct type class instance for `Show` is chosen based on the inferred type of the argument to `show`. The selected instance might depend on many such instance relationships, but this complexity is not exposed to the developer.

X> ## Exercises
X>
X> 1. (Easy) The following declaration defines a type of non-empty arrays of elements of type `a`:
X>
X>     ```haskell
X>     data NonEmpty a = NonEmpty a (Array a)
X>     ```
X>      
X>     Write an `Eq` instance for the type `NonEmpty a` which reuses the instances for `Eq a` and `Eq (Array a)`.
X> 1. (Medium) Write a `Semigroup` instance for `NonEmpty a` by reusing the `Semigroup` instance for `Array`.
X> 1. (Medium) Write a `Functor` instance for `NonEmpty`.
X> 1. (Medium) Given any type `a` with an instance of `Ord`, we can add a new "infinite" value which is greater than any other value:
X>
X>     ```haskell
X>     data Extended a = Finite a | Infinite
X>     ```
X>         
X>     Write an `Ord` instance for `Extended a` which reuses the `Ord` instance for `a`.
X> 1. (Difficult) Write a `Foldable` instance for `NonEmpty`. _Hint_: reuse the `Foldable` instance for arrays.
X> 1. (Difficult) Given an type constructor `f` which defines an ordered container (and so has a `Foldable` instance), we can create a new container type which includes an extra element at the front:
X>
X>     ```haskell
X>     data OneMore f a = OneMore a (f a)
X>     ```
X>         
X>     The container `OneMore f` is also has an ordering, where the new element comes before any element of `f`. Write a `Foldable` instance for `OneMore f`:
X>   
X>     ```haskell
X>     instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
X>       ...
X>     ```

## Multi Parameter Type Classes

It's not the case that a type class can only take a single type as an argument. This is the most common case, but in fact, a type class can be parameterized by _zero or more_ type arguments.

Let's see an example of a type class with two type arguments.

```haskell
module Stream where

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.String as String

class Stream stream element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons
```

The `Stream` module defines a class `Stream` which identifies types which look like streams of elements, where elements can be pulled from the front of the stream using the `uncons` function.

Note that the `Stream` type class is parameterized not only by the type of the stream itself, but also by its elements. This allows us to define type class instances for the same stream type but different element types.

The module defines two type class instances: an instance for arrays, where `uncons` removes the head element of the array using pattern matching, and an instance for String, which removes the first character from a String.

We can write functions which work over arbitrary streams. For example, here is a function which accumulates a result in some `Monoid` based on the elements of a stream:

```haskell
import Prelude
import Data.Monoid (class Monoid, mempty)

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail
```

Try using `foldStream` in PSCi for different types of `Stream` and different types of `Monoid`.

## Functional Dependencies

Multi-parameter type classes can be very useful, but can easily lead to confusing types and even issues with type inference. As a simple example, consider writing a generic `tail` function on streams using the `Stream` class given above:

```haskell
genericTail xs = map _.tail (uncons xs)
```

This gives a somewhat confusing error message:

```text
The inferred type

  forall stream a. Stream stream a => stream -> Maybe stream

has type variables which are not mentioned in the body of the type. Consider adding a type annotation.
```

The problem is that the `genericTail` function does not use the `element` type mentioned in the definition of the `Stream` type class, so that type is left unsolved.

Worse still, we cannot even use `genericTail` by applying it to a specific type of stream:

```text
> map _.tail (uncons "testing")

The inferred type

  forall a. Stream String a => Maybe String

has type variables which are not mentioned in the body of the type. Consider adding a type annotation.
```

Here, we might expect the compiler to choose the `streamString` instance. After all, a `String` is a stream of `Char`s, and cannot be a stream of any other type of elements.

The compiler is unable to make that deduction automatically, and cannot commit to the `streamString` instance. However, we can help the compiler by adding a hint to the type class definition:

```haskell
class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }
```

Here, `stream -> element` is called a _functional dependency_. A functional dependency asserts a functional relationship between the type arguments of a multi-parameter type class. This functional dependency tells the compiler that there is a function from stream types to (unique) element types, so if the compiler knows the stream type, then it can commit to the element type.

This hint is enough for the compiler to infer the correct type for our generic tail function above:

```text
> :type genericTail
forall stream element. Stream stream element => stream -> Maybe stream

> genericTail "testing"
(Just "esting")
```

Functional dependencies can be quite useful when using multi-parameter type classes to design certain APIs.

## Nullary Type Classes

We can even define type classes with zero type arguments! These correspond to compile-time assertions about our functions, allowing us to track global properties of our code in the type system.

An important example is the `Partial` class which we saw earlier when discussing partial functions. We've seen the partial functions `head` and `tail`, defined in `Data.Array.Partial` already:

```haskell
head :: forall a. Partial => Array a -> a

tail :: forall a. Partial => Array a -> Array a
```

Note that there is no instance defined for the `Partial` type class! Doing so would defeat its purpose: attempting to use the `head` function directly will result in a type error:

```text
> head [1, 2, 3]

No type class instance was found for

  Prim.Partial
```

Instead, we can republish the `Partial` constraint for any functions making use of partial functions:

```haskell
secondElement :: forall a. Partial => Array a -> a
secondElement xs = head (tail xs)
```

We've already seen the `unsafePartial` function, which allows us to treat a partial function as a regular function (unsafely). This function is defined in the `Partial.Unsafe` module:

```haskell
unsafePartial :: forall a. (Partial => a) -> a
```

Note that the `Partial` constraint appears _inside the parentheses_ on the left of the function arrow, but not in the outer `forall`. That is, `unsafePartial` is a function from partial values to regular values.

## Superclasses

Just as we can express relationships between type class instances by making an instance dependent on another instance, we can express relationships between type classes themselves using so-called _superclasses_.

We say that one type class is a superclass of another if every instance of the second class is required to be an instance of the first, and we indicate a superclass relationship in the class definition by using a backwards facing double arrow.

We've already seen some examples of superclass relationships: the `Eq` class is a superclass of `Ord`, and the `Semigroup` class is a superclass of `Monoid`. For every type class instance of the `Ord` class, there must be a corresponding `Eq` instance for the same type. This makes sense, since in many cases, when the `compare` function reports that two values are incomparable, we often want to use the `Eq` class to determine if they are in fact equal.

In general, it makes sense to define a superclass relationship when the laws for the subclass mention the members of the superclass. For example, it is reasonable to assume, for any pair of `Ord` and `Eq` instances, that if two values are equal under the `Eq` instance, then the `compare` function should return `EQ`. In order words, `a == b` should be true exactly when `compare a b` evaluates to `EQ`. This relationship on the level of laws justifies the superclass relationship between `Eq` and `Ord`.

Another reason to define a superclass relationship is in the case where there is a clear "is-a" relationship between the two classes. That is, every member of the subclass _is a_ member of the superclass as well.

X> ## Exercises
X>
X> 1. (Medium) Define a partial function which finds the maximum of a non-empty array of integers. Your function should have type `Partial => Array Int -> Int`. Test out your function in PSCi using `unsafePartial`. _Hint_: Use the `maximum` function from `Data.Foldable`.
X> 1. (Medium) The `Action` class is a multi-parameter type class which defines an action of one type on another:
X>
X>     ```haskell
X>     class Monoid m <= Action m a where
X>       act :: m -> a -> a
X>     ```
X>           
X>     An _action_ is a function which describes how monoidal values can be used to modify a value of another type. There are two laws for the `Action` type class:
X>
X>     - `act mempty a = a`
X>     - `act (m1 <> m2) a = act m1 (act m2 a)`
X>
X>     That is, the action respects the operations defined by the `Monoid` class.
X>        
X>     For example, the natural numbers form a monoid under multiplication:
X>
X>     ```haskell
X>     newtype Multiply = Multiply Int
X>
X>     instance semigroupMultiply :: Semigroup Multiply where
X>       append (Multiply n) (Multiply m) = Multiply (n * m)
X>
X>     instance monoidMultiply :: Monoid Multiply where
X>       mempty = Multiply 1
X>     ```
X>
X>     This monoid acts on strings by repeating an input string some number of times. Write an instance which implements this action:
X>
X>     ```haskell
X>     instance repeatAction :: Action Multiply String
X>     ```
X>
X>     Does this instance satisfy the laws listed above?
X> 1. (Medium) Write an instance `Action m a => Action m (Array a)`, where the action on arrays is defined by acting on each array element independently.
X> 1. (Difficult) Given the following newtype, write an instance for `Action m (Self m)`, where the monoid `m` acts on itself using `append`:
X>
X>     ```haskell
X>     newtype Self m = Self m
X>     ```
X> 1. (Difficult) Should the arguments of the multi-parameter type class `Action` be related by some functional dependency? Why or why not?

## A Type Class for Hashes

In the last section of this chapter, we will use the lessons from the rest of the chapter to create a library for hashing data structures.

Note that this library is for demonstration purposes only, and is not intended to provide a robust hashing mechanism.

What properties might we expect of a hash function?

- A hash function should be deterministic, and map equal values to equal hash codes.
- A hash function should distribute its results approximately uniformly over some set of hash codes.

The first property looks a lot like a law for a type class, whereas the second property is more along the lines of an informal contract, and certainly would not be enforceable by PureScript's type system. However, this should provide the intuition for the following type class:

```haskell
newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode
```

with the associated law that `a == b` implies `hash a == hash b`.

We'll spend the rest of this section building a library of instances and functions associated with the `Hashable` type class.

We will need a way to combine hash codes in a deterministic way:

```haskell
combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)
```

The `combineHashes` function will mix two hash codes and redistribute the result over the interval 0-65535.

Let's write a function which uses the `Hashable` constraint to restrict the types of its inputs. One common task which requires a hashing function is to determine if two values hash to the same hash code. The `hashEqual` relation provides such a capability:

```haskell
hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash
```

This function uses the `on` function from `Data.Function` to define hash-equality in terms of equality of hash codes, and should read like a declarative definition of hash-equality: two values are "hash-equal" if they are equal after each value has been passed through the `hash` function.

Let's write some `Hashable` instances for some primitive types. Let's start with an instance for integers. Since a `HashCode` is really just a wrapped integer, this is simple - we can use the `hashCode` helper function:

```haskell
instance hashInt :: Hashable Int where
  hash = hashCode
```

We can also define a simple instance for `Boolean` values using pattern matching:

```haskell
instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1
```

With an instance for hashing integers, we can create an instance for hashing `Char`s by using the `toCharCode` function from `Data.Char`:

```haskell
instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode
```

To define an instance for arrays, we can `map` the `hash` function over the elements of the array (if the element type is also an instance of `Hashable`) and then perform a left fold over the resulting hashes using the `combineHashes` function:

```haskell
instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash
```

Notice how we build up instances using the simpler instances we have already written. Let's use our new `Array` instance to define an instance for `String`s, by turning a `String` into an array of `Char`s:

```haskell
instance hashString :: Hashable String where
  hash = hash <<< toCharArray
```

How can we prove that these `Hashable` instances satisfy the type class law that we stated above? We need to make sure that equal values have equal hash codes. In cases like `Int`, `Char`, `String` and `Boolean`, this is simple because there are no values of those types which are equal in the sense of `Eq` but not equal identically.

What about some more interesting types? To prove the type class law for the `Array` instance, we can use induction on the length of the array. The only array with length zero is `[]`. Any two non-empty arrays are equal only if they have equals head elements and equal tails, by the definition of `Eq` on arrays. By the inductive hypothesis, the tails have equal hashes, and we know that the head elements have equal hashes if the `Hashable a` instance must satisfy the law. Therefore, the two arrays have equal hashes, and so the `Hashable (Array a)` obeys the type class law as well.

The source code for this chapter includes several other examples of `Hashable` instances, such as instances for the `Maybe` and `Tuple` type.

X> ## Exercises
X>
X> 1. (Easy) Use PSCi to test the hash functions for each of the defined instances.
X> 1. (Medium) Use the `hashEqual` function to write a function which tests if an array has any duplicate elements, using hash-equality as an approximation to value equality. Remember to check for value equality using `==` if a duplicate pair is found. _Hint_: the `nubBy` function in `Data.Array` should make this task much simpler.
X> 1. (Medium) Write a `Hashable` instance for the following newtype which satisfies the type class law:
X>
X>     ```haskell
X>     newtype Hour = Hour Int
X>     
X>     instance eqHour :: Eq Hour where
X>       eq (Hour n) (Hour m) = mod n 12 == mod m 12
X>     ```
X>     
X>     The newtype `Hour` and its `Eq` instance represent the type of integers modulo 12, so that 1 and 13 are identified as equal, for example. Prove that the type class law holds for your instance.
X> 1. (Difficult) Prove the type class laws for the `Hashable` instances for `Maybe`, `Either` and `Tuple`.

## Conclusion

In this chapter, we've been introduced to _type classes_, a type-oriented form of abstraction which enables powerful forms of code reuse. We've seen a collection of standard type classes from the PureScript standard libraries, and defined our own library based on a type class for computing hash codes.

This chapter also gave an introduction to the notion of type class laws, a technique for proving properties about code which uses type classes for abstraction. Type class laws are part of a larger subject called _equational reasoning_, in which the properties of a programming language and its type system are used to enable logical reasoning about its programs. This is an important idea, and will be a theme which we will return to throughout the rest of the book.
