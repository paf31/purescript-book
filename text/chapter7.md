# Applicative Validation

## Chapter Goals

In this chapter, we will meet an important new abstraction - the _applicative functor_, described by the `Applicative` type class. Don't worry if the name sounds confusing - we will motivate the concept with a practical example - validating form data. This technique allows us to convert code which usually involves a lot of boilerplate checking into a simple, declarative description of our form.

We will also meet another type class, `Traversable`, which describes _traversable functors_, and see how this concept also arises very naturally from solutions to real-world problems.

The example code for this chapter will be a continuation of the address book example from chapter 3. This time, we will extend our address book data types, and write functions to validate values for those types. The understanding is that these functions could be used, for example in a web user interface, to display errors to the user as part of a data entry form.

## Project Setup

The source code for this chapter is defined in the files `src/Data/AddressBook.purs` and `src/Data/AddressBook/Validation.purs`.

The project has a number of Bower dependencies, many of which we have seen before. There are two new dependencies:

- `purescript-control`, which defines functions for abstracting control flow using type classes like `Applicative`.
- `purescript-validation`, which defines a functor for _applicative validation_, the subject of this chapter.

The `Data.AddressBook` module defines data types and `Show` instances for the types in our project, and the `Data.AddressBook.Validation` module contains validation rules for those types.

## Generalizing Function Application

To explain the concept of an _applicative functor_, let's consider the type constructor `Maybe` that we met earlier.

The source code for this module defines a function `address` which has the following type:

```haskell
address :: String -> String -> String -> Address
```

This function is used to construct a value of type `Address` from three strings: a street name, a city, and a state.

We can apply this function easily and see the result in PSCi:

```text
> import Data.AddressBook

> address "123 Fake St." "Faketown" "CA"
Address { street: "123 Fake St.", city: "Faketown", state: "CA" }
```

However, suppose we did not necessarily have a street, city, or state, and wanted to use the `Maybe` type to indicate a missing value in each of the three cases.

In one case, we might have a missing city. If we try to apply our function directly, we will receive an error from the type checker:

```text
> import Data.Maybe
> address (Just "123 Fake St.") Nothing (Just "CA")

Could not match type

  Maybe String

with type

  String
```

Of course, this is an expected type error - `address` takes strings as arguments, not values of type `Maybe String`.

However, it is reasonable to expect that we should be able to "lift" the `address` function to work with optional values described by the `Maybe` type. In fact, we can, and the `Control.Apply` provides the function `lift3` function which does exactly what we need:

```text
> import Control.Apply
> lift3 address (Just "123 Fake St.") Nothing (Just "CA")

Nothing
```

In this case, the result is `Nothing`, because one of the arguments (the city) was missing. If we provide all three arguments using the `Just` constructor, then the result will contain a value as well:

```text
> lift3 address (Just "123 Fake St.") (Just "Faketown") (Just "CA")

Just (Address { street: "123 Fake St.", city: "Faketown", state: "CA" })
```

The name of the function `lift3` indicates that it can be used to lift functions of 3 arguments. There are similar functions defined in `Control.Apply` for functions of other numbers of arguments.

## Lifting Arbitrary Functions

So, we can lift functions with small numbers of arguments by using `lift2`, `lift3`, etc. But how can we generalize this to arbitrary functions?

It is instructive to look at the type of `lift3`:

```text
> :type lift3
forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

In the `Maybe` example above, the type constructor `f` is `Maybe`, so that `lift3` is specialized to the following type:

```haskell
forall a b c d. (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
```

This type says that we can take any function with three arguments, and lift it to give a new function whose argument and result types are wrapped with `Maybe`.

Certainly, this is not possible for any type constructor `f`, so what is it about the `Maybe` type which allowed us to do this? Well, in specializing the type above, we removed a type class constraint on `f` from the `Apply` type class. `Apply` is defined in the Prelude as follows:

```haskell
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
```

The `Apply` type class is a subclass of `Functor`, and defines an additional function `apply`. As `<$>` was defined as an alias for `map`, the `Prelude` module defines `<*>` as an alias for `apply`. As we'll see, these two operators are often used together.

The type of `apply` looks a lot like the type of `map`. The difference between `map` and `apply` is that `map` takes a function as an argument, whereas the first argument to `apply` is wrapped in the type constructor `f`. We'll see how this is used soon, but first, let's see how to implement the `Apply` type class for the `Maybe` type:

```haskell
instance functorMaybe :: Functor Maybe where
  map f (Just a) = Just (f a)
  map f Nothing  = Nothing

instance applyMaybe :: Apply Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply _        _        = Nothing
```

This type class instance says that we can apply an optional function to an optional value, and the result is defined only if both are defined.

Now we'll see how `map` and `apply` can be used together to lift functions of arbitrary number of arguments.

For functions of one argument, we can just use `map` directly.

For functions of two arguments, we have a curried function `g` with type `a -> b -> c`, say. This is equivalent to the type `a -> (b -> c)`, so we can apply `map` to `g` to get a new function of type `f a -> f (b -> c)` for any type constructor `f` with a `Functor` instance. Partially applying this function to the first lifted argument (of type `f a`), we get a new wrapped function of type `f (b -> c)`. If we also have an `Apply` instance for `f`, then we can then use `apply` to apply the second lifted argument (of type `f b`) to get our final value of type `f c`.

Putting this all together, we see that if we have values `x :: f a` and `y :: f b`, then the expression `(g <$> x) <*> y` has type `f c` (remember, this expression is equivalent to `apply (map g x) y`). The precedence rules defined in the Prelude allow us to remove the parentheses: `g <$> x <*> y`.

In general, we can use `<$>` on the first argument, and `<*>` for the remaining arguments, as illustrated here for `lift3`:

```haskell
lift3 :: forall a b c d f
       . Apply f
      => (a -> b -> c -> d)
      -> f a
      -> f b
      -> f c
      -> f d
lift3 f x y z = f <$> x <*> y <*> z
```

It is left as an exercise for the reader to verify the types involved in this expression.

As an example, we can try lifting the address function over `Maybe`, directly using the `<$>` and `<*>` functions:

```text
> address <$> Just "123 Fake St." <*> Just "Faketown" <*> Just "CA"
Just (Address { street: "123 Fake St.", city: "Faketown", state: "CA" })

> address <$> Just "123 Fake St." <*> Nothing <*> Just "CA"
Nothing
```

Try lifting some other functions of various numbers of arguments over `Maybe` in this way.

## The Applicative Type Class

There is a related type class called `Applicative`, defined as follows:

```haskell
class Apply f <= Applicative f where
  pure :: forall a. a -> f a
```

`Applicative` is a subclass of `Apply` and defines the `pure` function. `pure` takes a value and returns a value whose type has been wrapped with the type constructor `f`.

Here is the `Applicative` instance for `Maybe`:

```haskell
instance applicativeMaybe :: Applicative Maybe where
  pure x = Just x
```

If we think of applicative functors as functors which allow lifting of functions, then `pure` can be thought of as lifting functions of zero arguments.

## Intuition for Applicative

Functions in PureScript are pure and do not support side-effects. Applicative functors allow us to work in larger "programming languages" which support some sort of side-effect encoded by the functor `f`.

As an example, the functor `Maybe` represents the side effect of possibly-missing values. Some other examples include `Either err`, which represents the side effect of possible errors of type `err`, and the arrow functor `r ->` which represents the side-effect of reading from a global configuration. For now, we'll only consider the `Maybe` functor.

If the functor `f` represents this larger programming language with effects, then the `Apply` and `Applicative` instances allow us to lift values and function applications from our smaller programming language (PureScript) into the new language.

`pure` lifts pure (side-effect free) values into the larger language, and for functions, we can use `map` and `apply` as described above.

This raises a question: if we can use `Applicative` to embed PureScript functions and values into this new language, then how is the new language any larger? The answer depends on the functor `f`. If we can find expressions of type `f a` which cannot be expressed as `pure x` for some `x`, then that expression represents a term which only exists in the larger language.

When `f` is `Maybe`, an example is the expression `Nothing`: we cannot write `Nothing` as `pure x` for any `x`. Therefore, we can think of PureScript as having been enlarged to include the new term `Nothing`, which represents a missing value.

## More Effects

Let's see some more examples of lifting functions over different `Applicative` functors.

Here is a simple example function defined in PSCi, which joins three names to form a full name:

```text
> import Prelude

> fullName first middle last = last <> ", " <> first <> " " <> middle

> fullName "Phillip" "A" "Freeman"
Freeman, Phillip A
```

Now suppose that this function forms the implementation of a (very simple!) web service with the three arguments provided as query parameters. We want to make sure that the user provided each of the three parameters, so we might use the `Maybe` type to indicate the presence or otherwise of a parameter. We can lift `fullName` over `Maybe` to create an implementation of the web service which checks for missing parameters:

```text
> import Data.Maybe

> fullName <$> Just "Phillip" <*> Just "A" <*> Just "Freeman"
Just ("Freeman, Phillip A")

> fullName <$> Just "Phillip" <*> Nothing <*> Just "Freeman"
Nothing
```

Note that the lifted function returns `Nothing` if any of the arguments was `Nothing`.

This is good, because now we can send an error response back from our web service if the parameters are invalid. However, it would be better if we could indicate which field was incorrect in the response.

Instead of lifting over `Maybe`, we can lift over `Either String`, which allows us to return an error message. First, let's write an operator to convert optional inputs into computations which can signal an error using `Either String`:

```text
> :paste
… withError Nothing  err = Left err
… withError (Just a) _   = Right a
… ^D
```

_Note_: In the `Either err` applicative functor, the `Left` constructor indicates an error, and the `Right` constructor indicates success.

Now we can lift over `Either String`, providing an appropriate error message for each parameter:

```text
> :paste
… fullNameEither first middle last =
…   fullName <$> (first  `withError` "First name was missing")
…            <*> (middle `withError` "Middle name was missing")
…            <*> (last   `withError` "Last name was missing")
… ^D

> :type fullNameEither
Maybe String -> Maybe String -> Maybe String -> Either String String
```

Now our function takes three optional arguments using `Maybe`, and returns either a `String` error message or a `String` result.

We can try out the function with different inputs:

```text
> fullNameEither (Just "Phillip") (Just "A") (Just "Freeman")
(Right "Freeman, Phillip A")

> fullNameEither (Just "Phillip") Nothing (Just "Freeman")
(Left "Middle name was missing")

> fullNameEither (Just "Phillip") (Just "A") Nothing
(Left "Last name was missing")
```

In this case, we see the error message corresponding to the first missing field, or a successful result if every field was provided. However, if we are missing multiple inputs, we still only see the first error:

```text
> fullNameEither Nothing Nothing Nothing
(Left "First name was missing")
```

This might be good enough, but if we want to see a list of _all_ missing fields in the error, then we need something more powerful than `Either String`. We will see a solution later in this chapter.

## Combining Effects

As an example of working with applicative functors abstractly, this section will show how to write a function which will generically combine side-effects encoded by an applicative functor `f`.

What does this mean? Well, suppose we have a list of wrapped arguments of type `f a` for some `a`. That is, suppose we have an list of type `List (f a)`. Intuitively, this represents a list of computations with side-effects tracked by `f`, each with return type `a`. If we could run all of these computations in order, we would obtain a list of results of type `List a`. However, we would still have side-effects tracked by `f`. That is, we expect to be able to turn something of type `List (f a)` into something of type `f (List a)` by "combining" the effects inside the original list.

For any fixed list size `n`, there is a function of `n` arguments which builds a list of size `n` out of those arguments. For example, if `n` is `3`, the function is `\x y z -> x : y : z : Nil`. This function has type `a -> a -> a -> List a`. We can use the `Applicative` instance for `List` to lift this function over `f`, to get a function of type `f a -> f a -> f a -> f (List a)`. But, since we can do this for any `n`, it makes sense that we should be able to perform the same lifting for any _list_ of arguments.

That means that we should be able to write a function

```haskell
combineList :: forall f a. Applicative f => List (f a) -> f (List a)
```

This function will take a list of arguments, which possibly have side-effects, and return a single wrapped list, applying the side-effects of each.

To write this function, we'll consider the length of the list of arguments. If the list is empty, then we do not need to perform any effects, and we can use `pure` to simply return an empty list:

```haskell
combineList Nil = pure Nil
```

In fact, this is the only thing we can do!

If the list is non-empty, then we have a head element, which is a wrapped argument of type `f a`, and a tail of type `List (f a)`. We can recursively combine the effects in the tail, giving a result of type `f (List a)`. We can then use `<$>` and `<*>` to lift the `Cons` constructor over the head and new tail:

```haskell
combineList (Cons x xs) = Cons <$> x <*> combineList xs
```

Again, this was the only sensible implementation, based on the types we were given.

We can test this function in PSCi, using the `Maybe` type constructor as an example:

```text
> import Data.List
> import Data.Maybe

> combineList (fromFoldable [Just 1, Just 2, Just 3])
(Just (Cons 1 (Cons 2 (Cons 3 Nil))))

> combineList (fromFoldable [Just 1, Nothing, Just 2])
Nothing
```

When specialized to `Maybe`, our function returns a `Just` only if every list element was `Just`, otherwise it returns `Nothing`. This is consistent with our intuition of working in a larger language supporting optional values - a list of computations which return optional results only has a result itself if every computation contained a result.

But the `combineList` function works for any `Applicative`! We can use it to combine computations which possibly signal an error using `Either err`, or which read from a global configuration using `r ->`.

We will see the `combineList` function again later, when we consider `Traversable` functors.

X> ## Exercises
X>
X> 1. (Easy) Use `lift2` to write lifted versions of the numeric operators `+`, `-`, `*` and `/` which work with optional arguments.
X> 1. (Medium) Convince yourself that the definition of `lift3` given above in terms of `<$>` and `<*>` does type check.
X> 1. (Difficult) Write a function `combineMaybe` which has type `forall a f. Applicative f => Maybe (f a) -> f (Maybe a)`. This function takes an optional computation with side-effects, and returns a side-effecting computation which has an optional result.

## Applicative Validation

The source code for this chapter defines several data types which might be used in an address book application. The details are omitted here, but the key functions which are exported by the `Data.AddressBook` module have the following types:

```haskell
address :: String -> String -> String -> Address

phoneNumber :: PhoneType -> String -> PhoneNumber

person :: String -> String -> Address -> Array PhoneNumber -> Person
```

where `PhoneType` is defined as an algebraic data type:

```haskell
data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone
```

These functions can be used to construct a `Person` representing an address book entry. For example, the following value is defined in `Data.AddressBook`:

```haskell
examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
  	     [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
  	     ]
```

Test this value in PSCi (this result has been formatted):

```text
> import Data.AddressBook

> examplePerson
Person
  { firstName: "John",
  , lastName: "Smith",
  , address: Address
      { street: "123 Fake St."
      , city: "FakeTown"
      , state: "CA"
      },
  , phones: [ PhoneNumber
                { type: HomePhone
                , number: "555-555-5555"
                }
            , PhoneNumber
                { type: CellPhone
                , number: "555-555-0000"
                }
            ]
  }  
```

We saw in a previous section how we could use the `Either String` functor to validate a data structure of type `Person`. For example, provided functions to validate the two names in the structure, we might validate the entire data structure as follows:

```haskell
nonEmpty :: String -> Either String Unit
nonEmpty "" = Left "Field cannot be empty"
nonEmpty _  = Right unit

validatePerson :: Person -> Either String Person
validatePerson (Person o) =
  person <$> (nonEmpty o.firstName *> pure o.firstName)
         <*> (nonEmpty o.lastName  *> pure o.lastName)
         <*> pure o.address
         <*> pure o.phones
```

In the first two lines, we use the `nonEmpty` function to validate a non-empty string. `nonEmpty` returns an error (indicated with the `Left` constructor) if its input is empty, or a successful empty value (`unit`) using the `Right` constructor otherwise. We use the sequencing operator `*>` to indicate that we want to perform two validations, returning the result from the validator on the right. In this case, the validator on the right simply uses `pure` to return the input unchanged.

The final lines do not perform any validation but simply provide the `address` and `phones` fields to the `person` function as the remaining arguments.

This function can be seen to work in PSCi, but has a limitation which we have seen before:

```haskell
> validatePerson $ person "" "" (address "" "" "") []
(Left "Field cannot be empty")
```

The `Either String` applicative functor only provides the first error encountered. Given the input here, we would prefer to see two errors - one for the missing first name, and a second for the missing last name.

There is another applicative functor which is provided by the `purescript-validation` library. This functor is called `V`, and it provides the ability to return errors in any _semigroup_. For example, we can use `V (Array String)` to return an array of `String`s as errors, concatenating new errors onto the end of the array.

The `Data.AddressBook.Validation` module uses the `V (Array String)` applicative functor to validate the data structures in the `Data.AddressBook` module.

Here is an example of a validator taken from the `Data.AddressBook.Validation` module:

```haskell
type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

lengthIs :: String -> Number -> String -> V Errors Unit
lengthIs field len value | S.length value /= len =
  invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     =
  pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)
```

`validateAddress` validates an `Address` structure. It checks that the `street` and `city` fields are non-empty, and checks that the string in the `state` field has length 2.

Notice how the `nonEmpty` and `lengthIs` validator functions both use the `invalid` function provided by the `Data.Validation` module to indicate an error. Since we are working in the `Array String` semigroup, `invalid` takes an array of strings as its argument.

We can try this function in PSCi:

```text
> import Data.AddressBook
> import Data.AddressBook.Validation

> validateAddress $ address "" "" ""
(Invalid [ "Field 'Street' cannot be empty"
         , "Field 'City' cannot be empty"
         , "Field 'State' must have length 2"
         ])

> validateAddress $ address "" "" "CA"
(Invalid [ "Field 'Street' cannot be empty"
         , "Field 'City' cannot be empty"
         ])
```

This time, we receive an array of all validation errors.

## Regular Expression Validators

The `validatePhoneNumber` function uses a regular expression to validate the form of its argument. The key is a `matches` validation function, which uses a `Regex` from the `Data.String.Regex` module to validate its input:

```haskell
matches :: String -> R.Regex -> String -> V Errors Unit
matches _     regex value | R.test regex value =
  pure unit
matches field _     _     =
  invalid ["Field '" <> field <> "' did not match the required format"]
```

Again, notice how `pure` is used to indicate successful validation, and `invalid` is used to signal an array of errors.

`validatePhoneNumber` is built from the `matches` function in the same way as before:

```haskell
validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)
```

Again, try running this validator against some valid and invalid inputs in PSCi:

```text
> validatePhoneNumber $ phoneNumber HomePhone "555-555-5555"
Valid (PhoneNumber { type: HomePhone, number: "555-555-5555" })

> validatePhoneNumber $ phoneNumber HomePhone "555.555.5555"
Invalid (["Field 'Number' did not match the required format"])
```

X> ## Exercises
X>
X> 1. (Easy) Use a regular expression validator to ensure that the `state` field of the `Address` type contains two alphabetic characters. _Hint_: see the source code for `phoneNumberRegex`.
X> 1. (Medium) Using the `matches` validator, write a validation function which checks that a string is not entirely whitespace. Use it to replace `nonEmpty` where appropriate.

## Traversable Functors

The remaining validator is `validatePerson`, which combines the validators we have seen so far to validate an entire `Person` structure:

```haskell
arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] =
  invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  =
  pure unit

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *>
              pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *>
              pure o.lastName)
	       <*> validateAddress o.address
         <*> (arrayNonEmpty "Phone Numbers" o.phones *>
              traverse validatePhoneNumber o.phones)
```

There is one more interesting function here, which we haven't seen yet - `traverse`, which appears in the final line.

`traverse` is defined in the `Data.Traversable` module, in the `Traversable` type class:

```haskell
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b f. Applicative f => (a -> f b) -> t a -> f (t b)
  sequence :: forall a f. Applicative f => t (f a) -> f (t a)
```

`Traversable` defines the class of _traversable functors_. The types of its functions might look a little intimidating, but `validatePerson` provides a good motivating example.

Every traversable functor is both a `Functor` and `Foldable` (recall that a _foldable functor_ was a type constructor which supported a fold operation, reducing a structure to a single value). In addition, a traversable functor provides the ability to combine a collection of side-effects which depend on its structure.

This may sound complicated, but let's simplify things by specializing to the case of arrays. The array type constructor is traversable, which means that there is a function:

```haskell
traverse :: forall a b f. Applicative f => (a -> f b) -> Array a -> f (Array b)
```

Intuitively, given any applicative functor `f`, and a function which takes a value of type `a` and returns a value of type `b` (with side-effects tracked by `f`), we can apply the function to each element of an array of type `Array a` to obtain a result of type `Array b` (with side-effects tracked by `f`).

Still not clear? Let's specialize further to the case where `m` is the `V Errors` applicative functor above. Now, we have a function of type

```haskell
traverse :: forall a b. (a -> V Errors b) -> Array a -> V Errors (Array b)
```

This type signature says that if we have a validation function `f` for a type `a`, then `traverse f` is a validation function for arrays of type `Array a`. But that's exactly what we need to be able to validate the `phones` field of the `Person` data structure! We pass `validatePhoneNumber` to `traverse` to create a validation function which validates each element successively.

In general, `traverse` walks over the elements of a data structure, performing computations with side-effects and accumulating a result.

The type signature for `Traversable`'s other function `sequence` might look more familiar:

```haskell
sequence :: forall a f. Applicative f => t (f a) -> f (t a)
```

In fact, the `combineList` function that we wrote earlier is just a special case of the `sequence` function from the `Traversable` type class. Setting `t` to be the type constructor `List`, we recover the type of the `combineList` function:

```haskell
combineList :: forall f a. Applicative f => List (f a) -> f (List a)
```

Traversable functors capture the idea of traversing a data structure, collecting a set of effectful computations, and combining their effects. In fact, `sequence` and `traverse` are equally important to the definition of `Traversable` - each can be implemented in terms of each other. This is left as an exercise for the interested reader.

The `Traversable` instance for lists is given in the `Data.List` module. The definition of `traverse` is given here:

```haskell
-- traverse :: forall a b f. Applicative f => (a -> f b) -> List a -> f (List b)
traverse _ Nil = pure Nil
traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
```

In the case of an empty list, we can simply return an empty list using `pure`. If the list is non-empty, we can use the function `f` to create a computation of type `f b` from the head element. We can also call `traverse` recursively on the tail. Finally, we can lift the `Cons` constructor over the applicative functor `f` to combine the two results.

But there are more examples of traversable functors than just arrays and lists. The `Maybe` type constructor we saw earlier also has an instance for `Traversable`. We can try it in PSCi:

```text
> import Data.Maybe
> import Data.Traversable

> traverse (nonEmpty "Example") Nothing
(Valid Nothing)

> traverse (nonEmpty "Example") (Just "")
(Invalid ["Field 'Example' cannot be empty"])

> traverse (nonEmpty "Example") (Just "Testing")
(Valid (Just unit))
```

These examples show that traversing the `Nothing` value returns `Nothing` with no validation, and traversing `Just x` uses the validation function to validate `x`. That is, `traverse` takes a validation function for type `a` and returns a validation function for `Maybe a`, i.e. a validation function for optional values of type `a`.

Other traversable functors include `Array`, and `Tuple a` and `Either a` for any type `a`. Generally, most "container" data type constructors have `Traversable` instances. As an example, the exercises will include writing a `Traversable` instance for a type of binary trees.

X> ## Exercises
X>
X> 1. (Medium) Write a `Traversable` instance for the following binary tree data structure, which combines side-effects from left-to-right:
X>
X>     ```haskell
X>     data Tree a = Leaf | Branch (Tree a) a (Tree a)
X>     ```
X>
X>     This corresponds to an in-order traversal of the tree. What about a preorder traversal? What about reverse order?
X>
X> 1. (Medium) Modify the code to make the `address` field of the `Person` type optional using `Data.Maybe`. _Hint_: Use `traverse` to validate a field of type `Maybe a`.
X> 1. (Difficult) Try to write `sequence` in terms of `traverse`. Can you write `traverse` in terms of `sequence`?

## Applicative Functors for Parallelism

In the discussion above, I chose the word "combine" to describe how applicative functors "combine side-effects". However, in all the examples given, it would be equally valid to say that applicative functors allow us to "sequence" effects. This would be consistent with the intuition that traversable functors provide a `sequence` function to combine effects in sequence based on a data structure.

However, in general, applicative functors are more general than this. The applicative functor laws do not impose any ordering on the side-effects that their computations perform. In fact, it would be valid for an applicative functor to perform its side-effects in parallel.

For example, the `V` validation functor returned an _array_ of errors, but it would work just as well if we picked the `Set` semigroup, in which case it would not matter what order we ran the various validators. We could even run them in parallel over the data structure!

As a second example, the `purescript-parallel` package provides a type class `Parallel` which supports _parallel computations_. `Parallel` provides a function `parallel` which uses some `Applicative` functor to compute the result of its input computation _in parallel_:

```haskell
f <$> parallel computation1
  <*> parallel computation2
```

This computation would start computing values asynchronously using `computation1` and `computation2`. When both results have been computed, they would be combined into a single result using the function `f`.

We will see this idea in more detail when we apply applicative functors to the problem of _callback hell_ later in the book.

Applicative functors are a natural way to capture side-effects which can be combined in parallel.

## Conclusion

In this chapter, we covered a lot of new ideas:

- We introduced the concept of an _applicative functor_ which generalizes the idea of function application to type constructors which capture some notion of side-effect.
- We saw how applicative functors gave a solution to the problem of validating data structures, and how by switching the applicative functor we could change from reporting a single error to reporting all errors across a data structure.
- We met the `Traversable` type class, which encapsulates the idea of a _traversable functor_, or a container whose elements can be used to combine values with side-effects.

Applicative functors are an interesting abstraction which provide neat solutions to a number of problems. We will see them a few more times throughout the book. In this case, the validation applicative functor provided a way to write validators in a declarative style, allowing us to define _what_ our validators should validate and not _how_ they should perform that validation. In general, we will see that applicative functors are a useful tool for the design of _domain specific languages_.

In the next chapter, we will see a related idea, the class of _monads_, and extend our address book example to run in the browser!
