# Functions and Records

## Chapter Goals

This chapter will introduce two building blocks of PureScript programs: functions and records. In addition, we'll see how to structure PureScript programs, and how to use types as an aid to program development.

We will build a simple address book application to manage a list of contacts. This code will introduce some new ideas from the syntax of PureScript.

The front-end of our application will be the interactive mode PSCi, but it would be possible to build on this code to write a front-end in Javascript. In fact, we will do exactly that in later chapters, adding form validation and save/restore functionality.

## Project Setup

The source code for this chapter is contained in the file `src/Data/AddressBook.purs`. This file starts with a module declaration and its import list:

```haskell
module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)
```

Here, we import several modules:

- The `Control.Plus` module, which defines the `empty` value.
- The `Data.List` module, which is provided by the `purescript-lists` package which can be installed using Bower. It contains a few functions which we will need for working with linked lists.
- The `Data.Maybe` module, which defines data types and functions for working with optional values.

Notice that the imports for these modules are listed explicitly in parentheses. This is generally a good practice, as it helps to avoid conflicting imports.

Assuming you have cloned the book's source code repository, the project for this chapter can be built using Pulp, with the following commands:

```text
$ cd chapter3
$ bower update
$ pulp build
```

## Simple Types

PureScript defines three built-in types which correspond to JavaScript's primitive types: numbers, strings and booleans. These are defined in the `Prim` module, which is implicitly imported by every module. They are called `Number`, `String`, and `Boolean`, respectively, and you can see them in PSCi by using the `:type` command to print the types of some simple values:

```text
$ pulp repl

> :type 1.0
Number

> :type "test"
String

> :type true
Boolean
```

PureScript defines some other built-in types: integers, characters, arrays, records, and functions.

Integers are differentiated from floating point values of type `Number` by the lack of a decimal point:

```text
> :type 1
Int
```

Character literals are wrapped in single quotes, unlike string literals which use double quotes:

```text
> :type 'a'
Char
```

Arrays correspond to JavaScript arrays, but unlike in JavaScript, all elements of a PureScript array must have the same type:

```text
> :type [1, 2, 3]
Array Int

> :type [true, false]
Array Boolean

> :type [1, false]
Could not match type Int with Boolean.
```

The error in the last example is an error from the type checker, which unsuccessfully attempted to _unify_ (i.e. make equal) the types of the two elements.

Records correspond to JavaScript's objects, and record literals have the same syntax as JavaScript's object literals:

```text
> author = { name: "Phil", interests: ["Functional Programming", "JavaScript"] }

> :type author
{ name :: String
, interests :: Array String
}
```

This type indicates that the specified object has two _fields_, a `name` field which has type `String`, and an `interests` field, which has type `Array String`, i.e. an array of `String`s.

Fields of records can be accessed using a dot, followed by the label of the field to access:

```text
> author.name
"Phil"

> author.interests
["Functional Programming","JavaScript"]
```

PureScript's functions correspond to JavaScript's functions. The PureScript standard libraries provide plenty of examples of functions, and we will see more in this chapter:

```text
> import Prelude
> :type flip
forall a b c. (a -> b -> c) -> b -> a -> c

> :type const
forall a b. a -> b -> a
```

Functions can be defined at the top-level of a file by specifying arguments before the equals sign:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

Alternatively, functions can be defined inline, by using a backslash character followed by a space-delimited list of argument names. To enter a multi-line declaration in PSCi, we can enter "paste mode" by using the `:paste` command. In this mode, declarations are terminated using the _Control-D_ key sequence:

```text
> :paste
… add :: Int -> Int -> Int
… add = \x y -> x + y
… ^D
```

Having defined this function in PSCi, we can _apply_ it to its arguments by separating the two arguments from the function name by whitespace:

```text
> add 10 20
30
```

## Quantified Types

In the previous section, we saw the types of some functions defined in the Prelude. For example, the `flip` function had the following type:

```text
> :type flip
forall a b c. (a -> b -> c) -> b -> a -> c
```

The keyword `forall` here indicates that `flip` has a _universally quantified type_. It means that we can substitute any types for `a`, `b` and `c`, and `flip` will work with those types.

For example, we might choose the type `a` to be `Int`, `b` to be `String` and `c` to be `String`. In that case we could _specialize_ the type of `flip` to

```text
(Int -> String -> String) -> String -> Int -> String
```

We don't have to indicate in code that we want to specialize a quantified type - it happens automatically. For example, we can just use `flip` as if it had this type already:

```text
> flip (\n s -> show n <> s) "Ten" 10

"10Ten"
```

While we can choose any types for `a`, `b` and `c`, we have to be consistent. The type of the function we passed to `flip` had to be consistent with the types of the other arguments. That is why we passed the string `"Ten"` as the second argument, and the number `10` as the third. It would not work if the arguments were reversed:

```text
> flip (\n s -> show n <> s) 10 "Ten"

Could not match type Int with type String
```

## Notes On Indentation

PureScript code is _indentation-sensitive_, just like Haskell, but unlike JavaScript. This means that the whitespace in your code is not meaningless, but rather is used to group regions of code, just like curly braces in C-like languages.

If a declaration spans multiple lines, then any lines except the first must be indented past the indentation level of the first line.

Therefore, the following is valid PureScript code:

```haskell
add x y z = x +
  y + z
```

But this is not valid code:

```haskell
add x y z = x +
y + z
```

In the second case, the PureScript compiler will try to parse _two_ declarations, one for each line.

Generally, any declarations defined in the same block should be indented at the same level. For example, in PSCi, declarations in a let statement must be indented equally. This is valid:

```text
> :paste
… x = 1
… y = 2
… ^D
```

but this is not:

```text
> :paste
… x = 1
…  y = 2
… ^D
```

Certain PureScript keywords (such as `where`, `of` and `let`) introduce a new block of code, in which declarations must be further-indented:

```haskell
example x y z = foo + bar
  where
    foo = x * y
    bar = y * z
```

Note how the declarations for `foo` and `bar` are indented past the declaration of `example`.

The only exception to this rule is the `where` keyword in the initial `module` declaration at the top of a source file.

## Defining Our Types

A good first step when tackling a new problem in PureScript is to write out type definitions for any values you will be working with. First, let's define a type for records in our address book:

```haskell
type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }
```

This defines a _type synonym_ called `Entry` - the type `Entry` is equivalent to the type on the right of the equals symbol: a record type with three fields - `firstName`, `lastName` and `address`. The two name fields will have type `String`, and the `address` field will have type `Address`, defined as follows:

```haskell
type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }
```

Note that records can contain other records.

Now let's define a third type synonym, for our address book data structure, which will be represented simply as a linked list of entries:

```haskell
type AddressBook = List Entry
```

Note that `List Entry` is not the same as `Array Entry`, which represents an _array_ of entries.

## Type Constructors and Kinds

`List` is an example of a _type constructor_. Values do not have the type `List` directly, but rather `List a` for some type `a`. That is, `List` takes a _type argument_ `a` and _constructs_ a new type `List a`.

Note that just like function application, type constructors are applied to other types simply by juxtaposition: the type `List Entry` is in fact the type constructor `List` _applied_ to the type `Entry` - it represents a list of entries.

If we try to incorrectly define a value of type `List` (by using the type annotation operator `::`), we will see a new type of error:

```text
> import Data.List
> Nil :: List
In a type-annotated expression x :: t, the type t must have kind Type
```

This is a _kind error_. Just like values are distinguished by their _types_, types are distinguished by their _kinds_, and just like ill-typed values result in _type errors_, _ill-kinded_ types result in _kind errors_.

There is a special kind called `Type` which represents the kind of all types which have values, like `Number` and `String`.

There are also kinds for type constructors. For example, the kind `Type -> Type` represents a function from types to types, just like `List`. So the error here occurred because values are expected to have types with kind `Type`, but `List` has kind `Type -> Type`.

To find out the kind of a type, use the `:kind` command in PSCi. For example:

```text
> :kind Number
Type

> import Data.List
> :kind List
Type -> Type

> :kind List String
Type
```

PureScript's _kind system_ supports other interesting kinds, which we will see later in the book.

## Displaying Address Book Entries

Let's write our first function, which will render an address book entry as a string. We start by giving the function a type. This is optional, but good practice, since it acts as a form of documentation. In fact, the PureScript compiler will give a warning if a top-level declaration does not contain a type annotation. A type declaration separates the name of a function from its type with the `::` symbol:

```haskell
showEntry :: Entry -> String
```

This type signature says that `showEntry` is a function, which takes an `Entry` as an argument and returns a `String`. Here is the code for `showEntry`:

```haskell
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address
```

This function concatenates the three fields of the `Entry` record into a single string, using the `showAddress` function to turn the record inside the `address` field into a `String`. `showAddress` is defined similarly:

```haskell
showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state
```

A function definition begins with the name of the function, followed by a list of argument names. The result of the function is specified after the equals sign. Fields are accessed with a dot, followed by the field name. In PureScript, string concatenation uses the diamond operator (`<>`), instead of the plus operator like in Javascript.

## Test Early, Test Often

The PSCi interactive mode allows for rapid prototyping with immediate feedback, so let's use it to verify that our first few functions behave as expected.

First, build the code you've written:

```text
$ pulp build
```

Next, load PSCi, and use the `import` command to import your new module:

```text
$ pulp repl

> import Data.AddressBook
```

We can create an entry by using a record literal, which looks just like an anonymous object in JavaScript. Bind it to a name with a `let` expression:

```text
> address = { street: "123 Fake St.", city: "Faketown", state: "CA" }
```

Now, try applying our function to the example:

```text
> showAddress address

"123 Fake St., Faketown, CA"
```

Let's also test `showEntry` by creating an address book entry record containing our example address:

```text
> entry = { firstName: "John", lastName: "Smith", address: address }
> showEntry entry

"Smith, John: 123 Fake St., Faketown, CA"
```

## Creating Address Books

Now let's write some utility functions for working with address books. We will need a value which represents an empty address book: an empty list.

```haskell
emptyBook :: AddressBook
emptyBook = empty
```

We will also need a function for inserting a value into an existing address book. We will call this function `insertEntry`. Start by giving its type:

```haskell
insertEntry :: Entry -> AddressBook -> AddressBook
```

This type signature says that `insertEntry` takes an `Entry` as its first argument, and an `AddressBook` as a second argument, and returns a new `AddressBook`.

We don't modify the existing `AddressBook` directly. Instead, we return a new `AddressBook` which contains the same data. As such, `AddressBook` is an example of an _immutable data structure_. This is an important idea in PureScript - mutation is a side-effect of code, and inhibits our ability to reason effectively about its behavior, so we prefer pure functions and immutable data where possible.

To implement `insertEntry`, we can use the `Cons` function from `Data.List`. To see its type, open PSCi and use the `:type` command:

```text
$ pulp repl

> import Data.List
> :type Cons

forall a. a -> List a -> List a
```

This type signature says that `Cons` takes a value of some type `a`, and a list of elements of type `a`, and returns a new list with entries of the same type. Let's specialize this with `a` as our `Entry` type:

```haskell
Entry -> List Entry -> List Entry
```

But `List Entry` is the same as `AddressBook`, so this is equivalent to

```haskell
Entry -> AddressBook -> AddressBook
```

In our case, we already have the appropriate inputs: an `Entry`, and a `AddressBook`, so can apply `Cons` and get a new `AddressBook`, which is exactly what we wanted!

Here is our implementation of `insertEntry`:

```haskell
insertEntry entry book = Cons entry book
```

This brings the two arguments `entry` and `book` into scope, on the left hand side of the equals symbol, and then applies the `Cons` function to create the result.

## Curried Functions

Functions in PureScript take exactly one argument. While it looks like the `insertEntry` function takes two arguments, it is in fact an example of a _curried function_.

The `->` operator in the type of `insertEntry` associates to the right, which means that the compiler parses the type as

```haskell
Entry -> (AddressBook -> AddressBook)
```

That is, `insertEntry` is a function which returns a function! It takes a single argument, an `Entry`, and returns a new function, which in turn takes a single `AddressBook` argument and returns a new `AddressBook`.

This means that we can _partially apply_ `insertEntry` by specifying only its first argument, for example. In PSCi, we can see the result type:

```text
> :type insertEntry entry

AddressBook -> AddressBook
```

As expected, the return type was a function. We can apply the resulting function to a second argument:

```text
> :type (insertEntry entry) emptyBook
AddressBook
```

Note though that the parentheses here are unnecessary - the following is equivalent:

```text
> :type insertEntry example emptyBook
AddressBook
```

This is because function application associates to the left, and this explains why we can just specify function arguments one after the other, separated by whitespace.

Note that in the rest of the book, I will talk about things like "functions of two arguments". However, it is to be understood that this means a curried function, taking a first argument and returning another function.

Now consider the definition of `insertEntry`:

```haskell
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book
```

If we explicitly parenthesize the right-hand side, we get `(Cons entry) book`. That is, `insertEntry entry` is a function whose argument is just passed along to the `(Cons entry)` function. But if two functions have the same result for every input, then they are the same function! So we can remove the argument `book` from both sides:

```haskell
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry = Cons entry
```

But now, by the same argument, we can remove `entry` from both sides:

```haskell
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons
```

This process is called _eta conversion_, and can be used (along with some other techniques) to rewrite functions in _point-free form_, which means functions defined without reference to their arguments.

In the case of `insertEntry`, _eta conversion_ has resulted in a very clear definition of our function - "`insertEntry` is just cons on lists". However, it is arguable whether point-free form is better in general.

## Querying the Address Book

The last function we need to implement for our minimal address book application will look up a person by name and return the correct `Entry`. This will be a nice application of building programs by composing small functions - a key idea from functional programming.

We can first filter the address book, keeping only those entries with the correct first and last names. Then we can simply return the head (i.e. first) element of the resulting list.

With this high-level specification of our approach, we can calculate the type of our function. First open PSCi, and find the types of the `filter` and `head` functions:

```text
$ pulp repl

> import Data.List
> :type filter

forall a. (a -> Boolean) -> List a -> List a

> :type head

forall a. List a -> Maybe a
```

Let's pick apart these two types to understand their meaning.

`filter` is a curried function of two arguments. Its first argument is a function, which takes a list element and returns a `Boolean` value as a result. Its second argument is a list of elements, and the return value is another list.

`head` takes a list as its argument, and returns a type we haven't seen before: `Maybe a`. `Maybe a` represents an optional value of type `a`, and provides a type-safe alternative to using `null` to indicate a missing value in languages like Javascript. We will see it again in more detail in later chapters.

The universally quantified types of `filter` and `head` can be _specialized_ by the PureScript compiler, to the following types:

```haskell
filter :: (Entry -> Boolean) -> AddressBook -> AddressBook

head :: AddressBook -> Maybe Entry
```

We know that we will need to pass the first and last names that we want to search for, as arguments to our function.

We also know that we will need a function to pass to `filter`. Let's call this function `filterEntry`. `filterEntry` will have type `Entry -> Boolean`. The application `filter filterEntry` will then have type `AddressBook -> AddressBook`. If we pass the result of this function to the `head` function, we get our result of type `Maybe Entry`.

Putting these facts together, a reasonable type signature for our function, which we will call `findEntry`, is:

```haskell
findEntry :: String -> String -> AddressBook -> Maybe Entry
```

This type signature says that `findEntry` takes two strings, the first and last names, and a `AddressBook`, and returns an optional `Entry`. The optional result will contain a value only if the name is found in the address book.

And here is the definition of `findEntry`:

```haskell
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
```

Let's go over this code step by step.

`findEntry` brings three names into scope: `firstName`, and `lastName`, both representing strings, and `book`, an `AddressBook`.

The right hand side of the definition combines the `filter` and `head` functions: first, the list of entries is filtered, and the `head` function is applied to the result.

The predicate function `filterEntry` is defined as an auxiliary declaration inside a `where` clause. This way, the `filterEntry` function is available inside the definition of our function, but not outside it. Also, it can depend on the arguments to the enclosing function, which is essential here because `filterEntry` uses the `firstName` and `lastName` arguments to filter the specified `Entry`.

Note that, just like for top-level declarations, it was not necessary to specify a type signature for `filterEntry`. However, doing so is recommended as a form of documentation.

## Infix Function Application

In the code for `findEntry` above, we used a different form of function application: the `head` function was applied to the expression `filter filterEntry book` by using the infix `$` symbol.

This is equivalent to the usual application `head (filter filterEntry book)`

`($)` is just an alias for a regular function called `apply`, which is defined in the Prelude. It is defined as follows:

```haskell
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $
```

So `apply` takes a function and a value and applies the function to the value. The `infixr` keyword is used to define `($)` as an alias for `apply`.

But why would we want to use `$` instead of regular function application? The reason is that `$` is a right-associative, low precedence operator. This means that `$` allows us to remove sets of parentheses for deeply-nested applications.

For example, the following nested function application, which finds the street in the address of an employee's boss:

```haskell
street (address (boss employee))
```

becomes (arguably) easier to read when expressed using `$`:

```haskell
street $ address $ boss employee
```

## Function Composition

Just like we were able to simplify the `insertEntry` function by using eta conversion, we can simplify the definition of `findEntry` by reasoning about its arguments.

Note that the `book` argument is passed to the `filter filterEntry` function, and the result of this application is passed to `head`. In other words, `book` is passed to the _composition_ of the functions `filter filterEntry` and `head`.

In PureScript, the function composition operators are `<<<` and `>>>`. The first is "backwards composition", and the second is "forwards composition".

We can rewrite the right-hand side of `findEntry` using either operator. Using backwards-composition, the right-hand side would be

```
(head <<< filter filterEntry) book
```

In this form, we can apply the eta conversion trick from earlier, to arrive at the final form of `findEntry`:

```haskell
findEntry firstName lastName = head <<< filter filterEntry
  where
    ...
```

An equally valid right-hand side would be:

```haskell
filter filterEntry >>> head
```

Either way, this gives a clear definition of the `findEntry` function: "`findEntry` is the composition of a filtering function and the `head` function".

I will let you make your own decision which definition is easier to understand, but it is often useful to think of functions as building blocks in this way - each function executing a single task, and solutions assembled using function composition.

## Tests, Tests, Tests ...

Now that we have the core of a working application, let's try it out using PSCi.

```text
$ pulp repl

> import Data.AddressBook
```

Let's first try looking up an entry in the empty address book (we obviously expect this to return an empty result):

```text
> findEntry "John" "Smith" emptyBook

No type class instance was found for

    Data.Show.Show { firstName :: String
                   , lastName :: String
                   , address :: { street :: String
                                , city :: String
                                , state :: String
                                }
                   }
```

An error! Not to worry, this error simply means that PSCi doesn't know how to print a value of type `Entry` as a String.

The return type of `findEntry` is `Maybe Entry`, which we can convert to a `String` by hand.

Our `showEntry` function expects an argument of type `Entry`, but we have a value of type `Maybe Entry`. Remember that this means that the function returns an optional value of type `Entry`. What we need to do is apply the `showEntry` function if the optional value is present, and propagate the missing value if not.

Fortunately, the Prelude module provides a way to do this. The `map` operator can be used to lift a function over an appropriate type constructor like `Maybe` (we'll see more on this function, and others like it, later in the book, when we talk about functors):

```text
> import Prelude
> map showEntry (findEntry "John" "Smith" emptyBook)

Nothing
```

That's better - the return value `Nothing` indicates that the optional return value does not contain a value - just as we expected.

For ease of use, we can create a function which prints an `Entry` as a String, so that we don't have to use `showEntry` every time:

```text
> printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)
```

Now let's create a non-empty address book, and try again. We'll reuse our example entry from earlier:

```text
> book1 = insertEntry entry emptyBook

> printEntry "John" "Smith" book1

Just ("Smith, John: 123 Fake St., Faketown, CA")
```

This time, the result contained the correct value. Try defining an address book `book2` with two names by inserting another name into `book1`, and look up each entry by name.

X> ## Exercises
X>
X> 1. (Easy) Test your understanding of the `findEntry` function by writing down the types of each of its major subexpressions. For example, the type of the `head` function as used is specialized to `AddressBook -> Maybe Entry`.
X> 1. (Medium) Write a function which looks up an `Entry` given a street address, by reusing the existing code in `findEntry`. Test your function in PSCi.
X> 1. (Medium) Write a function which tests whether a name appears in a `AddressBook`, returning a Boolean value. _Hint_: Use PSCi to find the type of the `Data.List.null` function, which test whether a list is empty or not.
X> 1. (Difficult) Write a function `removeDuplicates` which removes duplicate address book entries with the same first and last names. _Hint_: Use PSCi to find the type of the `Data.List.nubBy` function, which removes duplicate elements from a list based on an equality predicate.

## Conclusion

In this chapter, we covered several new functional programming concepts:

- How to use the interactive mode PSCi to experiment with functions and test ideas.
- The role of types as both a correctness tool, and an implementation tool.
- The use of curried functions to represent functions of multiple arguments.
- Creating programs from smaller components by composition.
- Structuring code neatly using `where` expressions.
- How to avoid null values by using the `Maybe` type.
- Using techniques like eta conversion and function composition to refactor code into a clear specification.

In the following chapters, we'll build on these ideas.
