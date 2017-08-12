# Introduction

## Functional JavaScript

Functional programming techniques have been making appearances in JavaScript for some time now:

- Libraries such as [UnderscoreJS](http://underscorejs.org) allow the developer to leverage tried-and-trusted functions such as `map`, `filter` and `reduce` to create larger programs from smaller programs by composition:

    ```javascript
    var sumOfPrimes =
        _.chain(_.range(1000))
         .filter(isPrime)
         .reduce(function(x, y) {
             return x + y;
         })
         .value();
    ```

- Asynchronous programming in NodeJS leans heavily on functions as first-class values to define callbacks.

    ```javascript
    require('fs').readFile(sourceFile, function (error, data) {
      if (!error) {
        require('fs').writeFile(destFile, data, function (error) {
          if (!error) {
            console.log("File copied");
          }
        });
      }
    });
    ```

- Libraries such as [React](http://facebook.github.io/react/) and [virtual-dom](https://github.com/Matt-Esch/virtual-dom) model views as pure functions of application state.

Functions enable a simple form of abstraction which can yield great productivity gains. However, functional programming in JavaScript has its own disadvantages: JavaScript is verbose, untyped, and lacks powerful forms of abstraction. Unrestricted JavaScript code also makes equational reasoning very difficult.

PureScript is a programming language which aims to address these issues. It features lightweight syntax, which allows us to write very expressive code which is still clear and readable. It uses a rich type system to support powerful abstractions. It also generates fast, understandable code, which is important when interoperating with JavaScript, or other languages which compile to JavaScript. All in all, I hope to convince you that PureScript strikes a very practical balance between the theoretical power of purely functional programming, and the fast-and-loose programming style of JavaScript.

## Types and Type Inference

The debate over statically typed languages versus dynamically typed languages is well-documented. PureScript is a _statically typed_ language, meaning that a correct program can be given a _type_ by the compiler which indicates its behavior. Conversely, programs which cannot be given a type are _incorrect programs_, and will be rejected by the compiler. In PureScript, unlike in dynamically typed languages, types exist only at _compile-time_, and have no representation at runtime.

It is important to note that in many ways, the types in PureScript are unlike the types that you might have seen in other languages like Java or C#. While they serve the same purpose at a high level, the types in PureScript are inspired by languages like ML and Haskell. PureScript's types are expressive, allowing the developer to assert strong claims about their programs. Most importantly, PureScript's type system supports _type inference_ - it requires far fewer explicit type annotations than other languages, making the type system a _tool_ rather than a hindrance. As a simple example, the following code defines a _number_, but there is no mention of the `Number` type anywhere in the code:

```haskell
iAmANumber =
  let square x = x * x
  in square 42.0
```

A more involved example shows that type-correctness can be confirmed without type annotations, even when there exist types which are _unknown to the compiler_:

```haskell
iterate f 0 x = x
iterate f n x = iterate f (n - 1) (f x)
```

Here, the type of `x` is unknown, but the compiler can still verify that `iterate` obeys the rules of the type system, no matter what type `x` might have.

In this book, I will try to convince you (or reaffirm your belief) that static types are not only a means of gaining confidence in the correctness of your programs, but also an aid to development in their own right. Refactoring a large body of code in JavaScript can be difficult when using any but the simplest of abstractions, but an expressive type system together with a type checker can even make refactoring into an enjoyable, interactive experience.

In addition, the safety net provided by a type system enables more advanced forms of abstraction. In fact, PureScript provides a powerful form of abstraction which is fundamentally type-driven: type classes, made popular in the functional programming language Haskell.

## Polyglot Web Programming

Functional programming has its success stories - applications where it has been particularly successful: data analysis, parsing, compiler implementation, generic programming, parallelism, to name a few.

It would be possible to practice end-to-end application development in a functional language like PureScript. PureScript provides the ability to import existing JavaScript code, by providing types for its values and functions, and then to use those functions in regular PureScript code. We'll see this approach later in the book.

However, one of PureScript's strengths is its interoperability with other languages which target JavaScript. Another approach would be to use PureScript for a subset of your application's development, and to use one or more other languages to write the rest of the JavaScript.

Here are some examples:

- Core logic written in PureScript, with the user interface written in JavaScript.
- Application written in JavaScript or another compile-to-JS language, with tests written in PureScript.
- PureScript used to automate user interface tests for an existing application.

In this book, we'll focus on solving small problems with PureScript. The solutions could be integrated into a larger application, but we will also look at how to call PureScript code from JavaScript, and vice versa.

## Prerequisites

The software requirements for this book are minimal: the first chapter will guide you through setting up a development environment from scratch, and the tools we will use are available in the standard repositories of most modern operating systems.

The PureScript compiler itself can be downloaded as a binary distribution, or built from source on any system running an up-to-date installation of the GHC Haskell compiler, and we will walk through this process in the next chapter.

The code in this version of the book is compatible with versions `0.11.*` of
the PureScript compiler.

## About You

I will assume that you are familiar with the basics of JavaScript. Any prior familiarity with common tools from the JavaScript ecosystem, such as NPM and Gulp, will be beneficial if you wish to customize the standard setup to your own needs, but such knowledge is not necessary.

No prior knowledge of functional programming is required, but it certainly won't hurt. New ideas will be accompanied by practical examples, so you should be able to form an intuition for the concepts from functional programming that we will use.

Readers who are familiar with the Haskell programming language will recognize a lot of the ideas and syntax presented in this book, because PureScript is heavily influenced by Haskell. However, those readers should understand that there are a number of important differences between PureScript and Haskell. It is not necessarily always appropriate to try to apply ideas from one language in the other, although many of the concepts presented here will have some interpretation in Haskell.

## How to Read This Book

The chapters in this book are largely self contained. A beginner with little functional programming experience would be well-advised, however, to work through the chapters in order. The first few chapters lay the groundwork required to understand the material later on in the book. A reader who is comfortable with the ideas of functional programming (especially one with experience in a strongly-typed language like ML or Haskell) will probably be able to gain a general understanding of the code in the later chapters of the book without reading the preceding chapters.

Each chapter will focus on a single practical example, providing the motivation for any new ideas introduced. Code for each chapter are available from the book's [GitHub repository](https://github.com/paf31/purescript-book). Some chapters will include code snippets taken from the chapter's source code, but for a full understanding, you should read the source code from the repository alongside the material from the book. Longer sections will contain shorter snippets which you can execute in the interactive mode PSCi to test your understanding.

Code samples will appear in a monospaced font, as follows:

```haskell
module Example where

import Control.Monad.Eff.Console (log)

main = log "Hello, World!"
```

Commands which should be typed at the command line will be preceded by a dollar symbol:

```text
$ pulp build
```

Usually, these commands will be tailored to Linux/Mac OS users, so Windows users may need to make small changes such as modifying the file separator, or replacing shell built-ins with their Windows equivalents.

Commands which should be typed at the PSCi interactive mode prompt will be preceded by an angle bracket:

```text
> 1 + 2
3
```

Each chapter will contain exercises, labelled with their difficulty level. It is strongly recommended that you attempt the exercises in each chapter to fully understand the material.

This book aims to provide an introduction to the PureScript language for beginners, but it is not the sort of book that provides a list of template solutions to problems. For beginners, this book should be a fun challenge, and you will get the most benefit if you read the material, attempt the exercises, and most importantly of all, try to write some code of your own.

## Getting Help

If you get stuck at any point, there are a number of resources available online for learning PureScript:

- The PureScript IRC channel is a great place to chat about issues you may be having. Point your IRC client at irc.freenode.net, and connect to the #purescript channel.
- The [PureScript website](http://purescript.org) contains links to several learning resources, including code samples, videos and other resources for beginners.
- The [PureScript documentation repository](https://github.com/purescript/documentation) collects articles and examples on a wide variety of topics, written by PureScript developers and users.
- [Try PureScript!](http://try.purescript.org) is a website which allows users to compile PureScript code in the web browser, and contains several simple examples of code.
- [Pursuit](http://pursuit.purescript.org) is a searchable database of PureScript types and functions.

If you prefer to learn by reading examples, the `purescript`, `purescript-node` and `purescript-contrib` GitHub organizations contain plenty of examples of PureScript code.

## About the Author

I am the original developer of the PureScript compiler. I'm based in Los Angeles, California, and started programming at an early age in BASIC on an 8-bit personal computer, the Amstrad CPC. Since then I have worked professionally in a variety of programming languages (including Java, Scala, C#, F#, Haskell and PureScript).

Not long into my professional career, I began to appreciate functional programming and its connections with mathematics, and enjoyed learning functional concepts using the Haskell programming language.

I started working on the PureScript compiler in response to my experience with JavaScript. I found myself using functional programming techniques that I had picked up in languages like Haskell, but wanted a more principled environment in which to apply them. Solutions at the time included various attempts to compile Haskell to JavaScript while preserving its semantics (Fay, Haste, GHCJS), but I was interested to see how successful I could be by approaching the problem from the other side - attempting to keep the semantics of JavaScript, while enjoying the syntax and type system of a language like Haskell.

I maintain [a blog](http://blog.functorial.com), and can be [reached on Twitter](http://twitter.com/paf31).

## Acknowledgements

I would like to thank the many contributors who helped PureScript to reach its current state. Without the huge collective effort which has been made on the compiler, tools, libraries, documentation and tests, the project would certainly have failed.  

The PureScript logo which appears on the cover of this book was created by Gareth Hughes, and is gratefully reused here under the terms of the [Creative Commons Attribution 4.0 license](https://creativecommons.org/licenses/by/4.0/).

Finally, I would like to thank everyone who has given me feedback and corrections on the contents of this book.
