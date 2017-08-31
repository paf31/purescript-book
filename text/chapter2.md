# Getting Started

## Chapter Goals

In this chapter, the goal will be to set up a working PureScript development environment, and to write our first PureScript program.

Our first project will be a very simple PureScript library, which will provide a single function which can compute the length of the diagonal in a right-angled triangle.

## Introduction

Here are the tools we will be using to set up our PureScript development environment:

- [`purs`](http://purescript.org) - The PureScript compiler itself.
- [`npm`](http://npmjs.org) - The Node Package Manager, which will allow us to install the rest of our development tools.
- [Pulp](https://github.com/bodil/pulp) - A command-line tool which automates many of the tasks associated with managing PureScript projects.

The rest of the chapter will guide you through installing and configuring these tools.

## Installing PureScript

The recommended approach to installing the PureScript compiler is to download a binary release for your platform from the [PureScript website](http://purescript.org).

You should verify that the PureScript compiler executables are available on your path. Try running the PureScript compiler on the command line to verify this:

```text
$ purs
```

Other options for installing the PureScript compiler include:

- Via NPM: `npm install -g purescript`.
- Building the compiler from source. Instructions can be found on the PureScript website.

## Installing Tools

If you do not have a working installation of [NodeJS](http://nodejs.org/), you should install it. This should also install the `npm` package manager on your system. Make sure you have `npm` installed and available on your path.

You will also need to install the Pulp command line tool, and the Bower package manager using `npm`, as follows:

```text
$ npm install -g pulp bower
```

This will place the `pulp` and `bower` command line tools on your path. At this point, you will have all the tools needed to create your first PureScript project.

## Hello, PureScript!

Let's start out simple. We'll use Pulp to compile and run a simple Hello World! program.

Begin by creating a project in an empty directory, using the `pulp init` command:

```text
$ mkdir my-project
$ cd my-project
$ pulp init

* Generating project skeleton in ~/my-project

$ ls

bower.json	src		test
```

Pulp has created two directories, `src` and `test`, and a `bower.json` configuration file for us. The `src` directory will contain our source files, and the `test` directory will contain our tests. We will use the `test` directory later in the book.

Modify the `src/Main.purs` file to contain the following content:

```haskell
module Main where

import Control.Monad.Eff.Console

main = log "Hello, World!"
```

This small sample illustrates a few key ideas:

- Every file begins with a module header. A module name consists of one or more capitalized words separated by dots. In this case, only a single word is used, but `My.First.Module` would be an equally valid module name.
- Modules are imported using their full names, including dots to separate the parts of the module name. Here, we import the `Control.Monad.Eff.Console` module, which provides the `log` function.
- The `main` program is defined as a function application. In PureScript, function application is indicated with whitespace separating the function name from its arguments.

Let's build and run this code using the following command:

```text
$ pulp run

* Building project in ~/my-project
* Build successful.
Hello, World!
```

Congratulations! You just compiled and executed your first PureScript program.

## Compiling for the Browser

Pulp can be used to turn our PureScript code into Javascript suitable for use in the web browser, by using the `pulp browserify` command:

```text
$ pulp browserify

* Browserifying project in ~/my-project
* Building project in ~/my-project
* Build successful.
* Browserifying...
```

Following this, you should see a large amount of Javascript code printed to the console. This is the output of the [Browserify](http://browserify.org/) tool, applied to a standard PureScript library called the _Prelude_, as well as the code in the `src` directory. This Javascript code can be saved to a file, and included in a HTML document. If you try this, you should see the words "Hello, World!" printed to your browser's console.

## Removing Unused Code

Pulp provides an alternative command, `pulp build`, which can be used with the `-O` option to apply _dead code elimination_, which removes unnecessary Javascript from the output. The result is much smaller:

```text
$ pulp build -O --to output.js

* Building project in ~/my-project
* Build successful.
* Bundling Javascript...
* Bundled.
```

Again, the generated code can be used in a HTML document. If you open `output.js`, you should see a few compiled modules which look like this:

```javascript
(function(exports) {
  "use strict";

  var Control_Monad_Eff_Console = PS["Control.Monad.Eff.Console"];

  var main = Control_Monad_Eff_Console.log("Hello, World!");
  exports["main"] = main;
})(PS["Main"] = PS["Main"] || {});
```

This illustrates a few points about the way the PureScript compiler generates Javascript code:

- Every module gets turned into an object, created by a wrapper function, which contains the module's exported members.
- PureScript tries to preserve the names of variables wherever possible
- Function applications in PureScript get turned into function applications in JavaScript.
- The main method is run after all modules have been defined, and is generated as a simple method call with no arguments.
- PureScript code does not rely on any runtime libraries. All of the code that is generated by the compiler originated in a PureScript module somewhere which your code depended on.

These points are important, since they mean that PureScript generates simple, understandable code. In fact, the code generation process in general is quite a shallow transformation. It takes relatively little understanding of the language to predict what JavaScript code will be generated for a particular input.

## Compiling CommonJS Modules

Pulp can also be used to generate CommonJS modules from PureScript code. This can be useful when using NodeJS, or just when developing a larger project which uses CommonJS modules to break code into smaller components.

To build CommonJS modules, use the `pulp build` command (without the `-O` option):

```text
$ pulp build

* Building project in ~/my-project
* Build successful.
```

The generated modules will be placed in the `output` directory by default. Each PureScript module will be compiled to its own CommonJS module, in its own subdirectory.

## Tracking Dependencies with Bower

To write the `diagonal` function (the goal of this chapter), we will need to be able to compute square roots. The `purescript-math` package contains type definitions for functions defined on the JavaScript `Math` object, so let's install it:

```text
$ bower install purescript-math --save
```

The `--save` option causes the dependency to be added to the `bower.json` configuration file.

The `purescript-math` library sources should now be available in the `bower_components` subdirectory, and will be included when you compile your project.

## Computing Diagonals

Let's write the `diagonal` function, which will be an example of using a function from an external library.

First, import the `Math` module by adding the following line at the top of the `src/Main.purs` file:

```haskell
import Math (sqrt)
```

It's also necessary to import the `Prelude` module, which defines very basic operations such as numeric addition and multiplication:

```haskell
import Prelude
```

Now define the `diagonal` function as follows:

```haskell
diagonal w h = sqrt (w * w + h * h)
```

Note that there is no need to define a type for our function. The compiler is able to infer that `diagonal` is a function which takes two numbers and returns a number. In general, however, it is a good practice to provide type annotations as a form of documentation.

Let's also modify the `main` function to use the new `diagonal` function:

```haskell
main = logShow (diagonal 3.0 4.0)
```

Now compile and run the project again, using `pulp run`:

```text
$ pulp run

* Building project in ~/my-project
* Build successful.
5.0
```

## Testing Code Using the Interactive Mode

The PureScript compiler also ships with an interactive REPL called PSCi. This can be very useful for testing your code, and experimenting with new ideas. Let's use PSCi to test the `diagonal` function.

Pulp can load source modules into PSCi automatically, via the `pulp repl` command:

```text
$ pulp repl
>
```

You can type `:?` to see a list of commands:

```text
> :?
The following commands are available:

    :?                        Show this help menu
    :quit                     Quit PSCi
    :reset                    Reset
    :browse      <module>     Browse <module>
    :type        <expr>       Show the type of <expr>
    :kind        <type>       Show the kind of <type>
    :show        import       Show imported modules
    :show        loaded       Show loaded modules
    :paste       paste        Enter multiple lines, terminated by ^D
```

By pressing the Tab key, you should be able to see a list of all functions available in your own code, as well as any Bower dependencies and the Prelude modules.

Start by importing the `Prelude` module:

```text
> import Prelude
```

Try evaluating a few expressions now:

```text
> 1 + 2
3

> "Hello, " <> "World!"
"Hello, World!"
```

Let's try out our new `diagonal` function in PSCi:

```text
> import Main
> diagonal 5.0 12.0

13.0
```

You can also use PSCi to define functions:

```text
> double x = x * 2

> double 10
20
```

Don't worry if the syntax of these examples is unclear right now - it will make more sense as you read through the book.

Finally, you can check the type of an expression by using the `:type` command:

```text
> :type true
Boolean

> :type [1, 2, 3]
Array Int
```

Try out the interactive mode now. If you get stuck at any point, simply use the Reset command `:reset` to unload any modules which may be compiled in memory.

X> ## Exercises
X>
X> 1. (Easy) Use the `pi` constant, which is defined in the `Math` module, to write a function `circleArea` which computes the area of a circle with a given radius. Test your function using PSCi (_Hint_: don't forget to import `pi` by modifying the `import Math` statement).
X> 1. (Medium) Use `bower install` to install the `purescript-globals` package as a dependency. Test out its functions in PSCi (_Hint_: you can use the `:browse` command in PSCi to browse the contents of a module).

## Conclusion

In this chapter, we set up a simple PureScript project using the Pulp tool.

We also wrote our first PureScript function, and a JavaScript program which could be compiled and executed either in the browser or in NodeJS.

We will use this development setup in the following chapters to compile, debug and test our code, so you should make sure that you are comfortable with the tools and techniques involved.
