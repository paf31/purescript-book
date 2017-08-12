# Canvas Graphics

## Chapter Goals

This chapter will be an extended example focussing on the `purescript-canvas` package, which provides a way to generate 2D graphics from PureScript using the HTML5 Canvas API.

## Project Setup

This module's project introduces the following new Bower dependencies:

- `purescript-canvas`, which gives types to methods from the HTML5 Canvas API
- `purescript-refs`, which provides a side-effect for using _global mutable references_

The source code for the chapter is broken up into a set of modules, each of which defines a `main` method. Different sections of this chapter are implemented in different files, and the `Main` module can be changed by modifying the Pulp build command to run the appropriate file's `main` method at each point.

The HTML file `html/index.html` contains a single `canvas` element which will be used in each example, and a `script` element to load the compiled PureScript code. To test the code for each section, open the HTML file in your browser.

## Simple Shapes

The `Example/Rectangle.purs` file contains a simple introductory example, which draws a single blue rectangle at the center of the canvas. The module imports the `Control.Monad.Eff` module, and also the `Graphics.Canvas` module, which contains actions in the `Eff` monad for working with the Canvas API.

The `main` action starts, like in the other modules, by using the `getCanvasElementById` action to get a reference to the canvas object, and the `getContext2D` action to access the 2D rendering context for the canvas:

```haskell
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
```

_Note_: the call to `unsafePartial` here is necessary since the pattern match on the result of `getCanvasElementById` is partial, matching only the `Just` constructor. For our purposes, this is fine, but in production code, we would probably want to match the `Nothing` constructor and provide an appropriate error message.

The types of these actions can be found using PSCi or by looking at the documentation:

```haskell
getCanvasElementById :: forall eff. String -> Eff (canvas :: CANVAS | eff) (Maybe CanvasElement)

getContext2D :: forall eff. CanvasElement -> Eff (canvas :: CANVAS | eff) Context2D
```

`CanvasElement` and `Context2D` are types defined in the `Graphics.Canvas` module. The same module also defines the `Canvas` effect, which is used by all of the actions in the module.

The graphics context `ctx` manages the state of the canvas, and provides methods to render primitive shapes, set styles and colors, and apply transformations.

We continue by setting the fill style to solid blue, by using the `setFillStyle` action:

```haskell
  setFillStyle "#0000FF" ctx
```

Note that the `setFillStyle` action takes the graphics context as an argument. This is a common pattern in the `Graphics.Canvas` module.

Finally, we use the `fillPath` action to fill the rectangle. `fillPath` has the following type:

```haskell
fillPath :: forall eff a. Context2D ->
                          Eff (canvas :: CANVAS | eff) a ->
                          Eff (canvas :: CANVAS | eff) a
```

`fillPath` takes a graphics context, and another action which builds the path to render. To build a path, we can use the `rect` action. `rect` takes a graphics context, and a record which provides the position and size of the rectangle:

```haskell
  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }
```

Build the rectangle example, providing `Example.Rectangle` as the name of the main module:

```text
$ mkdir dist/
$ pulp build -O --main Example.Rectangle --to dist/Main.js
```

Now, open the `html/index.html` file and verify that this code renders a blue rectangle in the center of the canvas.

## Putting Row Polymorphism to Work

There are other ways to render paths. The `arc` function renders an arc segment, and the `moveTo`, `lineTo` and `closePath` functions can be used to render piecewise-linear paths.

The `Shapes.purs` file renders three shapes: a rectangle, an arc segment and a triangle.

We have seen that the `rect` function takes a record as its argument. In fact, the properties of the rectangle are defined in a type synonym:

```haskell
type Rectangle =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }
```

The `x` and `y` properties represent the location of the top-left corner, while the `w` and `h` properties represent the width and height respectively.

To render an arc segment, we can use the `arc` function, passing a record with the following type:

```haskell
type Arc =
  { x     :: Number
  , y     :: Number
  , r     :: Number
  , start :: Number
  , end   :: Number
  }
```

Here, the `x` and `y` properties represent the center point, `r` is the radius, and `start` and `end` represent the endpoints of the arc in radians.

For example, this code fills an arc segment centered at `(300, 300)` with radius `50`:

```haskell
  fillPath ctx $ arc ctx
    { x      : 300.0
    , y      : 300.0
    , r      : 50.0
    , start  : Math.pi * 5.0 / 8.0
    , end    : Math.pi * 2.0
    }
```

Notice that both the `Rectangle` and `Arc` record types contain `x` and `y` properties of type `Number`. In both cases, this pair represents a point. This means that we can write row-polymorphic functions which can act on either type of record.

For example, the `Shapes` module defines a `translate` function which translates a shape by modifying its `x` and `y` properties:

```haskell
translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }
```

Notice the row-polymorphic type. It says that `translate` accepts any record with `x` and `y` properties _and any other properties_, and returns the same type of record. The `x` and `y` fields are updated, but the rest of the fields remain unchanged.

This is an example of _record update syntax_. The expression `shape { ... }` creates a new record based on the `shape` record, with the fields inside the braces updated to the specified values. Note that the expressions inside the braces are separated from their labels by equals symbols, not colons like in record literals.

The `translate` function can be used with both the `Rectangle` and `Arc` records, as can be seen in the `Shapes` example.

The third type of path rendered in the `Shapes` example is a piecewise-linear path. Here is the corresponding code:

```haskell
  setFillStyle "#FF0000" ctx

  fillPath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx
```

There are three functions in use here:

- `moveTo` moves the current location of the path to the specified coordinates,
- `lineTo` renders a line segment between the current location and the specified coordinates, and updates the current location,
- `closePath` completes the path by rendering a line segment joining the current location to the start position.

The result of this code snippet is to fill an isosceles triangle.

Build the example by specifying `Example.Shapes` as the main module:

```text
$ pulp build -O --main Example.Shapes --to dist/Main.js
```

and open `html/index.html` again to see the result. You should see the three different types of shapes rendered to the canvas.

X> ## Exercises
X>
X> 1. (Easy) Experiment with the `strokePath` and `setStrokeStyle` functions in each of the examples so far.
X> 1. (Easy) The `fillPath` and `strokePath` functions can be used to render complex paths with a common style by using a do notation block inside the function argument. Try changing the `Rectangle` example to render two rectangles side-by-side using the same call to `fillPath`. Try rendering a sector of a circle by using a combination of a piecewise-linear path and an arc segment.
X> 1. (Medium) Given the following record type:
X>
X>     ```haskell
X>     type Point = { x :: Number, y :: Number }
X>     ```
X>
X>     which represents a 2D point, write a function `renderPath` which strokes a closed path constructed from a number of points:
X>
X>     ```haskell
X>     renderPath
X>       :: forall eff
X>        . Context2D
X>       -> Array Point
X>       -> Eff (canvas :: Canvas | eff) Unit
X>     ```
X>
X>     Given a function
X>
X>     ```haskell
X>     f :: Number -> Point
X>     ```
X>
X>     which takes a `Number` between `0` and `1` as its argument and returns a `Point`, write an action which plots `f` by using your `renderPath` function. Your action should approximate the path by sampling `f` at a finite set of points.
X>
X>     Experiment by rendering different paths by varying the function `f`.

## Drawing Random Circles

The `Example/Random.purs` file contains an example which uses the `Eff` monad to interleave two different types of side-effect: random number generation, and canvas manipulation. The example renders one hundred randomly generated circles onto the canvas.

The `main` action obtains a reference to the graphics context as before, and then sets the stroke and fill styles:

```haskell
  setFillStyle "#FF0000" ctx
  setStrokeStyle "#000000" ctx
```

Next, the code uses the `for_` function to loop over the integers between `0` and `100`:

```haskell
  for_ (1 .. 100) \_ -> do
```

On each iteration, the do notation block starts by generating three random numbers distributed between `0` and `1`. These numbers represent the `x` and `y` coordinates, and the radius of a circle:

```haskell
    x <- random
    y <- random
    r <- random
```

Next, for each circle, the code creates an `Arc` based on these parameters and finally fills and strokes the arc with the current styles:

```haskell
    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , r     : r * 50.0
         , start : 0.0
         , end   : Math.pi * 2.0
         }
    fillPath ctx path
    strokePath ctx path
```

Build this example by specifying the `Example.Random` module as the main module:

```text
$ pulp build -O --main Example.Random --to dist/Main.js
```

and view the result by opening `html/index.html`.

## Transformations

There is more to the canvas than just rendering simple shapes. Every canvas maintains a transformation which is used to transform shapes before rendering. Shapes can be translated, rotated, scaled, and skewed.

The `purescript-canvas` library supports these transformations using the following functions:

```haskell
translate :: forall eff
           . TranslateTransform
          -> Context2D
          -> Eff (canvas :: Canvas | eff) Context2D

rotate    :: forall eff
           . Number
          -> Context2D
          -> Eff (canvas :: Canvas | eff) Context2D

scale     :: forall eff
           . ScaleTransform
          -> Context2D
          -> Eff (canvas :: CANVAS | eff) Context2D

transform :: forall eff
           . Transform
          -> Context2D
          -> Eff (canvas :: CANVAS | eff) Context2D
```

The `translate` action performs a translation whose components are specified by the properties of the `TranslateTransform` record.

The `rotate` action performs a rotation around the origin, through some number of radians specified by the first argument.

The `scale` action performs a scaling, with the origin as the center. The `ScaleTransform` record specifies the scale factors along the `x` and `y` axes.

Finally, `transform` is the most general action of the four here. It performs an affine transformation specified by a matrix.

Any shapes rendered after these actions have been invoked will automatically have the appropriate transformation applied.

In fact, the effect of each of these functions is to _post-multiply_ the transformation with the context's current transformation. The result is that if multiple transformations applied after one another, then their effects are actually applied in reverse:

```haskell
transformations ctx = do
  translate { translateX: 10.0, translateY: 10.0 } ctx
  scale { scaleX: 2.0, scaleY: 2.0 } ctx
  rotate (Math.pi / 2.0) ctx

  renderScene
```

The effect of this sequence of actions is that the scene is rotated, then scaled, and finally translated.

## Preserving the Context

A common use case is to render some subset of the scene using a transformation, and then to reset the transformation afterwards.

The Canvas API provides the `save` and `restore` methods, which manipulate a _stack_ of states associated with the canvas. `purescript-canvas` wraps this functionality into the following functions:

```haskell
save
  :: forall eff
   . Context2D
  -> Eff (canvas :: CANVAS | eff) Context2D

restore
  :: forall eff
   . Context2D
  -> Eff (canvas :: CANVAS | eff) Context2D
```

The `save` action pushes the current state of the context (including the current transformation and any styles) onto the stack, and the `restore` action pops the top state from the stack and restores it.

This allows us to save the current state, apply some styles and transformations, render some primitives, and finally restore the original transformation and state. For example, the following function performs some canvas action, but applies a rotation before doing so, and restores the transformation afterwards:

```haskell
rotated ctx render = do
  save ctx
  rotate Math.pi ctx
  render
  restore ctx
```

In the interest of abstracting over common use cases using higher-order functions, the `purescript-canvas` library provides the `withContext` function, which performs some canvas action while preserving the original context state:

```haskell
withContext
  :: forall eff a
   . Context2D
  -> Eff (canvas :: CANVAS | eff) a
  -> Eff (canvas :: CANVAS | eff) a          
```

We could rewrite the `rotated` function above using `withContext` as follows:

```haskell
rotated ctx render =
  withContext ctx do
    rotate Math.pi ctx
    render
```

## Global Mutable State

In this section, we'll use the `purescript-refs` package to demonstrate another effect in the `Eff` monad.

The `Control.Monad.Eff.Ref` module provides a type constructor for global mutable references, and an associated effect:

```text
> import Control.Monad.Eff.Ref

> :kind Ref
Type -> Type

> :kind REF
Control.Monad.Eff.Effect
```

A value of type `Ref a` is a mutable reference cell containing a value of type `a`, much like an `STRef h a`, which we saw in the previous chapter. The difference is that, while the `ST` effect can be removed by using `runST`, the `Ref` effect does not provide a handler. Where `ST` is used to track safe, local mutation, `Ref` is used to track global mutation. As such, it should be used sparingly.

The `Example/Refs.purs` file contains an example which uses the `REF` effect to track mouse clicks on the `canvas` element.

The code starts by creating a new reference containing the value `0`, by using the `newRef` action:

```haskell
  clickCount <- newRef 0
```

Inside the click event handler, the `modifyRef` action is used to update the click count:

```haskell
    modifyRef clickCount (\count -> count + 1)
```

The `readRef` action is used to read the new click count:

```haskell
    count <- readRef clickCount
```

In the `render` function, the click count is used to determine the transformation applied to a rectangle:

```haskell
    withContext ctx do
      let scaleX = Math.sin (toNumber count * Math.pi / 4.0) + 1.5
      let scaleY = Math.sin (toNumber count * Math.pi / 6.0) + 1.5

      translate { translateX: 300.0, translateY:  300.0 } ctx
      rotate (toNumber count * Math.pi / 18.0) ctx
      scale { scaleX: scaleX, scaleY: scaleY } ctx
      translate { translateX: -100.0, translateY: -100.0 } ctx

      fillPath ctx $ rect ctx
        { x: 0.0
        , y: 0.0
        , w: 200.0
        , h: 200.0
        }
```

This action uses `withContext` to preserve the original transformation, and then applies the following sequence of transformations (remember that transformations are applied bottom-to-top):

- The rectangle is translated through `(-100, -100)` so that its center lies at the origin.
- The rectangle is scaled around the origin.
- The rectangle is rotated through some multiple of `10` degrees around the origin.
- The rectangle is translated through `(300, 300)` so that it center lies at the center of the canvas.

Build the example:

```text
$ pulp build -O --main Example.Refs --to dist/Main.js
```

and open the `html/index.html` file. If you click the canvas repeatedly, you should see a green rectangle rotating around the center of the canvas.

X> ## Examples
X>
X> 1. (Easy) Write a higher-order function which strokes and fills a path simultaneously. Rewrite the `Random.purs` example using your function.
X> 1. (Medium) Use the `RANDOM` and `DOM` effects to create an application which renders a circle with random position, color and radius to the canvas when the mouse is clicked.
X> 1. (Medium) Write a function which transforms the scene by rotating it around a point with specified coordinates. _Hint_: use a translation to first translate the scene to the origin.

## L-Systems

In this final example, we will use the `purescript-canvas` package to write a function for rendering _L-systems_ (or _Lindenmayer systems_).

An L-system is defined by an _alphabet_, an initial sequence of letters from the alphabet, and a set of _production rules_. Each production rule takes a letter of the alphabet and returns a sequence of replacement letters. This process is iterated some number of times starting with the initial sequence of letters.

If each letter of the alphabet is associated with some instruction to perform on the canvas, the L-system can be rendered by following the instructions in order.

For example, suppose the alphabet consists of the letters `L` (turn left), `R` (turn right) and `F` (move forward). We might define the following production rules:

```text
L -> L
R -> R
F -> FLFRRFLF
```

If we start with the initial sequence "FRRFRRFRR" and iterate, we obtain the following sequence:

```text
FRRFRRFRR
FLFRRFLFRRFLFRRFLFRRFLFRRFLFRR
FLFRRFLFLFLFRRFLFRRFLFRRFLFLFLFRRFLFRRFLFRRFLF...
```

and so on. Plotting a piecewise-linear path corresponding to this set of instruction approximates a curve called the _Koch curve_. Increasing the number of iterations increases the resolution of the curve.

Let's translate this into the language of types and functions.

We can represent our choice of alphabet by a choice of type. For our example, we can choose the following type:

```haskell
data Alphabet = L | R | F
```

This data type defines one data constructor for each letter in our alphabet.

How can we represent the initial sequence of letters? Well, that's just an array of letters from our alphabet, which we will call a `Sentence`:

```haskell
type Sentence = Array Alphabet

initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]
```

Our production rules can be represented as a function from `Alphabet` to `Sentence` as follows:

```haskell
productions :: Alphabet -> Sentence
productions L = [L]
productions R = [R]
productions F = [F, L, F, R, R, F, L, F]
```

This is just copied straight from the specification above.

Now we can implement a function `lsystem` which will take a specification in this form, and render it to the canvas. What type should `lsystem` have? Well, it needs to take values like `initial` and `productions` as arguments, as well as a function which can render a letter of the alphabet to the canvas.

Here is a first approximation to the type of `lsystem`:

```haskell
forall eff. Sentence
         -> (Alphabet -> Sentence)
         -> (Alphabet -> Eff (canvas :: Canvas | eff) Unit)
         -> Int
         -> Eff (canvas :: CANVAS | eff) Unit
```

The first two argument types correspond to the values `initial` and `productions`.

The third argument represents a function which takes a letter of the alphabet and _interprets_ it by performing some actions on the canvas. In our example, this would mean turning left in the case of the letter `L`, turning right in the case of the letter `R`, and moving forward in the case of a letter `F`.

The final argument is a number representing the number of iterations of the production rules we would like to perform.

The first observation is that the `lsystem` function should work for only one type of `Alphabet`, but for any type, so we should generalize our type accordingly. Let's replace `Alphabet` and `Sentence` with `a` and `Array a` for some quantified type variable `a`:

```haskell
forall a eff. Array a
           -> (a -> Array a)
           -> (a -> Eff (canvas :: CANVAS | eff) Unit)
           -> Int
           -> Eff (canvas :: CANVAS | eff) Unit
```

The second observation is that, in order to implement instructions like "turn left" and "turn right", we will need to maintain some state, namely the direction in which the path is moving at any time. We need to modify our function to pass the state through the computation. Again, the `lsystem` function should work for any type of state, so we will represent it using the type variable `s`.

We need to add the type `s` in three places:

```haskell
forall a s eff. Array a
             -> (a -> Array a)
             -> (s -> a -> Eff (canvas :: CANVAS | eff) s)
             -> Int
             -> s
             -> Eff (canvas :: CANVAS | eff) s
```

Firstly, the type `s` was added as the type of an additional argument to `lsystem`. This argument will represent the initial state of the L-system.

The type `s` also appears as an argument to, and as the return type of the interpretation function (the third argument to `lsystem`). The interpretation function will now receive the current state of the L-system as an argument, and will return a new, updated state as its return value.

In the case of our example, we can define use following type to represent the state:

```haskell
type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }
```

The properties `x` and `y` represent the current position of the path, and the `theta` property represents the current direction of the path, specified as the angle between the path direction and the horizontal axis, in radians.

The initial state of the system might be specified as follows:

```haskell
initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }
```

Now let's try to implement the `lsystem` function. We will find that its definition is remarkably simple.

It seems reasonable that `lsystem` should recurse on its fourth argument (of type `Int`). On each step of the recursion, the current sentence will change, having been updated by using the production rules. With that in mind, let's begin by introducing names for the function arguments, and delegating to a helper function:

```haskell
lsystem :: forall a s eff
         . Array a
        -> (a -> Array a)
        -> (s -> a -> Eff (canvas :: CANVAS | eff) s)
        -> Int
        -> s
        -> Eff (canvas :: CANVAS | eff) s
lsystem init prod interpret n state = go init n
  where
```

The `go` function works by recursion on its second argument. There are two cases: when `n` is zero, and when `n` is non-zero.

In the first case, the recursion is complete, and we simply need to interpret the current sentence according to the interpretation function. We have a sentence of type `Array a`, a state of type `s`, and a function of type `s -> a -> Eff (canvas :: CANVAS | eff) s`. This sounds like a job for the `foldM` function which we defined earlier, and which is available from the `purescript-control` package:

```haskell
  go s 0 = foldM interpret state s
```

What about in the non-zero case? In that case, we can simply apply the production rules to each letter of the current sentence, concatenate the results, and repeat by calling `go` recursively:

```haskell
  go s n = go (concatMap prod s) (n - 1)
```

That's it! Note how the use of higher order functions like `foldM` and `concatMap` allowed us to communicate our ideas concisely.

However, we're not quite done. The type we have given is actually still too specific. Note that we don't use any canvas operations anywhere in our implementation. Nor do we make use of the structure of the `Eff` monad at all. In fact, our function works for _any_ monad `m`!

Here is the more general type of `lsystem`, as specified in the accompanying source code for this chapter:

```haskell
lsystem :: forall a m s
         . Monad m
        => Array a
        -> (a -> Array a)
        -> (s -> a -> m s)
        -> Int
        -> s
        -> m s
```

We can understand this type as saying that our interpretation function is free to have any side-effects at all, captured by the monad `m`. It might render to the canvas, or print information to the console, or support failure or multiple return values. The reader is encouraged to try writing L-systems which use these various types of side-effect.

This function is a good example of the power of separating data from implementation. The advantage of this approach is that we gain the freedom to interpret our data in multiple different ways. We might even factor `lsystem` into two smaller functions: the first would build the sentence using repeated application of `concatMap`, and the second would interpret the sentence using `foldM`. This is also left as an exercise for the reader.

Let's complete our example by implementing its interpretation function. The type of `lsystem` tells us that its type signature must be `s -> a -> m s` for some types `a` and `s` and a type constructor `m`. We know that we want `a` to be `Alphabet` and `s` to be `State`, and for the monad `m` we can choose `Eff (canvas :: CANVAS)`. This gives us the following type:

```haskell
interpret :: State -> Alphabet -> Eff (canvas :: CANVAS) State
```

To implement this function, we need to handle the three data constructors of the `Alphabet` type. To interpret the letters `L` (move left) and `R` (move right), we simply have to update the state to change the angle `theta` appropriately:

```haskell
interpret state L = pure $ state { theta = state.theta - Math.pi / 3 }
interpret state R = pure $ state { theta = state.theta + Math.pi / 3 }
```

To interpret the letter `F` (move forward), we can calculate the new position of the path, render a line segment, and update the state, as follows:

```haskell
interpret state F = do
  let x = state.x + Math.cos state.theta * 1.5
      y = state.y + Math.sin state.theta * 1.5
  moveTo ctx state.x state.y
  lineTo ctx x y
  pure { x, y, theta: state.theta }
```

Note that in the source code for this chapter, the `interpret` function is defined using a `let` binding inside the `main` function, so that the name `ctx` is in scope. It would also be possible to move the context into the `State` type, but this would be inappropriate because it is not a changing part of the state of the system.

To render this L-system, we can simply use the `strokePath` action:

```haskell
strokePath ctx $ lsystem initial productions interpret 5 initialState
```

Compile the L-system example using

```text
$ pulp build -O --main Example.LSystem --to dist/Main.js
```

and open `html/index.html`. You should see the Koch curve rendered to the canvas.

X> ## Exercises
X>
X> 1. (Easy) Modify the L-system example above to use `fillPath` instead of `strokePath`. _Hint_: you will need to include a call to `closePath`, and move the call to `moveTo` outside of the `interpret` function.
X> 1. (Easy) Try changing the various numerical constants in the code, to understand their effect on the rendered system.
X> 1. (Medium) Break the `lsystem` function into two smaller functions. The first should build the final sentence using repeated application of `concatMap`, and the second should use `foldM` to interpret the result.
X> 1. (Medium) Add a drop shadow to the filled shape, by using the `setShadowOffsetX`, `setShadowOffsetY`, `setShadowBlur` and `setShadowColor` actions. _Hint_: use PSCi to find the types of these functions.
X> 1. (Medium) The angle of the corners is currently a constant (`pi/3`). Instead, it can be moved into the `Alphabet` data type, which allows it to be changed by the production rules:
X>
X>     ```haskell
X>     type Angle = Number
X>     
X>     data Alphabet = L Angle | R Angle | F
X>     ```
X>     
X>     How can this new information be used in the production rules to create interesting shapes?
X> 1. (Difficult) An L-system is given by an alphabet with four letters: `L` (turn left through 60 degrees), `R` (turn right through 60 degrees), `F` (move forward) and `M` (also move forward).
X>
X>     The initial sentence of the system is the single letter `M`.
X>
X>     The production rules are specified as follows:
X>
X>     ```text
X>     L -> L
X>     R -> R
X>     F -> FLMLFRMRFRMRFLMLF
X>     M -> MRFRMLFLMLFLMRFRM
X>     ```
X>
X>     Render this L-system. _Note_: you will need to decrease the number of iterations of the production rules, since the size of the final sentence grows exponentially with the number of iterations.
X>
X>     Now, notice the symmetry between `L` and `M` in the production rules. The two "move forward" instructions can be differentiated using a `Boolean` value using the following alphabet type:
X>
X>     ```haskell
X>     data Alphabet = L | R | F Boolean
X>     ```
X>
X>     Implement this L-system again using this representation of the alphabet.
X> 1. (Difficult) Use a different monad `m` in the interpretation function. You might try using the `CONSOLE` effect to write the L-system onto the console, or using the `RANDOM` effect to apply random "mutations" to the state type.

## Conclusion

In this chapter, we learned how to use the HTML5 Canvas API from PureScript by using the `purescript-canvas` library. We also saw a practical demonstration of many of the techniques we have learned already: maps and folds, records and row polymorphism, and the `Eff` monad for handling side-effects.

The examples also demonstrated the power of higher-order functions and _separating data from implementation_. It would be possible to extend these ideas to completely separate the representation of a scene from its rendering function, using an algebraic data type, for example:

```haskell
data Scene
  = Rect Rectangle
  | Arc Arc
  | PiecewiseLinear (Array Point)
  | Transformed Transform Scene
  | Clipped Rectangle Scene
  | ...
```

This approach is taken in the `purescript-drawing` package, and it brings the flexibility of being able to manipulate the scene as data in various ways before rendering.

In the next chapter, we will see how to implement libraries like `purescript-canvas` which wrap existing JavaScript functionality, by using PureScript's _foreign function interface_.
