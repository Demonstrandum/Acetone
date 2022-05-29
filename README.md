# Acetone

Haskell library for 2D interactive graphics.

## Overview

- All spatial units are normalised by default. e.g., for width `0.0` is all the
  way to the left, and `1.0` is all the way to the right. Pixel units are still supported
  and are trivial to calculate. A `fromPixels pixels` function is provided.
- The default coordinate system has the origin in the bottom-left corner.

## Build

```sh
cabal install
```

## Example

Run the GLFW based test/example program:

```sh
cd example
cabal run example-acetone
```
