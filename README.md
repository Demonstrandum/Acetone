# Acetone

Haskell library for 2D interactive graphics.

## Overview

- All spatial units are normalised by default. e.g., `0.0` is all the
  way to the left, and `1.0` is all the way to the right on the shortest axis.
  Pixel units are still supported, and other coördinate transforms are trivial.
  A `fromPixels pixels` function is provided.
- The default coordinate system has the origin in the top-left corner.

## TODO

- Resizing the window does not behave properly on Windows.
- Add gradient and texture support.
- Add font/text layouting and rasterization with FreeType.

## Install and Build

### Get Haskell

Windows, macOS and GNU+Linux users can get GHC and Cabal with
[GHCUp](https://www.haskell.org/ghcup/)

### Note to Windows users

This repository/package's structure relies on symlink.
The latest versions of Windows should support this fully.
By enabling Developer Mode on Windows, you should be granted `mklink`
permission.
Symlinks have been supported since Vista, but needed to be
enabled and needed administrator privileges.

You must have Git set up to treat symlinks correctly, by doing as so:
```sh
git config --global core.symlinks true
```
in the Git CLI/Bash application.

### Build and install this package

Once in the repository, having completed the prerequisites, one can:

```sh
cabal update
cabal build
cabal install
```

## Example

Run the GLFW based test/example program:

```sh
cd example
cabal run example-acetone
```
