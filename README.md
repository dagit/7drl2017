[![Build
Status](https://travis-ci.org/dagit/7drl2017.svg?branch=master)](https://travis-ci.org/dagit/7drl2017)

This is an attempt to build a roguelike game in 7 days (or less).

# Building

The most tested way to build this project is using the `new-build` feature of
`cabal`:

```
cabal new-build
```

Sandboxes should also work:

```
cabal sandbox init
cabal install
cabal build
```

# Playing

The game executable is named `7drl2017`. If you build with `new-build`, you'll
have to find and run the executable yourself as there is no `new-exec` as of
writing this. If you use sandboxes instead, you should able to type `cabal exec
7drl2017`.

Right now game play is very simple. You can walk around and exit.

# TODO

Everything.
