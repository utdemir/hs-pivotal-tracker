# pivotal-tracker

[![Build Status](https://img.shields.io/travis/utdemir/hs-pivotal-tracker.svg)](https://travis-ci.org/utdemir/hs-pivotal-tracker)
[![Hackage](https://img.shields.io/hackage/v/pivotal-tracker.svg)](http://hackage.haskell.org/package/pivotal-tracker)

A library and a CLI tool for accessing Pivotal Tracker API.

See [the Hackage page for `pivotal-tracker`](http://hackage.haskell.org/package/pivotal-tracker) for details.

## Building

[Stack](https://www.stackage.org) is the only tool needed to
bootstrap your environment. It will install compiler, all libs necessary
and build the project.

Once stack is [properly installed](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
inside the root directory of this repository use the commands:

    stack setup
    stack build

The setup step is need only the first time you run the project.
To run the tests use:

    stack test

You can also install the [Haskell Platform](https://www.haskell.org/downloads)
and use cabal commands.
