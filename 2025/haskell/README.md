# AOC 2025

## Setup

You are expected to have `ghcup` installed on your machine.

Run `./ghcup-set.sh` to set the intended version of GHC, cabal and HLS via GHCup.

If any of the versions needed are not yet installed on your machine, you can use `ghcup tui` to install them.

It might work with other combination of versions also, but this combo works for sure.

If all is good, `cabal build` should complete successfully for you.

## Organization of the codebase

`src/` contains all the Haskell code. `src/Day01.hs`, `src/Day02.hs`, ... each contain the solution
for that specific day, and they are all imported into the `src/Main.hs`, which is the executable
that we use to easily run the solution for the specific day.

`data/` dir is where puzzle input files go for each day.
Your code in `src/DayXX.hs` can then read those files as needed.
These data files are gitignored, since AoC authors ask not to publicly post your puzzle input files.

## Running

Easiest way is calling `./day <num>` to run specific day, e.g. `./day 1`.

You can also use `cabal` or `cabal repl` directly, e.g. `cabal repl src/Day01.hs`.
