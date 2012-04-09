# Sentry - Simple process monitoring

This project is in early stage.

Sentry is a process monitoring tool written and configured in Haskell. Its aim
is to keep running programs. For each configured program (called an entry in
its configuration) , multiple processes can be started.

## Install

The development version can be installed by cloning the repository and using
cabal:

    > git clone git://github.com/noteed/sentry.git
    > cd sentry && cabal install

## Usage

Sentry is written in Haskell and is also configured in Haskell. The principle
is to write a short script in `~/.sentry/conf/`. For instance a `dummy.hs`
configuration would look like the following:

    import Sentry

    main :: IO ()
    main = sentry
      [ entry "dummy" "sleep" ["4"] 1000 1
      ]

You can then compile `dummy.hs` like this:

    > ghc --make -threaded dummy.hs

Start your Sentry with:

    > ~/.sentry/conf/dummy

Recompiling a configuration can be done while Sentry is running. A SIGHUP will
instruct Sentry to re-exec itself, using the new configuration.

## TODO

- Dynamically resize the number of processes for a specifif entry.
- Let Sentry start a configuration instead of manually run it. I.e.
  `sentry start -c dummy` instead of `~/.sentry/conf/dummy`. Default
  configuration could be `sentry.hs`.
- Move things around (proper module organization).
- Properly use stdout/stderr.
- Properly redirect monitored processes outputs.
- Separate data types for save/restore (i.e. with SafeCopy instances) and
  data types actually used at runtime.
- Tag a 0.1 version and push it to Hackage.
