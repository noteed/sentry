# sentry - Simple process monitoring

This project is in early stage.

Sentry is a process monitoring tool written and configured in Haskell. Its aim
is to keep running programs. For each configured program (called an entry in
its configuration) , multiple processes can be started.

## Usage

Sentry is written in Haskell and is also configured in Haskell. The principle
is to write a short script in `~/.sentry/conf/`. For instance a `dummy.hs`
configuration would look like the following:

    import Sentry.Command
    import Sentry.Types

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

- Newly added entries are not taken into account on re-exec (but deleted and
  altered entries are).
- Dynamically resize the number of processes for a specifif entry.
- Sentry should compile itself its configuration.
- Let Sentry start a configuration instead of manually run it. I.e.
  `sentry start -c dummy` instead of `~/.sentry/conf/dummy`. Default
  configuration could be `sentry.hs`.
- `sentry restart`: `sentry start` should save its PID to ~/.sentry/sentry.pid
  so `sentry restart` can conveniently `kill -HUP <pid>`.
- Move things around (proper module organization).
- Tag a 0.1 version and push it to Hackage.
