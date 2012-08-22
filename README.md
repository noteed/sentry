# Sentry - Simple process monitoring

This project is in early stage.

Sentry is a process monitoring tool written and configured in Haskell. Its aim
is to keep running programs. For each configured program (called an entry in
its configuration) , multiple processes can be started.

Currently Sentry runs on Linux only.

## Install

The development version can be installed by cloning the Git repository and
using cabal:

    > git clone git://github.com/noteed/sentry.git
    > cd sentry && cabal install

## Usage

Sentry is written in Haskell and is also configured in Haskell. The principle
is to write a short script in `~/.sentry/conf/`. For instance a `sample.hs`
configuration would look like the following:

    import Sentry.Server

    main :: IO ()
    main = sentry
      [ entry "dummy" "sleep" ["4"] 1000 1
      ]

`"dummy"` is the entry name, also called the process type. `"sleep"` and
`["4"]` is the command that need to be run (and kept running), together with
its arguments. The `1000` value specifies how long in milliseconds Sentry has
to wait before restarting the command when it dies. Finally `1` is the number
of processes you want to run that particular process type. You can then compile
`sample.hs` and obtain a `sample` binary like this:

    > ghc --make -threaded sample.hs

Start your Sentry with:

    > ~/.sentry/conf/sample start

Sentry will save its PID to `sample.pid` and start running the "dummy" entry:

    Sentry started (PID: 1511s aved in /home/thu/.sentry/conf/sample.pid).
    20:17:51 dummy.1514      Started at 1334081871.
    20:17:55 dummy.1514      Exited at 1334081875 with ExitSuccess.
    20:17:56 dummy.1516      Started at 1334081876.
    20:18:00 dummy.1516      Exited at 1334081880 with ExitSuccess.
    ^CBye.
    >

Whenever `sleep` exits, Sentry will run it again, and again.

Recompiling a configuration (i.e. `sample.hs` in the example) can be done while
Sentry is running. A SIGHUP will instruct Sentry to re-exec itself, using the
new configuration. The command (while Sentry is already running)

    > ~/.sentry/conf/sample reload

will just do that:

    > .sentry/conf/sample start
    Sentry started (PID: 1530saved in /home/thu/.sentry/conf/sample.pid).
    20:32:01 dummy.1536      Started at 1334082721.
    20:32:05 dummy.1536      Exited at 1334082725 with ExitSuccess.
    /home/thu/.sentry/conf/sample.hs successfully compiled.
    Sentry reexec'd. Initially started at 1334082711 (Previously reexec'd at 1334082711).
    20:32:08 dummy.1550      Started at 1334082728.
    20:32:12 dummy.1550      Exited at 1334082732 with ExitSuccess.
    ^CBye.
    >

## HTTP interface

Sentry has an embedded web server to exposes its state over HTTP. It is also
used to accept reconfiguration commands (e.g. scale a given process type).

## Documentation

This `README.md` file should be a good starting point. Additional information
can be found in the Haddock pages. Those pages are currently available from
http://hypered.be/haddock/sentry.

## Design

Sentry is configured with a list of process specifications. For each
specification, multiple processes can be spawn and monitored.

Sentry spawns a process by forking itself and then exec'ing the process'
command. Waiting for the process to complete is done in a lightweight thread.
Forking and waiting a child process is a more accurate way to know when the
process dies than polling. (Another good way to do that is to use netlink with
the process events connector.)

A main thread reads commands from a concurrent queue (a `Chan` in Haskell).
Commands are pushed by signal handlers and monitoring threads to instruct the
main thread to act on its configuration (and thus change its set of monitored
processes).

## TODO

- Dynamically resize the number of processes for a specific entry.
- Let Sentry start a configuration instead of manually run it. I.e.
  `sentryd start -c sample` instead of `~/.sentry/conf/sample`. Default
  configuration could be `sentry.hs`.
- Separate data types for save/restore (i.e. with SafeCopy instances) and
  data types actually used at runtime.
- Build on 7.0 and 7.4 (was tested only on 7.0.3 so far).
