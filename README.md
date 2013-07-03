# Moka

## Overview

Moka is a tool for breaking test dependencies. Similar to other mocks, it allows
a module A depending on a module B to run without depending upon the complete
behaviour of module B.

There are a number of tools like this for Erlang, being
[Meck](https://github.com/eproxus/meck) the most popular. Both Moka and Meck
will create a stub of B (say B') with some controlled behaviour, and make A call
B' instead of B.

Meck achieves that by replacing the module B with B', meaning that A, and any
other module calling B, will get the modified behaviour (B') instead.

Moka uses a different approach. Instead of replacing B, it modifies the
behaviour of A by redirecting mocked calls to B' but retaining the unmocked
calls to B.

Of course, mocking with either Moka's or Meck's approach is largely equivalent
in most situations. However modifying the caller module instead of the called
module is cleaner and safer.

The main motivation for building Moka is that it allows us to mock (or _mok_)
modules that are used by other modules we cannot control during our testing.
Good examples are `file:read`, or `erlang:now`. If you try to mock those with
Meck, you will most likely break something in your system (e.g. Meck itself may
fail to recompile the mocked module as the compiler depends on `file`).

There are, however, some drawbacks in Moka's approach:

 * Moka needs the module under test source code, so it must be compiled with
   `debug_info`
 * It is easier to couple tests with internal implementation details
 * Mocking dynamic calls is not straight forward (Moka cannot do it in its
   current state), so calls like `Module:Function(...)` will still call the
   original dependency

## Features

This is a list of the main Moka capabilities, linked to their acceptance tests:

 * Replace static, fully-qualified calls with a fun
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/mok_system_functions.erl)
 * Replace static, internal function calls with a fun
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/mok_internal_functions.erl)
 * Make internal functions available for testing
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/dynamic_exports.erl)
 * Read the history of function calls handled by a moka
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/hold_state.erl)
 * Handle exceptions transparently
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/mok_exceptions.erl)

## Compiling and Testing

Currently, the compilation process delegates on
[rebar](https://github.com/basho/rebar), but it is wrapped in a Makefile. You
need Internet access to download the dependencies the first time you
compile. This should compile and generate the documentation:
```
git clone git://github.com/samuelrivas/moka.git
cd moka
make all
```

To check everything works as expected, `make test` should run the
test cases successfully with a coverage about 80%.

The tests are run twice, once with cover disabled and once with cover
enabled. The reason to have a run without cover is that moka needs to know
whether a module is cover-compiled, so some behaviour changes when moking
cover-compiled modules.

At the end of the cover-compiled test run you will see some warnings like these:

```
ERROR: Cover analyze failed for dynamic_exports_aux: {not_cover_compiled,
                                                     moka_test_module}
       "/home/samuel/.../moka/.eunit/moka_test_module.beam"
```

This is because rebar cover compiles everything, runs the tests, and then
collects the results. However some tests load (or re-load) binaries, without
restoring the previously loaded version. When rebar tries to collect the results
for those modules it fails since they are no longer cover-compiled. Most of the
tests take care of this and restore the previously loaded module, but a few of
them would become too cumbersome, so we decided to live with the warning. Those
modules are just test modules that no other application should use anyway.

`make check` runs static checks on the code. Currently, some predefined xref
analysis and a dialyzer check.

## Some Words on Cover

The integration with cover is a bit tricky. Without going too deep in the
technical details, the bottom line is:

 * The accumulated coverage before a module is moked is restored after stopping
   the moka, but it will not be increased while the module is moked. That
   applies to the module specified in `moka:start`, but not to the target
   modules (when calling `moka:replace` for example). Coverage for the latter
   still accumulates.
 * If you attempt to get coverage information for a module while it is moked you
   will confuse cover and probably lose all coverage information. Moka may not
   be able to restore the coverage figures after calling `moka:stop`.
 * Cover compiling a module while it is moked will confuse moka, the code will
   no longer be moked, but moka processes may still be running in inconsistent
   state.

In most of the usual situations moka will play well with cover anyway, just
remember that coverage doesn't increase while the module is moked. Solving this
issues is in the backlog, but not very high, as it seems to require some dirty
hacking.

## Running

When starting a new erlang node to use Moka, make sure you include Moka's ebin
dir in the code search path, as well as the ebin dirs of the dependencies.

## Demo!

Write a module like this, compile it **with `debug_info`** and place the beam
somewhere in the code search path:

```erlang
-module(short_demo).

-export([read_a_file/1]).

read_a_file(File) ->
    file:read_file(File).
```

Start a shell with the code path set so that you can call this module and
**start the moka application** (you have to do it manually for now). Check that
it works as expected.

Now we are going to mock it as if we wanted to unit test the code without
setting any special files anywhere:

    1> sel_application:start_app(moka).
    [samerlib,moka]
    2> short_demo:read_a_file("Whatever.txt").
    {error,enoent}
    3> Moka = moka:start(short_demo).
    short_demo_moka
    4> moka:replace(Moka, file, read_file,
           fun(File) ->
               "So you really want to read " ++ File ++ ", do you?"
           end).
    ok
    5> moka:load(Moka).
    ok
    6> short_demo:read_a_file("Whatever.txt").
    "So you really want to read Whatever.txt, do you?"
    7> file:read_file("Whatever.txt").
    {error,enoent}
    8> moka:stop(Moka).
    ok
    9> short_demo:read_a_file("Whatever.txt").
    {error,enoent}

Just in case something breaks and leaves you with a modified version of the
moked module, you can restore the old code with
`moka_mod_utils:restore_module/1`.

## Current Status

The current status is that moking explicit calls to functions (either local or
external) *should* work. All the tests pass, including a proper fsm test that
checks a number of different call combinations.

However, Moka is quite young, and little battle-tested, so you should expect
some quirks, and, of course, lack of functionality. Feedback about desired
improvements is very welcome (and collaborations even more :))

# Branching

 * [![Build
   Status](https://secure.travis-ci.org/samuelrivas/moka.png?branch=master)](http://travis-ci.org/samuelrivas/moka)
   the `master` branch points to the latest released tag
 * [![Build
   Status](https://secure.travis-ci.org/samuelrivas/moka.png?branch=develop)](http://travis-ci.org/samuelrivas/moka)
   the `develop` branch points to the latest merged change. This branch always
   contains `master`

Both `master` and `develop` are safe to branch off and rebase on as they are
guaranteed not to change their history or be ever deleted. Any other branch is
considered temporary and may be rebased or deleted without notice.

Also, `master` and `develop` are not allowed to contain merge commits. Any merge
from another branch is done rebasing it first to the HEAD of `develop`.

## Acknowledgements

Special thanks to Diana Parra Corbacho as she had the original idea of using
erlang forms to implement the non-intrusive mock.

Thanks to the organisers of [Spawnfest,](http://spawnfest.com/) the development
of Moka started as a 48 hours clock race in the 2012 eddition (and won some
niceties for the authors).
