# Moka

## Overview

Moka is a tool to break test dependencies. Similar to other mocks, it allows a
module A depending on a module B to run without the need of the whole B
behaviour.

There are a number of tools like this for Erlang, being
[Meck](https://github.com/eproxus/meck) the most popular. Both Moka and Meck
will create a stub of B (say B') with some controlled behaviour, and make A call
to B' instead of B.

Meck achieves that by replacing the module B with B', that means that A, and any
other module calling B, will get the modified behaviour.

Moka uses a different approach, it wouldn't replace B, but modify the behaviour
of A redirecting any calls to B to B'.

Of course, mocking with either Moka's or Meck's approach is grossly equivalent
in many situations. However, we think that modifying the caller module instead
of the called module is a cleaner and safer.

The main motivation for building Moka is that it allows us to mock (or "mok")
modules that are used by other modules we cannot control during our testing.
Good examples are `file:read`, or `erlang:now`. If you try to mock those with
Meck, you will most likely break something in your system (e.g. Meck itself may
fail to recompile the mocked module as the compiler depends on `file`).

There are, however, some drawbacks in Moka's approach:

 * Moka needs the module under test source code, so it must have
   been compiled with `debug_info`
 * It is easier to copule tests with internal implementation details
 * Mocking dynamic calls is not straight forward (Moka cannot do it in its
   current state), so calls like `Module:Function(...)` will still call the
   original dependency

## Features

This is a list of the main Moka capabilities, linked to their acceptance tests:

 * Replace static, fully-qualified calls with a fun
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/mok_system_functions.erl)
 * Make internal functions available for testing
   [(test)](https://github.com/samuelrivas/moka/blob/master/test/acceptance/dynamic_exports.erl)

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
test cases successfully with a coverage about 80%. You will get some cover
warnings like those, as Moka don't play well with cover yet (cover fails for
those modules that are moked as part of the testing process).

`make check` will run static checks on the code. Currently it just runs some
predefined xref analysis.

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

The current status is that moking explicit calls to other modules *should*
work. All the tests pass, including a proper fsm test that checks a lot of
different call combinations.

However, Moka is quite young, and little battle-tested, so you should expect
some quirks, and, of course, lack of functionality. For instance, no checking
can be done on whether the expected calls have been done, like you would do with
Meck. You can use the function that replaces the moked call, as it is used in
`mok_system_functions` acceptance test, but that will probably have some
limitations.

Feedback about desired improvements is very welcome (and collaborations even
more :))

# Branching

 * [![Build
   Status](https://secure.travis-ci.org/samuelrivas/moka.png?branch=master)](http://travis-ci.org/samuelrivas/moka)
   the `master` branch points to the latest released tag
 * [![Build
   Status](https://secure.travis-ci.org/samuelrivas/moka.png?branch=develop)](http://travis-ci.org/samuelrivas/moka)
   the `develop` branch points to the latest merged change. This branch always
   contains `master`. It is not possible to re-run [Travis
   CI](https://travis-ci.org/) for non master branches, so this branch may be
   red because an eventual failure of the Travis machinery until a new commit is
   merged

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
