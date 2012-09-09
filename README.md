# Moka

## Overview
Moka is a tool to break test dependencies. Similar to other mocks, it allows a
module A depending on a module B to run without the need of the whole B
behaviour.

There are a number of tools like this for Erlang, being Meck the most
popular. However Moka uses a different approach, it wouldn't modify the
behaviour of B at all, but the behaviour of A.

Of course, mocking with either Moka's or Meck's approach is grossly equivalent
in many situations. However, we think that modifying the caller module instead
of the called module is a cleaner and safer.

But the main motivation behind Moka is that it allows us to mock (or "mok")
modules that are used by other modules we cannot control during our testing.
Good examples are `file:read`, or `erlang:now`. If you try to mock those with
Meck, you will most likely break something in your system (e.g. Meck itself may
fail to compile the mocked module as the compiler depends on `file`).

There are, however, some drawbacks in Moka's approach:

 * Moka needs the module under test source code, so it must have
   been compiled with `debug_info`
 * Mocking dynamic calls is not straight forward (Moka cannot do it in its
   current state), so calls like `Module:Function(...)` will still call the
   original dependency

## Compiling

The compilation process is likely to change in the future, for now it relies
entirely on [rebar](https://github.com/basho/rebar). This should work for you:

```
git clone git://github.com/samuelrivas/moka.git
cd moka
./rebar get-deps
./rebar compile
```

To check everything works as expected, `./rebar eunit apps=moka` should run the
test cases successfully with a coverage about 80%. You will get some cover
warnings like those, as Moka don't play well with cover yet (cover fails for
those modules that are moked as part of the testing process).

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

Start a shell with the code path set so that you can call this module and start
the moka application (you have to do it manually for now). Check that it works
as expected.

Now we are going to mock it as if we wanted to unit test the code without
setting any special files anywhere:

    1> short_demo:read_a_file("Whatever.txt").
    {error,enoent}
    2> Moka = moka:start(short_demo).
    short_demo_moka
    3> moka:replace(Moka, file, read_file,
           fun(File) ->
               "So you really want to read " ++ File ++ ", do you?"
           end).
    ok
    4> moka:load(Moka).
    ok
    5> short_demo:read_a_file("Whatever.txt").
    "So you really want to read Whatever.txt, do you?"
    6> file:read_file("Whatever.txt").
    {error,enoent}
    7> moka:stop(Moka).
    ok
    8> short_demo:read_a_file("Whatever.txt").
    {error,enoent}

Just in case something breaks and leaves you with a modified version of the
moked module, you can restore the old code with
`moka_mod_utils:restore_module/1`.

Also, the first acceptance test we wrote to motivate our work must be
passing. You can check it out as another example of what Moka can do:
[mock_used_functions](https://github.com/samuelrivas/moka/blob/master/test/acceptance/mock_used_functions_eunit.erl)

## Current Status

The current status is that moking explicit calls to other modules *should*
work. All the tests pass, including a proper fsm test that checks a lot of
different call combinations.

However, Moka is quite young, and little battle-tested, so you should expect
some quirks, and, of course, lack of functionality. For instance, no checking
can be done on whether the expected calls have been done, like you would do with
Meck. You can use the function that replaces the moked call, as it is used in
`mock_used_functions` acceptance test, but that will probably have some
limitations.

Feedback about desired improvements is very welcome (and collaborations even
more :))

## Acknowledgements

Special thanks to Diana Parra Corbacho as she had the original idea of using
erlang forms to implement the non-intrusive mock.
