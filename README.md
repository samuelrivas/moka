# Moka

## Overview
Moka is a tool to break test dependencies. Similar to other mocks, it allows a
module A depending on a module B to run without the need of the whole B
behaviour.

There are a number of tools like this for Erlang, being meck the most
popular. However Moka is unique in one way, it wouldn't modify the behaviour of
B at all, but the behaviour of A.

Why is that a good thing. Well, just try to mock file:read with meck. If it
works (at the time of writing this there is an open issue about meck hanging
when mocking anything from file), you will most likely break something in your
system. You will change the behaviour of file:read for *all* modules that use
it.

As a drawback, Moka needs the module under test source code, so it must have
been compiled with debug info.

## Demo!

The system not ready to use reliably yet, but there is something you can already
test. Write a module like this, compile it with `debug_info` and place the beam
somewhere in the code search path:
```erlang
-module(short_demo).

-export([read_a_file/1]).

read_a_file(File) ->
    file:read_file(File).
```
Go to a shell, check that it works as expected.

Now we are going to mock it as if we wanted to unit test the code without
setting any special files anywhere:

    1> short_demo:read_a_file("Whatever.txt").
    {error,enoent}
    2> Moka = moka:start(short_demo).
    <0.2633.0>
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
Moked module, you can restore the old code with
`moka_mod_utils:restore_module/1`.

Also, the first acceptance test we wrote to motivate our work is now
passing. You can check it out as another example of what Moka can do: [mock_used_functions](https://github.com/samuelrivas/moka/blob/master/test/acceptance/mock_used_functions_eunit.erl)

## Current Status

The current status is shaky and incomplete. It is only possible to replace calls
in with the form of `module:call(...).` No other type of calls will work. From
support is not complete either. It is not rare that Moka cannot handle a module
because it contains code Moka still doesn't understand.

I am planning to experiment a bit with abstract syntax trees instead of abstract
forms instead of developing full support for abstract forms, but that work is
not yet completed.

## Acknowledgements

Special thanks to Diana Parra Corbacho as she had the original idea of using
erlang forms to implement the non-intrusive mock
