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

The system not ready to use from complete, but there is something you can
already test. Write a module like this, compile it with `debug_info` and place
the beam somewhere in the code search path:

    -module(short_demo).
    
    -export([read_a_file/1]).
    
    read_a_file(File) ->
        file:read_file(File).

Go to a shell, check that it works as expected.

Now we are going to mock it s if we wanted to unit test the code without setting
any special files anywhere:

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

Don't play to much with it though, we are not using supervisors, and also call
handlers are known to remain dangling. The Mokas can stop working at any moment.

Just in case something breaks and leaves you with a modified version of the
Moked module, you can restore the old code with
`moka_mod_utils:restore_module/1`.

## Current Status

The current status is shaky, in fact it still doesn't work. However the first
two days of development (during SpawnFest 2012) we focused on quality and a good
bakcing test suite, so the code is in good shape and is almost flying. The
acceptance tests are quite close to pass, once we save a technicality with the
call handler (we made it anonymous, but now we realised we cannot embed pids in
the source code ...)

As a small experience report, we wanted to check whether TDD and property based
testing will work for such a short time. We are not really sure if we could have
gone further following more the hackish way, but we doubt it. Anyway, first day
was a bit frustrating, with very little done but some diagrams in paper and some
failing test cases. Second day the functionality skyrocketed, despite one of us
being sick in bed (which left the other one in a team with 50% the intended
workforce :)). Late night hacking the second day was only possible because we
could run our test suite to check whether we were progressing or not. Sadly the
main test suite is still not passing, but we need some medium changes to make it
pass yet ...

## Acknowledgements

Special thanks to Diana Parra Corbacho as she had the original idea of using
erlang forms to implement the non-intrusive mock
