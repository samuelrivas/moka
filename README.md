# About Moka

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

# Acknowledgements

Special thanks to Diana Parra Corbacho as she had the original idea of using
erlang forms to implement the non-intrusive mock
