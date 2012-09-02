firefly
=======

Firefly is a Haskell 2D game library implemented as a thin but high-level layer
over SDL and OpenGL.

It is meant to be **lightweight**, **fast** and **easy**. It is designed as a
*library*, not as a *framework*, so you should be able to use it with pretty
much anything.

We embrace the notion that Haskell is the world's finest imperative language and
expose a very imperative API. However, it's very easy to add something more
declarative (FRP in particular) on top of it.

Dependencies
------------

Outside of Haskell, Firefly depends on:

- OpenGL, for fast graphics;
- libpng, for image loading;
- SDL, for pretty much anything else.

It should relatively painless to install these on your operating system.
