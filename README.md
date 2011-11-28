cabal-src is a package intended to help solve the Cabal diamond dependency
problem.

## The problem

Let's say you have three packages. foo depends on the text package, and can use
any version of it. bar depends on text as well, but requires version 0.10.\*.
foobar depends on both of those.

If you upload these three packages to Hackage and install foobar, cabal will
build foo and bar against the same version of text, and then build foobar
against them. However, if you install these packages locally, foo will be built
against the most recent version of text (currently 0.11.something), and then
foobar will be unbuildable, since its dependencies depend on different versions
of text.

You can see sample packages demonstrating the issue in the *example* folder.

This is just one example of the diamond dependency issue. When dealing with
complicated systems such as Yesod, with dozens of packages that are in
development, the situation becomes much worse.

## Our solution

The important thing to note is that, if the packages are on Hackage, Cabal can
handle the situation. The reason is that Cabal has enough information to
calculate the correct versions of all dependencies to be used. So our goal is
to give Cabal access to information on all dependencies, even those not yet on
Hackage.

Instead of installing a local package with "cabal install", you can now use
"cabal-src-install". This program actually calls out to "cabal install", and if
that build succeeds, will perform the following steps:

1. Create a source tarball via "cabal sdist"

2. Copy this tarball into a special location in your .cabal folder.

3. Update a 00-index.tar file specifically kept for cabal-src.

4. Update your .cabal/config file to recognize the special cabal-src folder as necessary.

If you now install your "foo" and "bar" packages via "cabal-src-install", Cabal
has full access to their source code. When it comes time to install foobar,
Cabal can determine that foo can be recompiled with text 0.10 and will do so
automatically.

## Project status

This software should be considered alpha. We'll likely be using it for all
Yesod development going forward, so I expect that alpha to be upgraded to beta
and finally production quality in short order. All feedback is welcome!

## Usage

Simply replace a call to "cabal install" with a call to "cabal-src-install".
If you would like to only install the source tarball without actually
installing the binary package, run it with "cabal-src-install --src-only".
