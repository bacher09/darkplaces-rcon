This is meta repository for haskell packages that's related
to darkplaces or quakes rcon protocol.
Currently it contains these packages:

* darkplaces-rcon &mdash; library for darkplaces rcon protocol
* darkplaces-rcon-util &mdash; client rcon utility


## Building from sources

These instructions will assume that you want to build all
repos hosted in this meta-repo.
First you need to install these tools:

* [GHC] >= 7.4 (recommended GHC >= 7.8)
* cabal-install &mdash; haskell package manager
* [Alex] &mdash; haskell lexer generator (same thing as C's lex)
* C compliller &mdash; required for compiling some dependent packages

Also you can choose easy way and install [Haskell Platform]
that contains all these components (except C compiler).

Now you may create build folder, clone repo and some
depended repo's and build packages.

    $ mkdir build && cd build
    $ export BUILD_DIR=$(pwd)
    $ git clone https://github.com/bacher09/darkplaces-rcon.git
    $ git clone https://github.com/bacher09/darkplaces-text.git
    $ cabal sandbox init
    $ cabal sandbox add-source $BUILD_DIR/darkplaces-rcon/darkplaces-text/
    $ cabal sandbox add-source $BUILD_DIR/darkplaces-rcon/darkplaces-rcon/
    $ cabal sandbox add-source $BUILD_DIR/darkplaces-rcon/darkplaces-rcon-util/

After this commands you will see folder `.cabal-sandbox` and file `cabal.sandbox.config`.
Folder `.cabal-sandbox` is place where packages and binaries will be installed after build.
Now you can build and install these packages.

    $ cabal install darkplaces-rcon
    $ cabal install darkplaces-rcon-util

If you want to reinstall package you can add `--reinstall` flag to cabal.

    $ cabal install --reinstall darkplaces-rcon-util

You can find binary in `.cabal-sanbox/bin/` folder.
If you'd like to call `drcon` util without providing full path
you can add it to yours PATH environment variable.

    $ export PATH=$BUILD_DIR/.cabal-sandbox/bin/:$PATH

[Haskell platform]: https://www.haskell.org/platform/
[GHC]: https://www.haskell.org/ghc/
[Alex]: https://www.haskell.org/alex/
