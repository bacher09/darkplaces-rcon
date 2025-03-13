# darkplaces-rcon (drcon)

This is a meta repository for Haskell packages that's related
to darkplaces or quakes rcon protocol.
Currently, it contains two packages:

* darkplaces-rcon &mdash; library for darkplaces rcon protocol
* [darkplaces-rcon-util][drcon] &mdash; client rcon utility `drcon`

## Demo

![Simple demo][demogif]

Full [demo].

## Building with docker

This is the simplest way to build this project from sources and produces a binary that supposed to be portable across different Linux distributions

```
git clone --recurse-submodules https://github.com/bacher09/darkplaces-rcon.git
cd darkplaces-rcon
mkdir output
docker build --output=./output
strip ./output/drcon # optional, strip binary from debug symbols
```
Result binary would be in `./output` directory.

## Building manually
 
First you need to install these tools:

* [GHC] >= 8.2 (recommended GHC is 9.8)
* cabal-install &mdash; haskell package manager
* C compiler &mdash; required for compiling some dependent packages as well as proper header files (e.g. `zlib-dev`, `ncurses-dev`).

As an option, you can install [ghcup] which can be used to install GHC and cabal:

```
ghcup install ghc 9.8
ghcup install cabal
```

You can also use `ghcup tui` if you prefer text ui.

Now you can compile package:

```
git clone --recurse-submodules https://github.com/bacher09/darkplaces-rcon.git
cd darkplaces-rcon
mkdir output
cabal install darkplaces-rcon-util --installdir=./output
```

Afterward, `output` directory should have a symlink to the compiled binary.

Now you can check tool [manual][drcon].


[GHC]: https://www.haskell.org/ghc/
[drcon]: ./darkplaces-rcon-util/README.md
[ghcup]: https://www.haskell.org/ghcup/install/
[demogif]: ./darkplaces-rcon-util/demo.gif
[demo]: https://asciinema.org/a/20146
