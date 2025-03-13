FROM alpine:3.21 AS alpine-haskell
RUN  apk update && apk add ghc cabal libc-dev zlib-static zlib-dev zlib-ng-dev ncurses-dev ncurses-static
RUN cabal update

FROM alpine-haskell as builder
COPY . /src
WORKDIR /src
RUN mkdir installdir && cabal install darkplaces-rcon-util --flags=LinuxStatic \
        --enable-split-sections --enable-static --enable-executable-static \
        --installdir=./installdir

FROM scratch
COPY --from=builder /src/installdir/drcon /
ENTRYPOINT ["/drcon"]
