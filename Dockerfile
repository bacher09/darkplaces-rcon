FROM haskell:8.10 as builder
RUN cabal update && cabal install alex
COPY . /src
WORKDIR /build
RUN cd /build && cabal v1-sandbox init && \
    cabal v1-sandbox add-source /src/darkplaces-text/ && \
    cabal v1-sandbox add-source /src/darkplaces-rcon/ && \
    cabal v1-sandbox add-source /src/darkplaces-rcon-util/ && \
    cabal v1-install --enable-split-objs --enable-split-sections \
            --enable-static --enable-executable-static darkplaces-rcon-util
RUN cp /build/.cabal-sandbox/bin/drcon /usr/local/bin/drcon

FROM alpine:3.12
COPY --from=builder /usr/local/bin/drcon /usr/bin/
