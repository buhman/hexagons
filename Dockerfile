FROM alpine:3.8

ENV CHICKEN_VERSION=4.13.0

RUN apk add --no-cache --virtual .build-deps make gcc libc-dev

RUN wget -qO- https://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz | tar xz

RUN set -ex \
        && cd chicken-${CHICKEN_VERSION} \
        && make PLATFORM=linux \
        && make PLATFORM=linux install

RUN chicken-install \
        matchable combinators clojurian \
        #sdl2 sdl2-ttf \
        tcp6 mailbox \
        test

# fixme: add levo
