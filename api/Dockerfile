FROM haskell AS builder

WORKDIR /src

ADD stack.yaml .
RUN stack setup

ADD package.yaml .
RUN stack install --only-dependencies

ADD . /src
RUN stack install && cp $(stack path --local-bin)/all-o-stasis-api /usr/bin/

FROM haskell
COPY --from=builder /usr/bin/all-o-stasis-api /usr/bin
CMD ["/usr/bin/all-o-stasis-api"]
