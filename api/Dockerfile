FROM haskell

WORKDIR /src

ADD stack.yaml .
RUN stack setup

ADD *.cabal .
RUN stack install --only-dependencies

ADD . /src
RUN stack install && cp $(stack path --local-bin)/all-o-stasis-api /usr/bin/

CMD ["/usr/bin/all-o-stasis-api"]
