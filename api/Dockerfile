FROM ubuntu:16.04

RUN apt-get update && \
    apt-get install -y software-properties-common libffi6 zlib1g-dev curl

RUN curl -sSL https://get.haskellstack.org/ | sh

ADD . /src
RUN cd src && \
    stack --no-system-ghc setup && \
    stack build && \
    stack install && \
    cp $(stack path --local-bin-path)/all-o-stasis-api /usr/bin/ && \
    cd / && \
    rm -rf /src

EXPOSE 80
CMD ["/usr/bin/all-o-stasis-api", "80"]
