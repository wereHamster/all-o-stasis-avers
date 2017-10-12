FROM b.gcr.io/ewa/haskell:v5-gbcd2905-t20160117172806

RUN apt-get update && \
    apt-get install -y libffi6 zlib1g-dev curl

ADD . /src
RUN cd src && \
    stack setup && \
    stack install && \
    cp $(stack path --local-bin-path)/all-o-stasis-api /usr/bin/ && \
    cd / && \
    rm -rf /src

EXPOSE 80
CMD ["/usr/bin/all-o-stasis-api", "80"]
