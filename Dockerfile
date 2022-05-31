FROM haskell:8.10

WORKDIR /workdir

RUN cabal update
COPY arepa.cabal /workdir/arepa.cabal

RUN cabal build --only-dependencies -j4

RUN apt-get update && apt-get -y install clang

ENTRYPOINT ["/bin/bash"]