FROM us.gcr.io/haskell-learn/server-builder as builder

# Move the configuration for the project to build our dependencies.
COPY ./server.cabal /opt/service/server.cabal

RUN cabal build --only-dependencies -j4

# Move the rest of the project over to build that.
COPY . /opt/service

RUN cabal install --installdir=/usr/local/bin
