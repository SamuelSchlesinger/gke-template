FROM haskell:8.10.2-buster

WORKDIR /opt/service

# Install our dependencies, which will be present on the production image.
RUN apt-get update -y \
&& apt-get upgrade -y \
&& apt-get install -y \
  curl \
  xz-utils \
  gcc \
  make \
  libtinfo5 \
  libgmp-dev \
  zlib1g-dev \
  libpq-dev

# Move the configuration for the project to build our dependencies.
COPY ./server-builder.cabal /opt/service/server-builder.cabal

RUN cabal update \
&& cabal build --only-dependencies -j4 \
&& rm server-builder.cabal
