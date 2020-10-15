cd server/builder
docker build . -t us.gcr.io/haskell-learn/server-builder
docker push us.gcr.io/haskell-learn/server-builder
cd -

cd server/runner
docker build . -t us.gcr.io/haskell-learn/server-runner
docker push us.gcr.io/haskell-learn/server-runner
cd -

cd server/hs
docker build . -t us.gcr.io/haskell-learn/server-common
docker push us.gcr.io/haskell-learn/server-common
cd -

cd server/hs
docker build . -t us.gcr.io/haskell-learn/server --file main/Dockerfile
docker push us.gcr.io/haskell-learn/server
cd -

cd server/hs
docker build . -t us.gcr.io/haskell-learn/migrate --file migrate/Dockerfile
docker push us.gcr.io/haskell-learn/migrate
cd -

cd server/hs
docker build . -t us.gcr.io/haskell-learn/fuzzer --file fuzzer/Dockerfile
docker push us.gcr.io/haskell-learn/fuzzer
cd -
