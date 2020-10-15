FROM us.gcr.io/haskell-learn/server-common as common

FROM us.gcr.io/haskell-learn/server-runner as runner

COPY --from=common /usr/local/bin/server /usr/local/bin/server

CMD ["server"]
