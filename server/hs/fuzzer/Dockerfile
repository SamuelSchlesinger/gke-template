FROM us.gcr.io/haskell-learn/server-common as common

FROM us.gcr.io/haskell-learn/server-runner as runner

COPY --from=common /usr/local/bin/fuzzer /usr/local/bin/fuzzer

CMD ["fuzzer"]
