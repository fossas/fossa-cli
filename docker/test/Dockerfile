# This Dockerfile contains the base tools and the pre-compiled FOSSA CLI.
FROM quay.io/fossa/fossa-cli-test-base

# Add FOSSA CLI
COPY --chown=fossa:fossa . $GOPATH/src/github.com/fossas/fossa-cli
RUN cd $GOPATH/src/github.com/fossas/fossa-cli && \
    mkdir -p .git && \
    make

WORKDIR $GOPATH/src/github.com/fossas/fossa-cli
CMD [ "/bin/bash", "./test.sh" ]
