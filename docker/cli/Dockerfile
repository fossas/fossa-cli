# This Dockerfile contains the base tools and the pre-compiled FOSSA CLI.
FROM quay.io/fossa/fossa-cli-base

# Add FOSSA CLI
COPY . $GOPATH/src/github.com/fossas/fossa-cli
RUN sudo chown -R fossa:fossa $GOPATH/src/github.com/fossas && \
    cd $GOPATH/src/github.com/fossas/fossa-cli && \
    make

WORKDIR $GOPATH/src/github.com/fossas/fossa-cli
CMD [ "/bin/bash" ]
