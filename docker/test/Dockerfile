# We use fossa-cli-base instead of fossa-cli-devel because fossa-cli-devel has
# an ADD step, which invalidates the cache for all following steps. This causes
# a rebuild of Kubernetes, which takes _forever_.
FROM fossa-cli-base

# Known good Go projects (some are `git clone`d because `go get` causes
# compilation errors):
## Kubernetes (godep)
RUN mkdir -p $GOPATH/src/k8s.io && \
    cd $GOPATH/src/k8s.io && \
    git clone --depth=1 https://github.com/kubernetes/kubernetes && \
    cd kubernetes && \
    make

## Consul (nested govendor)
### This is currently vendored because of the go-discover vendor.json syntax
### error (see hashicorp/go-discover@266744f)
RUN mkdir -p $GOPATH/src/github.com/hashicorp && \
    cd $GOPATH/src/github.com/hashicorp && \
    git clone --depth=1 https://github.com/fossas/consul

## Docker, Docker CE (vndr)
RUN mkdir -p $GOPATH/src/github.com/docker && \
    cd $GOPATH/src/github.com/docker && \
    git clone --depth=1 https://github.com/docker/docker && \
    git clone --depth=1 https://github.com/docker/docker-ce

# Add FOSSA CLI
ADD . $GOPATH/src/github.com/fossas/fossa-cli
RUN sudo chown -R fossa:fossa $GOPATH/src/github.com/fossas && \
    cd $GOPATH/src/github.com/fossas/fossa-cli && \
    make

WORKDIR $GOPATH/src/github.com/fossas/fossa-cli
CMD [ "/bin/bash", "./test.sh" ]