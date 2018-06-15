#!/usr/bin/env bash

set -e

# Test known good Go projects:
## FOSSA CLI (dep)
cd $GOPATH/src/github.com/fossas/fossa-cli
fossa init
time fossa analyze --output go:./cmd/fossa

## Kubernetes (godep)
cd $GOPATH/src/k8s.io/kubernetes
fossa init
time fossa analyze --output --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver

## Consul (govendor)
cd $GOPATH/src/github.com/hashicorp/consul
fossa init
time fossa analyze --output --option allow-nested-vendor:true --option allow-deep-vendor:true go:.