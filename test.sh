#!/usr/bin/env bash

set -e

# Test known good Go projects:

# Download Kubernetes
mkdir -p $GOPATH/src/k8s.io
cd $GOPATH/src/k8s.io
git clone --depth=1 https://github.com/kubernetes/kubernetes
cd kubernetes

# Run build
fossa init
fossa analyze --output

# Download Consul
go get -u github.com/hashicorp/consul
cd $GOPATH/src/github.com/hashicorp/consul

# Run build
fossa init
fossa analyze --output