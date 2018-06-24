#!/usr/bin/env bash

# TODO: move these into `*_test.go` files.

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

## Docker (vndr)
cd $GOPATH/src/github.com/docker/docker
fossa init
time fossa analyze --output --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd

## Docker CE (vndr)
cd $GOPATH/src/github.com/docker/docker-ce
fossa init
time fossa analyze --output --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd

## InfluxDB (gdm)
cd $GOPATH/src/github.com/influxdata/influxdb
fossa init
time fossa analyze --output --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd

## rkt (glide)
cd $GOPATH/src/github.com/rkt/rkt
fossa init
time fossa analyze --output go:./rkt

## Jaeger (glide)
cd $GOPATH/src/github.com/jaegertracing/jaeger
fossa init
time fossa analyze --output go:./cmd/agent

# Test known good NodeJS projects:
cd $HOME/puppeteer
fossa init
time fossa analyze --output

cd $HOME/pkg
fossa init
time fossa analyze --output

cd $HOME/faker.js
fossa init
time fossa analyze --output

cd $HOME/fastify
fossa init
time fossa analyze --output

cd $HOME/nest
fossa init
time fossa analyze --output npm:package.json

cd $HOME/ohm
fossa init
time fossa analyze --output

cd $HOME/express
fossa init
time fossa analyze --output

## standard has missing peer dependencies.
cd $HOME/standard
fossa init
time fossa analyze --output --option allow-npm-err:true

cd $HOME/sodium-encryption
fossa init
time fossa analyze --output

cd $HOME/request
fossa init
time fossa analyze --output

# Test known good Python projects:
## Since we parse `requirements.txt`, many projects (like Django) which only
## provide `setup.py` are not suitable.
cd $HOME/vibora
fossa init
time fossa analyze --output

# Test known good Ruby projects:
cd $HOME/rails
fossa init
time fossa analyze --output
