#!/usr/bin/env bash

# TODO: move these into `*_test.go` files.

set -e

# Test known good Go projects:
## FOSSA CLI (dep)
echo "Testing fossa-cli"
cd $GOPATH/src/github.com/fossas/fossa-cli
fossa init
time fossa analyze --output go:./cmd/fossa

## Kubernetes (godep)
echo "Testing kubernetes"
cd $GOPATH/src/k8s.io/kubernetes
fossa init
time fossa analyze --output --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver

## Consul (govendor)
echo "Testing consul"
cd $GOPATH/src/github.com/hashicorp/consul
fossa init
time fossa analyze --output --option allow-nested-vendor:true --option allow-deep-vendor:true go:.

## Docker (vndr)
echo "Testing docker"
cd $GOPATH/src/github.com/docker/docker
fossa init
time fossa analyze --output --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd

## Docker CE (vndr)
echo "Testing moby"
cd $GOPATH/src/github.com/docker/docker-ce
fossa init
time fossa analyze --output --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd

## InfluxDB (gdm)
echo "Testing influxDB"
cd $GOPATH/src/github.com/influxdata/influxdb
fossa init
time fossa analyze --output --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd

## rkt (glide)
echo "Testing rkt"
cd $GOPATH/src/github.com/rkt/rkt
fossa init
time fossa analyze --output go:./rkt

## Jaeger (glide)
echo "Testing jaeger"
cd $GOPATH/src/github.com/jaegertracing/jaeger
fossa init
time fossa analyze --output go:./cmd/agent

# Test known good NodeJS projects:
echo "Testing puppeteer"
cd $HOME/puppeteer
fossa init
time fossa analyze --output

echo "Testing pkg"
cd $HOME/pkg
fossa init
time fossa analyze --output

echo "Testing faker.js"
cd $HOME/faker.js
fossa init
time fossa analyze --output

echo "Testing fastify"
cd $HOME/fastify
fossa init
time fossa analyze --output

echo "Testing nest"
cd $HOME/nest
fossa init
time fossa analyze --output npm:package.json

echo "Testing ohm"
cd $HOME/ohm
fossa init
time fossa analyze --output

echo "Testing express"
cd $HOME/express
fossa init
time fossa analyze --output

## standard has missing peer dependencies.
echo "Testing standard"
cd $HOME/standard
fossa init
time fossa analyze --output --option allow-npm-err:true

echo "Testing sodium-encryption"
cd $HOME/sodium-encryption
fossa init
time fossa analyze --output

echo "Testing request"
cd $HOME/request
fossa init
time fossa analyze --output

# Test known good Python projects:
## Since we parse `requirements.txt`, many projects (like Django) which only
## provide `setup.py` are not suitable.
echo "Testing vibora"
cd $HOME/vibora
fossa init
time fossa analyze --output

# Test known good Ruby projects:
echo "Testing rails"
cd $HOME/rails
fossa init
time fossa analyze --output

echo "Testing vagrant"
cd $HOME/vagrant
fossa init
time fossa analyze --output

# Test known good Gradle projects:
echo "Testing hibernate-orm"
source $HOME/.sdkman/bin/sdkman-init.sh
cd $HOME/hibernate-orm
fossa init
time fossa analyze --output --debug

# Test known good Bower projects:
echo "Testing bower-example-project"
cd $HOME/bower-example-project
fossa init
time fossa analyze --output
