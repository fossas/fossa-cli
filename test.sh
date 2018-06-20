#!/usr/bin/env bash

set -xe

# Test known good Go projects:
## FOSSA CLI (dep)
cd $GOPATH/src/github.com/fossas/fossa-cli
echo '{{range $p, $base := .}}' > test.tmpl
echo '{{range $i, $dep := $base.Build.Dependencies }}{{$dep.Locator}}' >> test.tmpl
echo '{{end}}{{end}}' >> test.tmpl

rm -f *.test-tmp
fossa init
time fossa analyze --template test.tmpl --output analyze.test-tmp go:./cmd/fossa
cat analyze.test-tmp
time fossa report licenses --output TEST_NOTICE.test-tmp --show-unknown go:./cmd/fossa
cat TEST_NOTICE.test-tmp
time fossa report dependencies --output deps.test-tmp go:./cmd/fossa
cat deps.test-tmp

## Kubernetes (godep)
cd $GOPATH/src/k8s.io/kubernetes
fossa init
time fossa analyze --option allow-unresolved-prefix:k8s.io --output - go:./cmd/kube-apiserver
time fossa report licenses --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver
time fossa report dependencies --option allow-unresolved-prefix:k8s.io --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver

## Consul (govendor)
cd $GOPATH/src/github.com/hashicorp/consul
fossa init
time fossa analyze --output - --option allow-nested-vendor:true --option allow-deep-vendor:true go:.
time fossa report licenses --option allow-nested-vendor:true --option allow-deep-vendor:true go:.
time fossa report dependencies --option allow-nested-vendor:true --option allow-deep-vendor:true  go:.

## Docker (vndr)
cd $GOPATH/src/github.com/docker/docker
fossa init
time fossa analyze --output - --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd
time fossa report licenses --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd
time fossa report dependencies --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd

## Docker CE (vndr)
cd $GOPATH/src/github.com/docker/docker-ce
fossa init
time fossa analyze --output - --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd
time fossa report licenses --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd
time fossa report dependencies --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd

## InfluxDB (gdm)
cd $GOPATH/src/github.com/influxdata/influxdb
fossa init
time fossa analyze --output - --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd
time fossa report licenses --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd
time fossa report dependencies --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd

## rkt (glide)
cd $GOPATH/src/github.com/rkt/rkt
fossa init
time fossa analyze --output - go:./rkt
time fossa report licenses go:./rkt
time fossa report dependencies go:./rkt
