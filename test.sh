#!/usr/bin/env bash

# TODO: move these into `*_test.go` files.
set -xe


# Test known good Go projects:
## FOSSA CLI (dep)
echo "Testing fossa-cli"
cd $GOPATH/src/github.com/fossas/fossa-cli
echo '{{range $p, $base := .}}' > test.tmpl
echo '{{range $i, $dep := $base.Build.Dependencies }}{{$dep.Locator}}' >> test.tmpl
echo '{{end}}{{end}}' >> test.tmpl

rm -f *.test-tmp
fossa init
cat .fossa.yml
time fossa analyze --template test.tmpl --output analyze.test-tmp go:./cmd/fossa
cat analyze.test-tmp
time fossa report licenses --output TEST_NOTICE.test-tmp --show-unknown go:./cmd/fossa
cat TEST_NOTICE.test-tmp
time fossa report dependencies --output deps.test-tmp go:./cmd/fossa
cat deps.test-tmp

## Kubernetes (godep)
echo "Testing kubernetes"
cd $GOPATH/src/k8s.io/kubernetes
fossa init
cat .fossa.yml
time fossa analyze --output - --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver | json
time fossa report licenses --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver
time fossa report dependencies --option allow-unresolved-prefix:k8s.io go:./cmd/kube-apiserver | json

## Consul (govendor)
echo "Testing consul"
cd $GOPATH/src/github.com/hashicorp/consul
fossa init
cat .fossa.yml
time fossa analyze --output - --option allow-nested-vendor:true --option allow-deep-vendor:true go:. | json
time fossa report licenses --option allow-nested-vendor:true --option allow-deep-vendor:true go:.
time fossa report dependencies --option allow-nested-vendor:true --option allow-deep-vendor:true go:. | json

## Docker (vndr)
echo "Testing docker"
cd $GOPATH/src/github.com/docker/docker
fossa init
cat .fossa.yml
time fossa analyze --output - --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd | json
time fossa report licenses --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd
time fossa report dependencies --option allow-unresolved-prefix:"github.com/docker archive/tar" go:./cmd/dockerd | json

## Docker CE (vndr)
echo "Testing moby"
cd $GOPATH/src/github.com/docker/docker-ce
fossa init
cat .fossa.yml
time fossa analyze --output - --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd | json
time fossa report licenses --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd
time fossa report dependencies --option allow-unresolved-prefix:"github.com/docker archive/tar" --option allow-external-vendor-prefix:github.com/docker/docker go:./components/engine/cmd/dockerd | json

## InfluxDB (gdm)
echo "Testing influxDB"
cd $GOPATH/src/github.com/influxdata/influxdb
fossa init
cat .fossa.yml
time fossa analyze --output - --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd | json
time fossa report licenses --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd
time fossa report dependencies --option allow-unresolved-prefix:github.com/influxdata go:./cmd/influxd | json

## rkt (glide)
echo "Testing rkt"
cd $GOPATH/src/github.com/rkt/rkt
fossa init
cat .fossa.yml
time fossa analyze --output - go:./rkt | json
time fossa report licenses go:./rkt
time fossa report dependencies go:./rkt | json

## Jaeger (glide)
echo "Testing jaeger"
cd $GOPATH/src/github.com/jaegertracing/jaeger
fossa init
cat .fossa.yml
time fossa analyze --output - go:./cmd/agent | json
time fossa report licenses go:./cmd/agent
time fossa report dependencies go:./cmd/agent | json

# Test known good NodeJS projects:
echo "Testing puppeteer"
cd $HOME/puppeteer
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing pkg"
cd $HOME/pkg
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing faker.js"
cd $HOME/faker.js
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing fastify"
cd $HOME/fastify
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing nest"
cd $HOME/nest
fossa init
cat .fossa.yml
time fossa analyze --output - npm:package.json | json
time fossa report licenses npm:package.json
time fossa report dependencies npm:package.json | json

echo "Testing ohm"
cd $HOME/ohm
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing express"
cd $HOME/express
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

## standard has missing peer dependencies.
echo "Testing standard"
cd $HOME/standard
fossa init
cat .fossa.yml
time fossa analyze --output - --option allow-npm-err:true | json
time fossa report licenses --option allow-npm-err:true 
time fossa report dependencies --option allow-npm-err:true  | json

echo "Testing sodium-encryption"
cd $HOME/sodium-encryption
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing request"
cd $HOME/request
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

# Test known good Python projects:
## Since we parse `requirements.txt`, many projects (like Django) which only
## provide `setup.py` are not suitable.
echo "Testing vibora"
cd $HOME/vibora
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

# Test known good Ruby projects:
echo "Testing rails"
cd $HOME/rails
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing vagrant"
cd $HOME/vagrant
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

# Test known good Gradle projects:
source $HOME/.sdkman/bin/sdkman-init.sh
echo "Testing hibernate-orm"
cd $HOME/hibernate-orm
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

echo "Testing iosched"
cd $HOME/iosched
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

# Test known good Bower projects:
echo "Testing bower-example-project"
cd $HOME/bower-example-project
fossa init
cat .fossa.yml
time fossa analyze --output - | json
time fossa report licenses 
time fossa report dependencies | json

# Test known good Composer projects:
echo "Testing sylius"
cd $HOME/Sylius
fossa init
cat .fossa.yml
time fossa analyze --output - php:. | json
time fossa report licenses php:.
time fossa report dependencies php:. | json

# Test known good Maven projects:
echo "Testing hadoop"
cd $HOME/hadoop
fossa init
cat .fossa.yml
time fossa analyze --output - mvn:org.apache.hadoop:hadoop-yarn-client | json
time fossa report licenses mvn:org.apache.hadoop:hadoop-yarn-client
time fossa report dependencies mvn:org.apache.hadoop:hadoop-yarn-client | json

# Test known good NuGet projects:
echo "Testing bitwarden"
cd $HOME/core
fossa init
cat .fossa.yml
time fossa analyze --output - nuget:src/Api/Api.csproj | json
time fossa report licenses nuget:src/Api/Api.csproj
time fossa report dependencies nuget:src/Api/Api.csproj | json
