# This Dockerfile contains all the test fixtures for integration testing.
FROM quay.io/fossa/fossa-cli-base

# Known good Go projects (some are `git clone`d because `go get` causes
# compilation errors):
## Kubernetes (godep)
RUN mkdir -p $GOPATH/src/k8s.io && \
    cd $GOPATH/src/k8s.io && \
    git clone --depth=1 https://github.com/kubernetes/kubernetes && \
    cd kubernetes && \
    make ./cmd/kube-apiserver

## Docker, Docker CE (vndr)
RUN mkdir -p $GOPATH/src/github.com/docker && \
    cd $GOPATH/src/github.com/docker && \
    git clone --depth=1 https://github.com/docker/docker && \
    git clone --depth=1 https://github.com/docker/docker-ce

## InfluxDB (gdm)
## This is an older version of InfluxDB that still used GDM -- the current
## revision has migrated to Dep.
RUN mkdir -p $GOPATH/src/github.com/influxdata && \
    cd $GOPATH/src/github.com/influxdata && \
    git clone https://github.com/influxdata/influxdb && \
    cd influxdb && \
    git reset --hard d606fcf19a5bb81566aac58cd223638a8720be2c && \
    go get github.com/influxdata/influxdb/...

## Consul (nested govendor)
RUN go get -u -v github.com/hashicorp/consul

## rkt (glide)
RUN go get -u -v github.com/rkt/rkt/rkt

## Jaeger (glide)
RUN go get -u -v github.com/jaegertracing/jaeger && \
    cd $GOPATH/src/github.com/jaegertracing/jaeger && \
    glide install

# Known good NodeJS projects:
RUN git clone --depth=1 https://github.com/GoogleChrome/puppeteer && \
    cd puppeteer && \
    PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=1 npm install --production
RUN git clone --depth=1 https://github.com/zeit/pkg && \
    cd pkg && \
    npm install --production
RUN git clone --depth=1 https://github.com/Marak/faker.js && \
    cd faker.js && \
    npm install --production
RUN git clone --depth=1 https://github.com/fastify/fastify && \
    cd fastify && \
    npm install --production
RUN git clone --depth=1 https://github.com/nestjs/nest && \
    cd nest && \
    npm install --production
RUN git clone --depth=1 https://github.com/harc/ohm && \
    cd ohm && \
    npm install --production
RUN git clone --depth=1 https://github.com/expressjs/express && \
    cd express && \
    npm install --production
RUN git clone --depth=1 https://github.com/standard/standard && \
    cd standard && \
    npm install --production
RUN git clone --depth=1 https://github.com/mafintosh/sodium-encryption && \
    cd sodium-encryption && \
    npm install --production
RUN git clone --depth=1 https://github.com/request/request && \
    cd request && \
    npm install --production

# Known good Python projects:
RUN git clone --depth=1 https://github.com/vibora-io/vibora

# Known good Ruby projects:
## The apt dependencies are for native extensions.
RUN sudo apt-get install -y \
    zlib1g-dev liblzma-dev libncurses5-dev libtool autoconf libxml2-dev \
    libmysqlclient-dev postgresql-server-dev-all libsqlite3-dev && \
    git clone --depth=1 https://github.com/rails/rails && \
    cd rails && \
    bundle install --deployment --without doc job cable storage ujs test db

RUN git clone --depth=1 https://github.com/hashicorp/vagrant && \
    cd vagrant && \
    bundle install

# Known good Gradle projects:
RUN git clone --depth=1 https://github.com/hibernate/hibernate-orm && \
    cd hibernate-orm && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && ./gradlew"

# Known good Bower projects:
RUN git clone --depth=1 https://github.com/fossas/bower-example-project && \
    cd bower-example-project && \
    bower install --production
