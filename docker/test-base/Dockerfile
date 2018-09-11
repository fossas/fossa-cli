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

## gocode (none)
RUN go get -u -v github.com/nsf/gocode

## Caddy (gvt)
RUN go get -u -v github.com/mholt/caddy/caddy && \
    go get github.com/caddyserver/builds && \
    cd $GOPATH/src/github.com/mholt/caddy/caddy && \
    go run build.go

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
RUN git clone --depth=1 https://github.com/ansible/ansible
RUN git clone --depth=1 https://github.com/getsentry/sentry && \
    cd sentry && \
    pip install -r requirements-base.txt && \
    pip install -r requirements-dev.txt
RUN git clone --depth=1 https://github.com/fabric/fabric
RUN git clone --depth=1 https://github.com/boto/boto
RUN git clone --depth=1 https://github.com/samuelclay/NewsBlur
RUN git clone --depth=1 https://github.com/kennethreitz/legit
RUN git clone --depth=1 https://github.com/sshuttle/sshuttle

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
## Other testing candidates:
## - Netflix OSS projects
## - ReactiveX/RxJava
## - Spring
## - Mockito
## - TryGhost/Ghost-Android
RUN git clone --depth=1 https://github.com/hibernate/hibernate-orm && \
    cd hibernate-orm && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && ./gradlew"

RUN git clone --depth=1 https://github.com/google/iosched && \
    cd iosched && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && \
    yes | sdkmanager --licenses && \
    ./gradlew"

# Known good Bower projects:
RUN git clone --depth=1 https://github.com/fossas/bower-example-project && \
    cd bower-example-project && \
    bower install --production

# Known good PHP projects:
RUN git clone --depth=1 https://github.com/Sylius/Sylius && \
    cd Sylius && \
    composer install --prefer-dist --no-dev --no-plugins --no-scripts

# Known good Maven projects:
## Other testing candidates:
## - clojure/clojure
## - sendgrid/sendgrid-java
## - google/truth
## - google/auto
RUN git clone --depth=1 https://github.com/apache/hadoop && \
    cd hadoop && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && mvn dependency:resolve"

# Known good NuGet projects:
RUN git clone --depth=1 https://github.com/bitwarden/core && \
    cd core/src/Api && \
    dotnet restore
RUN git clone --depth=1 https://github.com/cloudRoutine/FsBuild && \
    cd FsBuild/src/FsBuild
RUN git clone --depth=1 https://github.com/enricosada/dotnet-proj-info && \
    cd dotnet-proj-info/src/Dotnet.ProjInfo && \
    dotnet restore
RUN git clone --depth=1 https://github.com/LarsSkovslund/NuGet.Package.Builder && \
    cd NuGet.Package.Builder/Source/NuGet.Package.Builder && \
    dotnet restore
RUN git clone --depth=1 https://github.com/OctopusDeploy/CSProjToXProj && \
    cd CSProjToXProj/src/CSProjToXProj

# Known good Scala projects:
## Other testing candidates:
## - apache/spark
## - linkerd/linkerd
## - graphcool/prisma
## - twitter/finagle
RUN git clone --depth=1 https://github.com/graphcool/prisma && \
    cd prisma && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && sbt dependencyTree"

# Known good Cocoapods projects:
RUN git clone --depth=1 https://github.com/HeroTransitions/Hero
RUN git clone --depth=1 https://github.com/lhc70000/iina
RUN git clone --depth=1 https://github.com/onevcat/Kingfisher
RUN git clone --depth=1 https://github.com/MacDownApp/macdown

# Known good Ant projects:
## Other testing candidates:
## - kissyui/kissy
## - arduino/Arduino
## - Cyberqat/Red-Dwarf
## - mproch/apache-ode
## - mediaslave/phrails
## - noerd/Multiple-File-Upload-Package
## - mikaelkindborg/droidscript
RUN git clone --depth=1 https://github.com/fossas/ant-example-project

# Raw projects:
RUN git clone --depth=1 https://github.com/jelaas/bifrost-framework
