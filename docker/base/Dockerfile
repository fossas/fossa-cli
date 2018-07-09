# This Dockerfile provides all build tools that FOSSA CLI invokes. It's useful
# for trying out FOSSA CLI on machines that may not have all the necessary
# build tools.
FROM ubuntu:18.04

# Workaround for add-apt-repository (see https://github.com/oerdnj/deb.sury.org/issues/56)
ENV LC_ALL=C.UTF-8 DEBIAN_FRONTEND=noninteractive 

# Housekeeping + CircleCI requirements
RUN apt-get update && \
    apt-get install -y \
      # Useful scripting + debugging utilities
      sudo tmux tree vim \
      # Required by CircleCI
      git ssh tar gzip ca-certificates \
      # Used by various build tools
      curl apt-transport-https build-essential libssl-dev software-properties-common

# Add extra PPA repositories
RUN add-apt-repository -y ppa:ondrej/php && \
    apt-get update

# Add and drop down to non-root user
RUN useradd fossa && \
    mkdir /home/fossa && \
    chown fossa /home/fossa && \
    echo "fossa ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/fossa && \
    chmod 0440 /etc/sudoers.d/fossa && \
    usermod -s /bin/bash fossa
USER fossa
WORKDIR /home/fossa

# Install JVM version manager
RUN sudo apt-get install -y zip unzip && \
    curl https://get.sdkman.io | bash

# Install JVM runtimes and build tools
RUN ["/bin/bash", "-c", "\
    source $HOME/.sdkman/bin/sdkman-init.sh && \
    # Install Java runtime and build tools
    sdk install java && \
    sdk install maven && \
    sdk install gradle && \
    # Install Scala runtime and build tools
    sdk install scala && \
    sdk install sbt \
    "]
RUN mkdir -p $HOME/.sbt/1.0/plugins && \
    echo 'addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")' > $HOME/.sbt/1.0/plugins/plugins.sbt

# Install Android SDK
RUN wget https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip -O /tmp/sdk-tools-linux.zip && \
    mkdir -p $HOME/Android/Sdk && \
    unzip /tmp/sdk-tools-linux.zip -d $HOME/Android/Sdk
ENV PATH=$PATH:/home/fossa/Android/Sdk/tools/bin ANDROID_HOME=/home/fossa/Android/Sdk

# Install Go compiler
RUN wget https://dl.google.com/go/go1.10.3.linux-amd64.tar.gz -O /tmp/go.tar.gz && \
    sudo tar -xf /tmp/go.tar.gz -C /usr/local 
ENV GOPATH=/home/fossa/go PATH=$PATH:/usr/local/go/bin:/home/fossa/go/bin

# Install Go build tools
RUN mkdir -p $GOPATH/bin && \
    # Install dep
    wget https://github.com/golang/dep/releases/download/v0.4.1/dep-linux-amd64 -O $GOPATH/bin/dep && \
    chmod +x $GOPATH/bin/dep && \
    # Install glide
    curl https://glide.sh/get | bash && \
    # Install godep
    go get github.com/tools/godep && \
    # Install govendor
    go get -u github.com/kardianos/govendor && \
    # Install vndr
    go get github.com/LK4D4/vndr && \
    # Install gdm
    go get github.com/sparrc/gdm

# Install Node.js runtime
RUN wget https://nodejs.org/dist/v8.9.4/node-v8.9.4-linux-x64.tar.xz -O /tmp/node.tar.xz && \
    sudo tar -xf /tmp/node.tar.xz -C /usr/local --strip-components=1 --no-same-owner && \
    sudo ln -s /usr/local/bin/node /usr/local/bin/nodejs && \
    mkdir $HOME/.npm && \
    npm config set prefix $HOME/.npm
ENV PATH=$PATH:/home/fossa/.npm/bin

# Install Node.js build + debugging tools
RUN npm i -g bower yarn json

# Install Ruby runtime
RUN sudo apt-get install -y ruby-full

# Install Ruby build tools
RUN sudo gem install bundler

# Install Python runtime
RUN sudo apt-get install -y python python-pip

# Install Cocoapods
RUN sudo gem install cocoapods -v 0.39.0

# Install PHP runtime
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    php7.2 php7.2-gd php7.2-curl php7.2-intl php7.2-xml php7.2-mbstring

# Install PHP build tools
RUN curl https://getcomposer.org/installer | sudo php -- --install-dir=/usr/local/bin --filename=composer && \
    sudo chown -R fossa:fossa $HOME/.composer

# Install NuGet
RUN sudo apt-key adv --keyserver packages.microsoft.com --recv-keys EB3E94ADBE1229CF && \
    sudo apt-key adv --keyserver packages.microsoft.com --recv-keys 52E16F86FEE04B979B07E28DB02C46DF417A0893 && \
    sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-bionic-prod bionic main" > /etc/apt/sources.list.d/dotnetdev.list' && \
    sudo apt-get update && \
    sudo apt-get install -y dotnet-sdk-2.1

# Install `go-bindata` and `goreleaser`
RUN go get -u github.com/go-bindata/go-bindata/... && \
    go get -u github.com/goreleaser/goreleaser/...
