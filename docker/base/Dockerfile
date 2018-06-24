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
      sudo tmux \
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
    chmod 0440 /etc/sudoers.d/fossa
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

# Install Android SDK
RUN wget https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip -O /tmp/sdk-tools-linux.zip && \
    sudo unzip /tmp/sdk-tools-linux.zip -d /opt/android-sdk && \
    sudo chmod -R 775 /opt/android-sdk
ENV PATH=$PATH:/opt/android-sdk/tools/bin ANDROID_HOME=/opt/android-sdk

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
RUN sudo apt-get install -y python

# Install Cocoapods
RUN sudo gem install cocoapods -v 0.39.0

# Install PHP runtime
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y php7.2 php7.2-gd php7.2-curl php7.2-intl

# Install PHP build tools
RUN curl https://getcomposer.org/installer | sudo php -- --install-dir=/usr/local/bin --filename=composer && \
    sudo chown -R fossa:fossa $HOME/.composer

# Install `go-bindata` and `goreleaser`
RUN go get -u github.com/go-bindata/go-bindata/... && \
    go get -u github.com/goreleaser/goreleaser/...
