# This Dockerfile provides all build tools that FOSSA CLI invokes. It's useful
# for developing the FOSSA CLI on machines that may not have all the necessary
# build tools.
FROM fossa/fossa-cli:base

# Workaround for add-apt-repository (see
# https://github.com/oerdnj/deb.sury.org/issues/56).
ENV LC_ALL=C.UTF-8 DEBIAN_FRONTEND=noninteractive 

# Install build tool and fixture requirements.
RUN sudo apt-get install -y \
    # Build tools
    apt-transport-https build-essential libssl-dev software-properties-common \
    # Rails (nokogiri)
    zlib1g-dev liblzma-dev libncurses5-dev libtool autoconf libxml2-dev \
    libmysqlclient-dev postgresql-server-dev-all libsqlite3-dev

# Install JVM version manager.
RUN sudo apt-get install -y zip unzip && \
    curl https://get.sdkman.io | bash

# Install JVM runtimes and build tools.
RUN ["/bin/bash", "-c", "\
    source $HOME/.sdkman/bin/sdkman-init.sh && \
    # Install Java runtime and build tools
    sdk install java && \
    sdk install maven && \
    sdk install gradle && \
    sdk install ant && \
    # Install Scala runtime and build tools
    sdk install scala && \
    sdk install sbt \
    "]
RUN mkdir -p $HOME/.sbt/1.0/plugins && \
    echo 'addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")' > $HOME/.sbt/1.0/plugins/plugins.sbt

# Install Android SDK.
RUN wget https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip -O /tmp/sdk-tools-linux.zip && \
    mkdir -p $HOME/Android/Sdk && \
    unzip /tmp/sdk-tools-linux.zip -d $HOME/Android/Sdk
ENV PATH=$PATH:/home/fossa/Android/Sdk/tools/bin ANDROID_HOME=/home/fossa/Android/Sdk

# Install additional Go build tools.
RUN curl https://glide.sh/get | bash && \
    go get -u -v github.com/tools/godep && \
    go get -u -v github.com/kardianos/govendor && \
    go get -u -v github.com/LK4D4/vndr && \
    go get -u -v github.com/sparrc/gdm

# Install Node.js runtime.
RUN wget https://nodejs.org/dist/v8.12.0/node-v8.12.0-linux-x64.tar.xz -O /tmp/node.tar.xz && \
    sudo tar -xf /tmp/node.tar.xz -C /usr/local --strip-components=1 --no-same-owner && \
    sudo ln -s /usr/local/bin/node /usr/local/bin/nodejs && \
    mkdir $HOME/.npm && \
    npm config set prefix $HOME/.npm
ENV PATH=$PATH:/home/fossa/.npm/bin

# Install Node.js build + debugging tools.
RUN npm i -g bower yarn json

# Install Ruby runtime.
RUN sudo apt-get install -y ruby-full

# Install Ruby build tools.
RUN sudo gem install bundler

# Install Python runtime.
RUN sudo apt-get install -y python python-pip

# Install Cocoapods.
RUN sudo gem install cocoapods -v 1.5.3

# Install PHP runtime.
RUN sudo add-apt-repository -y ppa:ondrej/php && \
    sudo apt-get update && \
    sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    php7.2 php7.2-gd php7.2-curl php7.2-intl php7.2-xml php7.2-mbstring

# Install PHP build tools.
RUN curl https://getcomposer.org/installer | sudo php -- --install-dir=/usr/local/bin --filename=composer && \
    sudo chown -R fossa:fossa $HOME/.composer

# Install NuGet.
RUN sudo apt-key adv --keyserver packages.microsoft.com --recv-keys EB3E94ADBE1229CF && \
    sudo apt-key adv --keyserver packages.microsoft.com --recv-keys 52E16F86FEE04B979B07E28DB02C46DF417A0893 && \
    sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-bionic-prod bionic main" > /etc/apt/sources.list.d/dotnetdev.list' && \
    sudo apt-get update && \
    sudo apt-get install -y dotnet-sdk-2.1
