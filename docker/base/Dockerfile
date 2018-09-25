# This Dockerfile is the base image for developing the FOSSA CLI. It sets up the
# non-root FOSSA user account, but contains no build tools or other extras.
FROM buildpack-deps:18.04-scm

# Housekeeping + CircleCI requirements + build utilities.
RUN apt-get update && \
    apt-get install -y \
      # Useful scripting + debugging utilities:
      sudo tmux tree vim curl \
      # Required by CircleCI:
      git ssh tar gzip ca-certificates

# Add and drop down to non-root user.
RUN useradd fossa && \
    mkdir /home/fossa && \
    chown fossa /home/fossa && \
    echo "fossa ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/fossa && \
    chmod 0440 /etc/sudoers.d/fossa && \
    usermod -s /bin/bash fossa
USER fossa
WORKDIR /home/fossa

# Install Go compiler and toolchain.
RUN wget https://dl.google.com/go/go1.11.linux-amd64.tar.gz -O /tmp/go.tar.gz && \
    sudo tar -xf /tmp/go.tar.gz -C /usr/local 
ENV GOPATH=/home/fossa/go PATH=$PATH:/usr/local/go/bin:/home/fossa/go/bin

# Install tools for developing FOSSA CLI.
RUN mkdir -p $GOPATH/bin && \
    curl https://raw.githubusercontent.com/golang/dep/master/install.sh | sh && \
    go get -u -v github.com/go-bindata/go-bindata/... && \
    go get -u -v github.com/cheekybits/genny && \
    go get -u -v github.com/jstemmer/go-junit-report && \
    go get -u -v github.com/mattn/goveralls
