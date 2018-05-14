#!/bin/bash

# Android projects
# TryGhost/Ghost-Android
git clone https://github.com/google/iosched android
cd android
git checkout 6a33a887ab1de307922bc33b1ee49c14e6124d92
cd ..

# Gradle projects
# Netflix OSS projects
# Hibernate
# Spring
# Mockito
git clone https://github.com/ReactiveX/RxJava gradle
cd gradle
git checkout f671b57b11845af04cc110792550ecc4f5464712
cd ..

# Maven projects
# clojure/clojure
# sendgrid/sendgrid-java
# google/truth
# google/auto
git clone https://github.com/apache/hadoop maven
cd maven
git checkout e65ff1c8be48ef4f04ed96f96ac4caef4974944d
cd ..

# Scala projects
# apache/spark
# linkerd/linkerd
# graphcool/prisma
# twitter/finagle
git clone https://github.com/graphcool/prisma sbt
cd sbt
git checkout 90058a94b096b3c0dece8df332288755632981de
cd ..

# Node projects
## NPM
## Yarn
## Bower

# Ruby projects

# Python projects

# Go projects

# PHP projects

# NuGet projects
git clone https://github.com/bitwarden/core nuget
cd nuget
git checkout fa198213af4af3e2075559a7d459dc8f78b131a2
cd ..