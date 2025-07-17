FROM alpine:3.14 AS prereq
RUN mkdir /jars
COPY top.jar /jars
COPY middle.jar /jars
COPY deepest.jar /jars
RUN mkdir /sym-link-test
RUN ln -s /jars/deepest.jar /sym-link-test/sym.jar
