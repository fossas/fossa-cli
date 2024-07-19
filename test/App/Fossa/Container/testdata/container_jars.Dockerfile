# Use this DockerFile to build an image used in the jars in containers tests.
FROM scratch
# https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.17.1/
COPY jackson-annotations-2.17.1.jar .
# https://repo1.maven.org/maven2/org/apache/commons/commons-email2-jakarta/2.0.0-M1/
COPY commons-email2-jakarta-2.0.0-M1.jar ./inner_directory/
# https://repo1.maven.org/maven2/org/slf4j/slf4j-ext/2.0.0/
COPY slf4j-ext-2.0.0.jar .
# This sample project is in the current directory.
COPY tiny-project .