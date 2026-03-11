# Using this test suite

**If you make any changes to this suite ensure that you manually create the tar file. The test suite does not automatically create the image for testing.**

This test suite tests the CLI's capability to recursively unpack archived files in a container image for the purpose of JAR analysis. 

## Building the tar file

Running the following commands in order will create `nested_jars.tar` for the purpose of testing.

```
./make-nested-jars
docker build -t nested_jars .
docker save nested_jars > ../nested_jars.tar
```

After this is run, ensure that you commit `nested_jars.tar` and any other files in this directory that you used to build it.

The existing assertions in the test files will likely need to be updated as well.

## Notes on the tar file

Running the make command results in a container with:
- jar called top.jar in it
- top.jar contains middle.jar
- middle.jar contains deepest.jar
- sym.jar symlinks to deepest.jar (for the sake of testing symlinks between layers)

These are not actual jar files. They're just zip files with a single text file in them.