# Making test/Container/testdata/nested-jar.tar
./make-nested-jar
docker build -t jar-tester .
docker save jar-tester > ../nested_jars.tar

You will end up with a container with a jar called top.jar in it
top.jar contains middle.jar
middle.jar contains deepest.jar

These are not actual jar files. They're just zip files with a single text file in them.
