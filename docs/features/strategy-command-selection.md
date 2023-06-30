# Strategy command selection

In some strategies, FOSSA CLI uses actual package managers or build tools in order to analyze the dependencies of a project.
FOSSA refers to such strategies as "dynamic analysis strategies".

Some dynamic analysis strategies have multiple command options to use, preferred in a fallback manner.
FOSSA refers to these as "candidate commands".

FOSSA CLI builds candidate commands with a matrix of the following:

- Command names (also called "binaries").
- The scan directory and its ancestors.
- PATH locations (`$PATH` in macOS and Linux; `%PATH%` in Windows).
- Extensions the current system can execute (empty list in macOS and Linux; `%PATHEXT%` in Windows).

FOSSA CLI chooses which command in a list of _candidate commands_ to use by checking for its existence on disk.
If all directories, commands, and extensions are exhausted without finding a match, FOSSA analysis fails.

## Concrete example: Maven

_Note: The command for Maven can be overriden with `FOSSA_MAVEN_CMD` as well._
_The below is what happens when this override is not provided._

The "Maven" strategy prefers, in order:

1. The `mvnw` command.
2. The `mvn` command.

FOSSA CLI searches for the first found in the list of desired _commands_,
with the first found of system-supplied _execution extensions_,
in the first of discovered _directories_.
The search is conducted in the order of `directories -> commands -> extensions`.

FOSSA CLI builds the list of _directories_ in two phases:
First, it considers the scan directory and all its parent directories, up to the root of the drive.
After that, it considers all entries in the system `PATH` environment variable.

In Unix-based systems, _execution extensions_ are always an empty list.<br>
In Windows-based systems, _execution extensions_ are determined using the system's
`%PATHEXT%` environment variable. This variable allows Windows to tell programs
the list of file extensions it considers "executable programs".

Finally, the list of _commands_ depends on the strategy being executed.
In this example (the Maven strategy), this list is comprised of `mvnw` followed by `mvn`.

FOSSA CLI then generates a set of paths at which the command may exist
based on the information collected up to this point.
It then checks each location in order to see if a file exists at that location;
if it does, FOSSA CLI uses that as the command to run.

Below are two examples, one for Windows and one for macOS/Linux,
demonstrating how these paths are built and the order in which they are checked.

**These examples are simplified in an attempt to keep this document from becoming overly long.**
**Note that in a real system, there are many more entries in `PATH` and `PATHEXT`.**

**Windows**

```
Binaries: [ "mvnw", "mvn" ]
PATH    : "C:\System32"
PATHEXT : ".bat;.exe"
SCANDIR : "C:\Users\me\projects\example"
```

Searches:

```
C:\Users\me\projects\example\mvnw.exe
C:\Users\me\projects\example\mvnw.bat
C:\Users\me\projects\example\mvn.exe
C:\Users\me\projects\example\mvn.bat
C:\Users\me\projects\mvnw.exe
C:\Users\me\projects\mvnw.bat
C:\Users\me\projects\mvn.exe
C:\Users\me\projects\mvn.bat
C:\Users\me\mvnw.exe
C:\Users\me\mvnw.bat
C:\Users\me\mvn.exe
C:\Users\me\mvn.bat
C:\Users\mvnw.exe
C:\Users\mvnw.bat
C:\Users\mvn.exe
C:\Users\mvn.bat
C:\mvnw.exe
C:\mvnw.bat
C:\mvn.exe
C:\mvn.bat
C:\System32\mvnw.exe
C:\System32\mvnw.bat
C:\System32\mvn.exe
C:\System32\mvn.bat
```

**macOS, Linux**

```
Binaries: [ "mvnw", "mvn" ]
PATH    : "/usr/local/bin"
SCANDIR : "/home/me/projects/example"
```

Searches:

```
/home/me/projects/example/mvnw
/home/me/projects/example/mvn
/home/me/projects/mvnw
/home/me/projects/mvn
/home/me/mvnw
/home/me/mvn
/home/mvnw
/home/mvn
/mvnw
/mvn
/usr/local/bin/mvnw
/usr/local/bin/mvn
```
