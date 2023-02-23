# Strategy command selection

In some strategies, FOSSA CLI uses actual package managers or build tools in order to analyze the dependencies of a project.
FOSSA refers to such strategies as "dynamic analysis strategies".

Some dynamic analysis strategies have multiple command options to use, preferred in a fallback manner.
FOSSA refers to these as "candidate commands".

FOSSA CLI chooses which command in a list of _candidate commands_ to use by running each command
with a set of flags suitable for determining whether the candidate will work for FOSSA CLI.
The particular flags used are up to each strategy; sometimes they simply test whether the command is able to run,
and sometimes they may test that the command is of a particular minimum version or supports some capability.
FOSSA refers to these flags as _evaluation flags_.

If the command, run with the _evaluation flags_, exits with a non-zero exit code FOSSA CLI
determines that the candidate command is unsuitable and moves on to the next candidate.
If no suitable command is found, the overall strategy fails.

If a _candidate command_ was chosen, but then later fails, FOSSA CLI treats
this as an overall strategy failure since that command was determined to be the correct
one to use for the project.

## Concrete example: Maven

For example the "Maven" strategy prefers, in order:

1. The command provided by the user via `FOSSA_MAVEN_CMD`, if present.
2. The local `mvnw` command, if one is found in the project or an ancestor directory.
3. The `mvn` command in `$PATH`.

Suppose FOSSA CLI is analyzing the following project:
```
.
├── mvnw
├── pom.xml
├── readme.md
└── src
   └── main
      └── java
         └── org
            └── apache
               └── maven
                  └── wrapper
                     └── BootstrapMainStarter.java
```

And that FOSSA CLI is being run with the following command:
```
FOSSA_MAVEN_CMD=/usr/local/bin/custom-mvn fossa analyze
```

This results in the following list of candidate commands,
where the earlier in the list the command appears, the higher priority that command has:
```
[ "/usr/local/bin/custom-mvn"
, "./mvnw"
, "mvn"
]
```

FOSSA CLI evaluates these candidates with the following commands in sequence,
stopping after the first one evaluates with exit code zero (signifying no error):
```
; /usr/local/bin/custom-mvn -v # exit code 1
; ./mvnw -v # exit code 0
```

Since `./mvnw` appears suitable, FOSSA CLI chooses that as the command to use
and performs analysis on this Maven project using the `./mvnw` command.
