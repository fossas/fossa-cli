# DPKG

The Debian Package Manager, often additionally used debian based distros such as `ubuntu`.

> This analysis is only ran for Container Scanning.

## Discovery

Find file named: `status` or `status.d` in `**/var/lib/dpkg/*`

## Analysis

Parse and analyze, reporting only package name, package version, and architecture.

```text
Package: bash
Essential: yes
Status: install ok installed
Priority: required
Section: shells
Installed-Size: 6512
Maintainer: Matthias Klose <doko@debian.org>
Architecture: arm64
Multi-Arch: foreign
Version: 5.1-2+deb11u1
Replaces: bash-completion (<< 20060301-0), bash-doc (<= 2.05-1)
Depends: base-files (>= 2.1.12), debianutils (>= 2.15)
Pre-Depends: libc6 (>= 2.25), libtinfo6 (>= 6)
Recommends: bash-completion (>= 20060301-0)
Suggests: bash-doc
Conflicts: bash-completion (<< 20060301-0)
Conffiles:
 /etc/bash.bashrc 89269e1298235f1b12b4c16e4065ad0d
 /etc/skel/.bash_logout 22bfb8c1dd94b5f3813a2b25da67463f
 /etc/skel/.bashrc ee35a240758f374832e809ae0ea4883a
 /etc/skel/.profile f4e81ade7d6f9fb342541152d08e7a97
Description: GNU Bourne Again SHell
 Bash is an sh-compatible command language interpreter that executes
 commands read from the standard input or from a file.  Bash also
 incorporates useful features from the Korn and C shells (ksh and csh).
 .
 Bash is ultimately intended to be a conformant implementation of the
 IEEE POSIX Shell and Tools specification (IEEE Working Group 1003.2).
 .
 The Programmable Completion Code, by Ian Macdonald, is now found in
 the bash-completion package.
Homepage: http://tiswww.case.edu/php/chet/bash/bashtop.html
```

For the above, the dependency `bash` at version `5.1-2+deb11u1` with architecture `arm64` is discovered.

## FAQ

### How do I *only perform analysis* for DPKG?

Explicitly specify an analysis target in `.fossa.yml` file. The example below excludes all other analysis targets:

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: dpkgdb
```
