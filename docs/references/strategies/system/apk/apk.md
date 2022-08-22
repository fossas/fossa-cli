# Apk

This is alpine package manager (apk), often used in alpine centric distros.

> This analysis is only ran for Container Scanning.

## Discovery

Find file named: `installed` under `**/apk/**`.

## Analysis

Parse and analyze, reporting only - package name, package version, and architecture.

```text
C:Q1Deb0jNytkrjPW4N/eKLZ43BwOlw=
P:musl
V:1.2.2-r7
A:x86_64
S:383152
I:622592
T:the musl c library (libc) implementation
U:https://musl.libc.org/
L:MIT
o:musl
m:Timo Teräs <timo.teras@iki.fi>
t:1632431095
c:bf5bbfdbf780092f387b7abe401fbfceda90c84d
p:so:libc.musl-x86_64.so.1=1
F:lib
R:ld-musl-x86_64.so.1
a:0:0:755
Z:Q12adwqQOjo9dFl+VJD2Ecd901vhE=
R:libc.musl-x86_64.so.1
a:0:0:777
Z:Q17yJ3JFNypA4mxhJJr0ou6CzsJVI=
```

For above, we will discover dependency: `musl` of version `1.2.2-r7` with architecture of `x86_64`.

## FAQ

### How do I *only perform analysis* for Apkdb?

You can explicitly specify an analysis target in `.fossa.yml` file. The example below will exclude all analysis targets except for the apkdb. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: apkdb
```
