# APK

The Alpine Package Manager (apk).

> This analysis is only ran for Container Scanning.

## Discovery

Find file named `installed` under `**/apk/**`.

## Analysis

Parse and analyze, reporting package name, package version, and architecture.

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

For the above, the dependency `musl` at version `1.2.2-r7` with architecture `x86_64` is discovered.

## FAQ

### How do I *only perform analysis* for APK?

Explicitly specify an analysis target in `.fossa.yml` file. The example below excludes all other analysis targets:

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: apkdb
```
