# Glide

Glide is very commonly encountered as a buildtool for older Go projects.
Though deprecated, it's prevalent enough among existing projects that it
warrants support in fossa-cli.

## Project Discovery

Find all files named `glide.lock`

## Analysis

`glide.lock` is a yaml-formatted file containing pinned package versions:

```yaml
hash: 12345
updated: 2018-10-12T14:37:49.968644-07:00
imports:
- name: github.com/pkg/one
  version: 100
  subpackages:
  - compute
- name: github.com/pkg/three/v3
  version: 300
  repo: fossas/privatefork
```
