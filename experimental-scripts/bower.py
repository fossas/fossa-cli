"""Converts output of `bower list --json` to fossa-deps.json.

The script includes direct and deep dependencies but *does not*
include edges information or filter un-used dependency.

Example:
    bower list --json | python3 bower.py > fossa-deps.json
"""

import sys
import json

sys.setrecursionlimit(10000)


def report(dependency_graph):
    resolved_deps = set()

    if not dependency_graph:
        return resolved_deps

    dependencies = dependency_graph.get("dependencies")
    if not dependencies:
        return resolved_deps

    for _, depValue in dependencies.items():
        meta = depValue.get("pkgMeta", {})
        name, version = meta.get("name"), meta.get("version")

        if (name, version) not in resolved_deps:
            resolved_deps.add((name, version))

        transitive_deps = report(depValue)
        resolved_deps = resolved_deps.union(transitive_deps)

    return resolved_deps

def to_fossa_deps(deps):
    o = {"referenced-dependencies": []}
    for (name, version) in deps:
        o["referenced-dependencies"].append(
            {"name": name, "version": version, "type": "bower"}
        )
    return json.dumps(o)


bower_list_json = json.loads(sys.stdin.read())
resolved_deps = report(bower_list_json)
print(to_fossa_deps(resolved_deps))