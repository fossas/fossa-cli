"""FOSSA CLI integration script for conan package manager.

Generates fossa-deps.yml with referenced-dependencies (Git), remote-dependencies
(archive tarball), and custom-dependencies from a Conan project.

Requires:
    - python3 to run this script
    - Conan package manager v2

Usage:
    Option A — run conan automatically (default):
        python3 make_fossa_deps_conan.py
        python3 make_fossa_deps_conan.py -s compiler=gcc   (extra conan args)

    Option B — read a pre-generated graph JSON:
        conan graph info . -f json > conan-graph-info.json
        python3 make_fossa_deps_conan.py conan-graph-info.json

    Then run: fossa analyze

If you run into any issues with integration, please reach out to us at:
    - https://support.fossa.com/hc/en-us

Docs:
    - https://github.com/fossas/fossa-cli
    - https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-deps.md
    - https://github.com/fossas/fossa-cli/blob/master/docs/walkthroughs/conan.md
"""

import sys
import json
import subprocess
import logging
import os
import re
from datetime import datetime

logging.basicConfig(
    level=logging.DEBUG,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[logging.StreamHandler()]
)


def extract_git_url_from_sources(node):
    """Extract Git URL from conandata sources field.

    Sometimes the homepage points to an index page, but the actual
    source URLs include GitHub releases. Check those first.

    Returns tuple: (git_url, is_github) or (None, False)
    """
    conandata = node.get("conandata", {})
    sources = conandata.get("sources", {})
    version = node.get("version")

    if version and version in sources:
        source_info = sources[version]
        urls = source_info.get("url", [])
        if isinstance(urls, str):
            urls = [urls]

        git_hosts = {
            "github.com": "github.com",
            "gitlab.com": "gitlab.com",
            "bitbucket.org": "bitbucket.org",
            "git.sr.ht": "git.sr.ht",
            "codeberg.org": "codeberg.org",
        }

        for url in urls:
            for host_pattern, host_name in git_hosts.items():
                if host_pattern in url:
                    clean_url = url.replace("https://", "").replace("http://", "")
                    if host_name in clean_url:
                        parts = clean_url.split(host_name + "/")
                        if len(parts) > 1:
                            path_parts = parts[1].split("/")
                            if len(path_parts) >= 2:
                                git_url = f"{host_name}/{path_parts[0]}/{path_parts[1]}"
                                return git_url, (host_name == "github.com")

    return None, False


def extract_git_url(node):
    """Extract Git URL from node's homepage or url field.

    Returns tuple: (git_url, is_github)
    """
    homepage = node.get("homepage") or node.get("url") or ""

    git_hosts = {
        "github.com": "github.com",
        "gitlab.com": "gitlab.com",
        "bitbucket.org": "bitbucket.org",
        "git.sr.ht": "git.sr.ht",
        "codeberg.org": "codeberg.org",
    }

    for host_pattern, host_name in git_hosts.items():
        if host_pattern in homepage:
            url = homepage.replace("https://", "").replace("http://", "")
            url = url.replace(".git", "").rstrip("/")
            if host_name in url:
                parts = url.split(host_name + "/")
                if len(parts) > 1:
                    path_parts = parts[1].split("/")
                    if len(path_parts) >= 2:
                        clean_url = f"{host_name}/{path_parts[0]}/{path_parts[1]}"
                        return clean_url, (host_name == "github.com")

    return None, False


def extract_archive_url(node):
    """Extract download URL from conandata sources field.

    Some packages (especially GNU packages) don't have Git repos,
    only tarball URLs. Returns the first valid URL found.
    """
    conandata = node.get("conandata", {})
    sources = conandata.get("sources", {})
    version = node.get("version")

    if version and version in sources:
        source_info = sources[version]
        url = source_info.get("url")
        if isinstance(url, list) and len(url) > 0:
            return url[0]
        elif isinstance(url, str):
            return url

    return None


def parse_version(node):
    """Extract clean version string from node."""
    return node.get("version", "").lstrip("v")


def extract_git_tag_from_source_url(node):
    """Extract the actual git tag from a conandata source URL.

    Some packages use non-standard tag formats (e.g. curl-8_17_0, openssl-3.6.0)
    that differ from the conan version string. The tag is embedded in the
    /releases/download/{tag}/ or /archive/{tag}.tar.gz path segment.

    Returns the tag string, or None if it cannot be determined.
    """
    conandata = node.get("conandata", {})
    sources = conandata.get("sources", {})
    version = node.get("version")

    if not (version and version in sources):
        return None

    source_info = sources[version]
    urls = source_info.get("url", [])
    if isinstance(urls, str):
        urls = [urls]

    for url in urls:
        m = re.search(r'/releases/download/([^/]+)/', url)
        if m:
            return m.group(1)
        m = re.search(r'/archive/([^/]+)\.(?:tar\.gz|tar\.xz|tar\.bz2|zip)$', url)
        if m:
            return m.group(1).lstrip("v")

    return None


def generate_fossa_deps(nodes):
    """Process graph nodes and write fossa-deps.yml."""
    if not nodes:
        logging.error("No nodes found in dependency graph")
        sys.exit(1)

    logging.info(f"Found {len(nodes)} nodes in dependency graph")

    git_deps = []
    archive_deps = []
    custom_deps = []

    for node_id, node in nodes.items():
        label = node.get("label", "")
        ref = node.get("ref", "")
        name = node.get("name", "")

        if "conanfile" in label.lower() or node.get("recipe") == "Consumer":
            logging.info(f"Skipping {label} (root/consumer node)")
            continue

        if node.get("context") == "build":
            logging.info(f"Skipping {ref or name} (build context)")
            continue

        if node.get("test", False):
            logging.info(f"Skipping {ref or name} (test dependency)")
            continue

        if node.get("binary") == "Skip" and node.get("package_type") == "header-library":
            logging.info(f"Skipping {ref or name} (header-only, skipped binary)")
            continue

        package_name = name or (ref.split("/")[0] if "/" in ref else ref)

        # Private channel packages without conandata have no upstream source
        # record to confirm their version — skip git lookup to avoid resolving
        # an internal version number as a git tag.
        is_private_no_conandata = bool(node.get("channel")) and not bool(node.get("conandata"))

        git_url = None
        if not is_private_no_conandata:
            git_url, _ = extract_git_url_from_sources(node)
            if not git_url:
                git_url, _ = extract_git_url(node)

        # Prefer the tag embedded in the conandata source URL when available —
        # some packages use non-standard tag formats (e.g. curl-8_17_0) that
        # differ from the plain conan version string.
        version = extract_git_tag_from_source_url(node) or parse_version(node)

        if git_url and version:
            logging.info(f"Adding (git): {package_name} → {git_url} @ {version}")
            git_deps.append({"type": "git", "name": git_url, "version": version})
        elif version and package_name:
            archive_url = extract_archive_url(node)
            if archive_url:
                logging.info(f"Adding (archive): {package_name} {version} — {archive_url}")
                archive_deps.append({"name": package_name, "version": version, "url": archive_url})
            else:
                license_info = node.get("license", "UNKNOWN")
                homepage = node.get("homepage") or node.get("url", "")
                description = node.get("description", "")
                logging.info(f"Adding (custom): {package_name} {version} — no URL found")
                custom_deps.append({
                    "name": package_name,
                    "version": version,
                    "license": license_info,
                    "metadata": {"homepage": homepage, "description": description},
                })
        else:
            logging.warning(f"Skipping {package_name} (insufficient info)")

    git_deps.sort(key=lambda x: x["name"])
    archive_deps.sort(key=lambda x: x["name"])
    custom_deps.sort(key=lambda x: x["name"])

    yaml_lines = [
        "# This is an auto-generated fossa-deps file for Conan project.",
        f"# Generated at: {datetime.now().isoformat()}",
        "#",
        "# Docs: https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-deps.md",
        "# FOSSA: https://fossa.com",
        "# FOSSA Support: https://support.fossa.com/hc/en-us",
        "",
    ]

    if git_deps:
        yaml_lines.append("referenced-dependencies:")
        for dep in git_deps:
            yaml_lines.append(f"- type: {dep['type']}")
            yaml_lines.append(f"  name: {dep['name']}")
            yaml_lines.append(f"  version: \"{dep['version']}\"")
            yaml_lines.append("")

    if archive_deps:
        yaml_lines.append("remote-dependencies:")
        for dep in archive_deps:
            yaml_lines.append(f"- name: {dep['name']}")
            yaml_lines.append(f"  version: \"{dep['version']}\"")
            yaml_lines.append(f"  url: {dep['url']}")
            yaml_lines.append("")

    if custom_deps:
        yaml_lines.append("custom-dependencies:")
        for dep in custom_deps:
            yaml_lines.append(f"- name: {dep['name']}")
            yaml_lines.append(f"  version: \"{dep['version']}\"")
            yaml_lines.append(f"  license: {dep['license']}")
            metadata = dep.get("metadata", {})
            if metadata.get("homepage") or metadata.get("description"):
                yaml_lines.append("  metadata:")
                if metadata.get("homepage"):
                    yaml_lines.append(f"    homepage: {metadata['homepage']}")
                if metadata.get("description"):
                    yaml_lines.append(f"    description: {metadata['description']}")
            yaml_lines.append("")

    with open("fossa-deps.yml", "w") as f:
        f.write("\n".join(yaml_lines))

    logging.info(
        f"Successfully generated fossa-deps.yml — "
        f"{len(git_deps)} git, {len(archive_deps)} archive, {len(custom_deps)} custom"
    )
    logging.info("Run 'fossa analyze' to upload to FOSSA")


def get_graph_from_file(json_path):
    """Read and parse a pre-generated conan graph info JSON file."""
    logging.info(f"Reading graph from file: {json_path}")
    with open(json_path, encoding="utf-8") as f:
        content = f.read()

    # Strip any conan text preamble — JSON blob starts at first {
    json_start = content.find("{")
    json_end = content.rfind("}") + 1
    if json_start == -1:
        logging.error(f"No JSON object found in {json_path}")
        sys.exit(1)

    try:
        return json.loads(content[json_start:json_end])
    except json.JSONDecodeError as e:
        logging.error(f"Failed to parse JSON: {e}")
        sys.exit(1)


def get_graph_from_conan(user_args=None):
    """Run conan graph info and return parsed graph JSON."""
    if user_args is None:
        user_args = []

    logging.info(f"User-provided conan args: {user_args}")

    if "-f" in user_args or "--format" in user_args:
        raise ValueError("Cannot provide -f | --format; the script requires JSON output")

    # https://docs.conan.io/2.0/reference/commands/graph/info.html
    cmd = ["conan", "graph", "info", ".", "-f", "json"] + user_args
    logging.info(f"Running: {' '.join(cmd)}")
    result = subprocess.run(cmd, stdout=subprocess.PIPE)

    if result.returncode != 0:
        sys.exit(result.returncode)

    return json.loads(result.stdout)


if __name__ == "__main__":
    args = sys.argv[1:]

    # If the sole argument is a JSON file, read it directly; otherwise run conan.
    if len(args) == 1 and args[0].endswith(".json") and os.path.isfile(args[0]):
        data = get_graph_from_file(args[0])
    else:
        data = get_graph_from_conan(args)

    graph = data.get("graph", data)
    generate_fossa_deps(graph.get("nodes", {}))
