# Quick reference: npm

## Requirements

A `package.json` file is required to be present all types of npm analysis.

Running `npm install` and generating a `package-lock.json` file will provide significantly better results. This allows FOSSA to detect the full dependency graph.

> Note: The `package-lock.json` file is expected to be located in the same directory as the `package.json` file.

## Project discovery

Directories containing `package.json` files are considered npm projects. `node_modules` subdirectories are skipped.
