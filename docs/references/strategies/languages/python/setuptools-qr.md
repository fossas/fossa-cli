# Quick reference: setuptools

## Requirements

**Ideal**

- `requirements.txt` files present in your project

**Minimum**

- `setup.py` files present in your project. 

Unfortunately, we fail to parse many valid `setup.py` files. In general, we recommend the use of `requirements.txt` files for better analysis.

## Project discovery

Directories containing `setup.py` files or `requirements.txt`-like files (e.g., `req-dev.txt`, `test-requires.txt` are valid -- or any filename matching the regex `.*req.*\.txt`) are treated as setuptools projects.
