# Ficus Integration Test Project

This is a test project used for integration testing of the ficus snippet scanning functionality in the FOSSA CLI.

## Files

- `main.c` - Main program with various C constructs for snippet analysis
- `helper.h` - Header file with function declarations and common macros
- `helper.c` - Implementation of helper functions with typical C patterns
- `README.md` - This documentation file

## Purpose

These files contain various C programming patterns that might be detected by ficus fingerprinting:

- Standard library usage (`stdio.h`, `stdlib.h`, `string.h`)
- Memory allocation and deallocation patterns
- String manipulation functions
- Mathematical operations
- Loops and conditionals
- Macro definitions
- Struct definitions
- Common C idioms and patterns

The goal is to provide realistic C code that ficus can analyze for snippets while remaining simple enough for integration testing.

## Building

This is not intended to be built as a real program, but rather analyzed by ficus for snippet detection.

```bash
# This is what ficus would analyze:
ficus analyze --endpoint <endpoint> --secret <api-key> --locator <project-locator>
```