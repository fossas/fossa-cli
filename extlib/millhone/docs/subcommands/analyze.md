# `analyze`

This subcommand analyzes a local project for snippets that match snippets in the FOSSA knowledgebase.
For more information on possible options, run `millhone analyze --help`.

## Output

Matches are written to the location specified by the `--output` (or `-o`) argument.

The output directory consists of a set of flat files, each representing a file in the scan directory
that had at least one matching snippet. These files are named with the path of the file relative to
the scan directory, with any path separators replaced by underscores, and a `.json` extension appended.

For example, the following project:
```
example-project/
  lib/
    lib.c
    vendor/
      openssh/
        openssh.c
  main.c
```

When scanned like `fossa snippets analyze -o snippets`,
would be presented like the below if all files contained a snippet match:
```
snippets/
  lib_lib.c.json
  lib_vendor_openssh_openssh.c.json
  main.c.json
```

The content of each of these files is a JSON encoded array of matches,
where each object in the array consists of the following keys:

Key                 | Description
--------------------|-------------------------------------------------------------------------------
`found_in`          | The relative path of the local file in which the snippet match was found.
`local_text`        | The text that matched the snippet in the local file.
`local_snippet`     | Information about the snippet extracted from the local file.
`matching_snippets` | A collection of snippets from the FOSSA knowledgebase that match this snippet.

The `local_snippet` object has the following keys:

Key           | Description
--------------|---------------------------------------------------------------------------
`fingerprint` | The base64 representation of the snippet fingerprint.
`target`      | The kind of source code item that matched for this snippet.
`kind`        | The kind of snippet that was matched.
`method`      | The normalization method used on the matching snippet.
`file_path`   | The path of the file containing the snippet, relative to the project root.
`byte_start`  | The byte index in the file at which the snippet begins.
`byte_end`    | The byte index in the file at which the snippet ends.
`line_start`  | The line number in the file at which the snippet begins.
`line_end`    | The line number in the file at which the snippet ends.
`col_start`   | The column number on the `line_start` at which the snippet begins.
`col_end`     | The column number on the `line_end` at which the snippet ends.
`language`    | The language of the identified snippet.

Each entry in the `matching_snippets` collection has the following keys:

Key           | Description
--------------|---------------------------------------------------------------------------
`locator`     | The FOSSA identifier for the project to which this snippet belongs.
`fingerprint` | The base64 representation of the snippet fingerprint.
`target`      | The kind of source code item that matched for this snippet.
`kind`        | The kind of snippet that was matched.
`method`      | The normalization method used on the matching snippet.
`file_path`   | The path of the file containing the snippet, relative to the project root.
`byte_start`  | The byte index in the file at which the snippet begins.
`byte_end`    | The byte index in the file at which the snippet ends.
`line_start`  | The line number in the file at which the snippet begins.
`line_end`    | The line number in the file at which the snippet ends.
`col_start`   | The column number on the `line_start` at which the snippet begins.
`col_end`     | The column number on the `line_end` at which the snippet ends.
`language`    | The language of the identified snippet.
`ingest_id`   | The ingestion run that discovered this snippet (not meaningful to users).

# Correcting Matches

In order to correct matches, users may manually edit the contents of this directory
or files within the directory to alter or remove matches.

For example, if a certain snippet is found in the local code that matches
a snippet in the FOSSA knowledgebase, but it's known to be a false positive,
users can script the removal of that snippet match from this directory prior to
committing these results in a FOSSA scan.

# Next Steps

After running `fossa snippets analyze`, the next step is to run `fossa snippets commit`.

These are separate steps to give users the ability to edit or review the matched data
prior to submitting the results to FOSSA.
