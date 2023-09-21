# `ingest`

This subcommand adds an open-source library to the FOSSA knowledge base.
For more information on possible options, run `millhone ingest --help`.

# Internal Users

This subcommand is run by internal FOSSA employees;
end users are not able to use this command to ingest matches.

This is controlled by the permissions granted to the API keys for the service.

# Ingesting a library

First, download the library locally, and determine the locator that FOSSA would use
to refer to this library in the future.

For example, if the library was at `https://github.com/openssh/libopenssh`,
the locator would be something like `git+github.com/openssh/libopenssh$05dfdd5f54d9a1bae5544141a7ee65baa3313ecd`.

Once these are both performed, run the program with the appropriate arguments to ingest:
```shell
millhone ingest \
  --locator 'git+github.com/openssh/libopenssh$05dfdd5f54d9a1bae5544141a7ee65baa3313ecd' \
  ~/projects/scratch/libopenssh
```

> [!IMPORTANT]
> Make sure to fill in your own data for these arguments.

For more information on possible options, run `millhone ingest --help`.

# Next Steps

Now that the library is ingested, you can discover matches via `millhone analyze`.
