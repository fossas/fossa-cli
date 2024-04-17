## Vendored Source Identification

Some projects, especially C or C++ projects, vendor open source libraries in directories adjacent to the first party code being developed in-house.
FOSSA's Vendored Source Identification is intended to support identifying and categorizing such vendored libraries.

### How does it work?

FOSSA fingerprints all files in your project and uploads those fingerprints to our analysis service.
The analysis service then uses a proprietary algorithm to compare those fingerprints with the fingerprints in our database of open source projects.

In the FOSSA CLI, this feature is called "VSI", for "vendored source identification".
VSI can be enabled with the `--detect-vendored` flag when running `fossa analyze`. For example:

```
fossa analyze --detect-vendored
```

### Default Filters

By default, VSI ignores the following directory:

- `{scandir}/.git`

### FAQ

#### Is there a limit to the number of scanned files?

This feature does not have a concrete limit, however at this time FOSSA considers projects that have more than 35,000 files "unsupported".
That being said, they may work.

> [!NOTE]
> Files are counted recursively, meaning that every archive contained in your project is unpacked and its contents are scanned,
> and each of those interior files count when talking about the 35,000 file supported limit.

If you find that this is a significant issue in your usage of this feature, we'd love to hear more about your use case-
we're always on the lookout for how we can improve this functionality!

#### Is my source code sent to FOSSA's servers?

VSI fingerprints your first party source code but does not send it to the server. Currently this is implemented with two SHA-256 hashes:
- A raw SHA-256 hash of the file content.
- For text files, a SHA-256 hash of the file content with lightweight comment and whitespace removal.

Additionally, the project file paths are uploaded to FOSSA; this is required for the statistical analysis to function properly.

The code to perform this is open source in this CLI; FOSSA can also provide a binary that displays the data FOSSA CLI uploads to the backend.

#### Why not always enable VSI?

VSI is more computationally intensive, and therefore takes longer to run.
For this reason, we recommend only enabling it when you are reasonably confident that you will obtain useful information by enabling it.

To explain, enabling VSI causes the CLI to:

1. Fingerprint all files in the project
2. Send those fingerprints to the FOSSA analysis service for vendored source identification
3. Wait for remote analysis to complete

This process has undergone a lot of optimization to improve its performance (and this is an area we’re continually investing in), but it will never be as fast as scanning local files for dependency information.

#### Manually specifying vendored source

Vendored dependencies can be manually specified in `fossa-deps.yml`.
For more information, see [`vendored-dependencies`](../../../features/vendored-dependencies.md) in `features`.
