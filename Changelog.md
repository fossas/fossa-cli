# v2.2.1

- Fixes bug where the req.txt strategy would run even when no relevant files were present ([#109](https://github.com/fossas/spectrometer/pull/109))

# v2.2.0

- Fixes `fossa test` and project links for git projects with `https` remotes ([#92](https://github.com/fossas/spectrometer/pull/92))

- Fixes strategy failures related to command-not-found errors ([#106](https://github.com/fossas/spectrometer/pull/106))

- Merges the dependencies of `*req*.txt` files we find ([#102](https://github.com/fossas/spectrometer/pull/102))

- Re-enables deep dependency gathering for golang projects ([#98](https://github.com/fossas/spectrometer/pull/98))

- Fixes directory skipping (e.g., `node_modules`) ([#100](https://github.com/fossas/spectrometer/pull/100))

- Adds CLI-side support for contributor counting ([#94](https://github.com/fossas/spectrometer/pull/94))

- Enables paket.lock strategy ([#107](https://github.com/fossas/spectrometer/pull/107))

- Improves parallelism of strategy discovery ([#93](https://github.com/fossas/spectrometer/pull/93))
