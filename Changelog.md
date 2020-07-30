# v2.2.0

- Fixes `fossa test` and project links for git projects with `https` remotes (#92)

- Fixes strategy failures related to command-not-found errors (#106)

- Merges the dependencies of `*req*.txt` files we find (#102)

- Re-enables deep dependency gathering for golang projects (#98)

- Fixes directory skipping (e.g., `node_modules`) (#100)

- Adds CLI-side support for contributor counting (#94)

- Enables paket.lock strategy (#107)

- Improves parallelism of strategy discovery (#93)
