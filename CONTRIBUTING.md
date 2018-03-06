# Contributing

## Adding language integrations

See [Adding New Languages](docs/integrations/adding-new-languages.md).

## Running tests

Since `fossa` relies on having the correct build tools in your local environment, running `fossa` tests requires being able to successfully build all projects in `test/fixtures/`. To provide these tools and prevent you from clobbering your local machine, we have run tests in a Docker container defined at `test/Dockerfile`.

## License

Check out our [DCO](DCO) for contribution certifications.

This repository is licensed under the AGPLv3:

```
Copyright (C) 2018 FOSSA, Inc.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```
