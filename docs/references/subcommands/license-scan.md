## `fossa license-scan`

The license scan command runs our license scanner (Themis) on a provided directory. It can additionally be pointed to a directory which contains a `fossa-deps.yml` file in order to run a license scan on the `vendored-dependencies` specified in the file. This license scanner is the exact one that is run on dependencies by FOSSA in order to determine the license, and so you can use it to understand how FOSSA is treating a dependency to better understand your results.

### Usage

#### fossa license-scan direct

`fossa license-scan direct [DIR]`

This runs the license scan on all files within a directory. If a directory is not specified, the current directory is the default.

Example output
```bash
❯ fossa license-scan direct  
[
  {
    "Name": "apache-2.0",
    "Type": "LicenseUnit",
    "Dir": "",
    "Files": [
      "LICENSE"
    ],
    "Data": [
      {
        "path": "LICENSE",
        "Copyright": "2023 FOSSA",
        "ThemisVersion": "729499e2db6d4a69f17d8565a34b148e750a7d06",
        "match_data": [
          {
            "match_string": "Copyright 2023 FOSSA\n\nLicensed under the Apache License, Version 2.0 (the \"License\");\nyou may not use this file except in compliance with the License.\nYou may obtain a copy of the License at\n\n    http://www.apache.org/licenses/LICENSE-2.0\n\nUnless required by applicable law or agreed to in writing, software\ndistributed under the License is distributed on an \"AS IS\" BASIS,\nWITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\nSee the License for the specific language governing permissions and\nlimitations under the License.",
            "match_type": "inexact",
            "location": 0,
            "length": 544,
            "index": 0,
            "start_line": 1,
            "end_line": 13,
            "score": 0.9438202247191011
          }
        ],
        "Copyrights": [
          "2023 FOSSA"
        ]
      }
    ],
    "Repo": "",
    "Ops": {},
    "Config": {},
    "Info": {
      "Description": "2023 FOSSA"
    },
    "Dependencies": []
  }
]
```

This will group licenses by the SPDX License Identifier (if one exists for the license, otherwise the FOSSA license ID is used). The `Files` attribute will show all files where this license was found and the `Data` attribute will show each individual occurrence of the license. 

Each element of the `Data` array will contain the relative path, the copyright, the version of Themis that was used to detect the license, the match data for the license, and the copyrights from the match. The match data consists of the full match string, the type of match, the length, the start/end lines of the match, the confidence score, and some other information that likely is not relevant to your use case.

#### fossa license-scan fossa-deps

The results from `fossa license-scan fossa-deps [DIR]` can be interpreted much the same as the results from `fossa license-scan direct`, the primary difference being that the results are divided by `uploadUnits`. Each dependency will be an individual element within the `uploadUnits` array, allowing for easy determination of which license match belong to which dependency.

Example output
```bash
~/FOSSA/scratch/license-example
❯ cat fossa-deps.yml 
vendored-dependencies:
- name: fake-dependency
  version: 0.1.0
  path: ./vendor/fake-dependency

- name: fake-dependency2
  version: 0.2.0
  path: ./vendor/fake-dep2

~/FOSSA/scratch/license-example
❯ tree
.
├── fossa-deps.yml
└── vendor
    ├── fake-dep2
    │   └── license
    └── fake-dependency
        └── LICENSE

3 directories, 3 files

~/FOSSA/scratch/license-example
❯ fossa license-scan fossa-deps | jq
License Scanning 'fake-dependency' at './vendor/fake-dependency'
License Scanning 'fake-dependency2' at './vendor/fake-dep2'
{
  "uploadUnits": [
    {
      "LicenseUnits": [
        {
          "Data": [
            {
              "Contents": null,
              "Copyright": "2023 FOSSA",
              "Copyrights": [
                "2023 FOSSA"
              ],
              "ThemisVersion": "729499e2db6d4a69f17d8565a34b148e750a7d06",
              "match_data": [
                {
                  "end_line": 13,
                  "index": 0,
                  "length": 544,
                  "location": 0,
                  "match_string": "Copyright 2023 FOSSA\n\nLicensed under the Apache License, Version 2.0 (the \"License\");\nyou may not use this file except in compliance with the License.\nYou may obtain a copy of the License at\n\n    http://www.apache.org/licenses/LICENSE-2.0\n\nUnless required by applicable law or agreed to in writing, software\ndistributed under the License is distributed on an \"AS IS\" BASIS,\nWITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\nSee the License for the specific language governing permissions and\nlimitations under the License.",
                  "start_line": 1
                }
              ],
              "path": "vendor/fake-dependency/LICENSE"
            }
          ],
          "Dir": "",
          "Files": [
            "vendor/fake-dependency/LICENSE"
          ],
          "Info": {
            "Description": "2023 FOSSA"
          },
          "Name": "apache-2.0",
          "Type": "LicenseUnit"
        }
      ],
      "Name": "./vendor/fake-dependency",
      "Type": "cli-license-scanned"
    },
    {
      "LicenseUnits": [
        {
          "Data": [
            {
              "Contents": null,
              "Copyright": "2023 NotFOSSA",
              "Copyrights": [
                "2023 NotFOSSA"
              ],
              "ThemisVersion": "729499e2db6d4a69f17d8565a34b148e750a7d06",
              "match_data": [
                {
                  "end_line": 7,
                  "index": 0,
                  "length": 1021,
                  "location": 25,
                  "match_string": "Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:\n\nThe above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.",
                  "start_line": 3
                }
              ],
              "path": "vendor/fake-dep2/license"
            }
          ],
          "Dir": "",
          "Files": [
            "vendor/fake-dep2/license"
          ],
          "Info": {
            "Description": "2023 NotFOSSA"
          },
          "Name": "mit",
          "Type": "LicenseUnit"
        }
      ],
      "Name": "./vendor/fake-dep2",
      "Type": "cli-license-scanned"
    }
  ]
}
```
