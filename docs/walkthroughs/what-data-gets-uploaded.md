# What Data Gets Uploaded to FOSSA's Servers

We upload a few different types of data to FOSSA's servers. These types are
* The dependency list from running `fossa analyze`
* Vendored dependency data
* Telemetry data

The following sections describe what these different types of data look like. At the end of this document we describe a tool called `echotraffic` that allows you to see exactly what data we are sending to our servers.

## Dependency list

When you run `fossa analyze`, we find the dependencies used by your project and then upload the list of dependencies to FOSSA's servers for analysis.

You can see what we will upload for your specific project by running `fossa analyze --output`.

As an example, we ran `fossa analyze --output` on https://github.com/bohnman/squiggly. You can [view the results here](../assets/fossa-analyze-output.json).

## Vendored Dependencies

If you have a `fossa-deps.yml` file that contains a `vendored-dependencies` section ([vendored dependencies documentation](../features/vendored-dependencies.md)), then we will scan and upload data for the vendored dependencies listed.

There are two types of vendored dependency scans: "CLI license scan" and "Archive upload". By default you will be using CLI license scans.

A "CLI license scan" inspects vendored dependencies for licensing on the local system within the CLI, and only uploads the matched license data to FOSSA's servers.

"Archive upload" uploads vendored dependencies to a secure S3 bucket. We license scan the uploaded files on our servers. All files that do not contain licenses are then removed after 30 days.

You can figure out whether the scan was a CLILicenseScan or an ArchiveUpload by running `fossa analyze` with the `--debug` flag: `fossa analyze --debug`.

If it was a CLILicenseScan, then you will see this in the logs:

```
"License Scanning '<vendored dependency name>' at '<vendored dependency path>'.

```

If it was an ArchiveUpload, then you will see this in the logs:

```
"Compressing '<vendored dependency name>' at '<vendored dependency path>'.

```

Here is a sample output of the data that gets uploaded to the server for a CLI license scan. The license text has been truncated for readability, but other than that this is exactly what would be uploaded. You can also [download the full JSON](../assets/license-scan-data.json).

```json
{
  "uploadUnits": [
    {
      "LicenseUnits": [
        {
          "Data": [
            {
              "Copyright": null,
              "Copyrights": null,
              "ThemisVersion": "df717b7762a7441a82713f03ad626b65734060ae",
              "match_data": [
                {
                  "end_line": 201,
                  "index": 0,
                  "length": 11322,
                  "location": 33,
                  "match_string": "Apache License\n                           Version 2.0, January 2004\n                        http://www.apache.org/licenses/\n\n   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION ...  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n   See the License for the specific language governing permissions and\n   limitations under the License.",
                  "start_line": 1
                }
              ],
              "path": "vendored/apache-2.0.LICENSE"
            }
          ],
          "Dir": "",
          "Files": [
            "vendored/apache-2.0.LICENSE"
          ],
          "Info": {
            "Description": ""
          },
          "Name": "apache-2.0",
          "Type": "LicenseUnit"
        },
        {
          "Data": [
            {
              "Copyright": null,
              "Copyrights": null,
              "ThemisVersion": "df717b7762a7441a82713f03ad626b65734060ae",
              "match_data": [
                {
                  "end_line": 18,
                  "index": 0,
                  "length": 1021,
                  "location": 0,
                  "match_string": "Permission is hereby granted, free of charge, to any person obtaining\na copy of this software and associated documentation files (the\n\"Software\"), to deal in the Software without restriction, including\nwithout limitation the rights to use, copy, modify, merge, publish,\ndistribute, sublicense, and/or sell copies of the Software, and to\npermit persons to whom the Software is furnished to do so, subject to\nthe following conditions:\n\nThe above copyright notice and this permission notice shall be\nincluded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\nEXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\nMERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\nIN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY\nCLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,\nTORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE\nSOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.",
                  "start_line": 1
                }
              ],
              "path": "vendored/mit.LICENSE"
            }
          ],
          "Dir": "",
          "Files": [
            "vendored/mit.LICENSE"
          ],
          "Info": {
            "Description": ""
          },
          "Name": "mit",
          "Type": "LicenseUnit"
        }
      ],
      "Name": "vendored",
      "Type": "cli-license-scanned"
    }
  ]
}

```

You can see exactly what data we are uploading for a CLI license scan for your code by running the following command:

```
fossa license-scan fossa-deps
```

## Telemetry

FOSSA CLI collects anonymous feature usage information, observed errors and warnings, and performance diagnostic information to help improve the experience for everyone.

You can get more information about what telemetry data gets uploaded and how to opt-out of telemetry uploads [here](../telemetry.md).

## Inspecting exactly what we upload with `echotraffic`

FOSSA provides an open-source tool called [`echotraffic`](https://github.com/fossas/echotraffic) that you can use to see exactly what we data we are sending to our servers.

Follow the directions in `echotraffic`'s README to get started.
