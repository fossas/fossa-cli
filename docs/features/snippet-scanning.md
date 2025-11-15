# Snippet Scanning

FOSSA's Snippet Scanning feature searches your codebase for snippets of code that may have been copied from open source projects or other sources (e.g. from StackOverflow).

You run a snippet scan on your codebase by adding the `--x-snippet-scan` flag to the `fossa analyze` command:

```
fossa analyze --x-snippet-scan
```

## Enabling snippet scanning for your organization

Snippet Scanning is not enabled by default, and is only available for enterprise customers. If you would like to enable it for your organization, please [contact us](https://support.fossa.com).

## Snippet Scanning UI

Once you have run a snippet scan, you can see the results in FOSSA's UI under the "snippets" section of the inventory dropdown for the revision that you just scanned.

![The snippet inventory dropdown](../assets/snippet-scanning-inventory.png)

Clicking on snippets in that dropdown will bring you to the snippet scanning UI.

![Snippet Scanning UI](../assets/snippet-scanning-ui.png)

## How Snippet Scanning Works

## What data is sent to FOSSA's servers

When we snippet scan your code, we detect snippets in two steps. First, we generate a [CRC64 hash](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) of every file's contents and send that hash to FOSSA. This allows us to determine if we already have snippet scan data for the contents of that file.

If we have the snippet scan data for this file already stored on our servers, then there is no need to fingerprint the file, and no more data for that file will be sent to our servers.

If we do not have snippet scan data for that file stored, then we fingerprint the file. A fingerprint will contain a series of [CRC64 hashes](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) generated from the contents of your file, and does not contain any of your file's contents.

We compare the fingerprint that is sent to our servers to fingerprints in our knowledge base. If, and only if, we find a match to your file in our knowledge base, we will send up the contents of the file to our servers. The contents will be stored on our servers for a maximum of 30 days. We store the code on our servers so that you can compare the code in your codebase to the snippet matches that we find.

![The Code Comparision screen](../assets/snippet-compare-ui.png)

The Snippet Scan retention time can be configured for your organization by going to <https://app.fossa.com/account/settings/organization> and changing the "Source Code Retention" setting in the "Snippets" section. Note that you must have permission to edit your organization to see this page. Also note that the Snippets section will only exist if Snippet Scanning has been enabled for your organization.

![The Source Code Retention Setting](../assets/snippet-retention-days-setting.png)


After 30 days have passed, we will remove the code from our servers.

## A note on scan times

The first time you run a snippet scan on a codebase, it can take a long time to scan. For example, scanning [Linux](https://github.com/torvalds/linux) takes around 60 minutes. This is because we spend quite a bit of time comparing the fingerprints to our knowledge base.

However, the next time you scan that codebase we will already have the snippet data for any unchanged files cached. For example, if you snippet scan that same revision of Linux a second time, the scan will complete in less than a minute.

The time it takes to scan newer versions of your codebase will depend on how many files in the new version have not been previously scanned. A file has been previously scanned if the exact same file has ever been snippet scanned. FOSSA recommends snippet scanning your codebase on a regular basis to keep scan times low.
