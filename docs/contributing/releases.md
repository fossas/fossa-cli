# Releases

So you've made a change that you want to get out to users.  Great!  Let's get that released!

We'll assume that you want to release all changes currently on `master`.  You should not release anything from another branch.

1. First, make sure that you update the release notes.
    a. Create a PR that updates the [Changelog.md](/Changelog.md).
    b. Rename the "Unreleased" section and to the next version, incrementing the version number as appropriate.
    c. Make sure that change makes it to the `master` branch before continuing.
2. Ensure that your local master branch is up to date and then create a _tag_ for the new release.
    The commands would look something like the following.
    ```shell
    git checkout master
    git pull origin

    git tag -a vX.Y.Z -m "Releases vX.Y.Z"
    git push origin vX.Y.Z
    ```
    Replace `vX.Y.Z` with your version number, such as `v3.1.4`.  The tag _must_ start with `v`.
3. GitHub Actions will take the tag and run some tests before generating a draft release which can be found on the [releases page](https://github.com/fossas/fossa-cli/releases).
4. If nothing has changed to dissuade you it's time to publish.  Edit the draft release and hit "publish" at the bottom.  You should not need to change anything else about the release.
5. Inform our users!