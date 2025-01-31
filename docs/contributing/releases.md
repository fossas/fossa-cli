# Releases

So you've made a change that you want to get out to users.  Great!  Let's get that released!

We'll assume that you want to release all changes currently on `master`.  You should not release anything from another branch.

1. First, make sure that you update the release notes.
    1. Create a PR that updates the [Changelog.md](./../../Changelog.md).
    2. Rename the "Unreleased" section and to the next version, incrementing the version number as appropriate.
    3. Make sure that change makes it to the `master` branch before continuing.
2. Ensure that your local master branch is up to date and then create a _tag_ for the new release.
    The commands would look something like the following.
    ```shell
    git checkout master
    git pull origin

    export TAG_NAME="vX.Y.Z"
    git tag --annotate $TAG_NAME -m "Releases $TAG_NAME"
    git push origin $TAG_NAME
    ```
    Replace `vX.Y.Z` with your version number, such as `v3.1.4`.  The tag _must_ start with `v`.
3. GitHub Actions will take the tag and run some tests before generating a draft release which can be found on the [releases page](https://github.com/fossas/fossa-cli/releases).
4. If nothing has changed to dissuade you it's time to publish.
    1. Add the notes for this release from [Changelog.md](./../../Changelog.md) to the release description.
    2. Hit "publish" at the bottom.
5. Inform our users!
