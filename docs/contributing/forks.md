# Accepting PRs from a fork

This document is mainly for FOSSA employees. It explains the process that we use to accept PRs from forks, and why we need this process.

Unfortunately, our CI does not work with PRs from forks. This is because we pull in some proprietary executables and CI will only have the right secrets to download those executables if it is run from the main repo.

To accept a PR from a fork, you create a new branch and cherry pick the commits from the forked PR onto the new branch. Then you push that branch to the main repo and create a new PR.

In order to cherry pick the commits, you will have to [add the fork as a remote to your local repo](https://simonhartcher.com/how-to-cherry-pick-a-git-commit-from-a-fork/), fetch the remote and then cherry-pick.

```
git checkout -b my-branch
git remote add fork git@github.com:<username>/fossa-cli.git
git fetch fork
git cherry-pick <commit id>
<repeat for all commits in the PR>
git remote rm fork
git push -u origin my-branch
```

Then, on the original PR, add a message like this:

> Hi, thank you for this contribution. Unfortunately due to the way our CI works (we pull in a few proprietary executables) I have to make a new PR with it. You will still be credited for the work of course.
>
> The new PR is here: #1511

You can then close the original PR.

Here's an example PR where we did this: https://github.com/fossas/fossa-cli/pull/1510
