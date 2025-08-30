# ANE-2523: csproj analysis failing

## Original Ticket

### Overview

When running Nuget package analysis the CLI only expects 1 packageReference file per directory. We should expect multiple to be acceptable.



## Relevant Code

#### src/Strategy/NuGet.hs

The issue is that we are creating an array of [NuGetProject file], but only ever adding one element. We need to add multiple by returning multiple from `isPackageRefFile`

```haskell
findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [NuGetProject]
findProjects = walkWithFilters' $ \_ _ files -> do
  case findProjectAssetsJsonFile files of
    Just file -> pure ([NuGetProject file], **WalkContinue**)
    Nothing -> case find isPackageRefFile files of
      Just file -> pure ([NuGetProject file], WalkContinue)
      Nothing -> pure ([], WalkContinue)
```

## Practical Notes

### Tests

- We need tests for the following
	- If a directory has multiple packagereference files, it successfully analyzes both and combines the results
	- If a project has both packagereference files and a project.assets.json file, the project.assets.json file takes priority

### Implementation

- We need to accept multiple packge reference files per directory, which happens on line 54 in src/Strategy/NuGet.hs
- WE should refactor the code so that NuGetProject has two fields
  - project.assets.json - a single file.
  - package reference file - an array 
- The main logic remains the same, except that we check for the existence of a "project.assets.json" file before descending into the analyze functions
- We need to handle parsing multiple package reference files. This happens in src/Strategy/NuGet/PackageReference.hs

Notes
- We need to ensure that things that find `project.assets.json` in the directory still execute first.