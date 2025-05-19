
<b>Pattern 1: Use `void` instead of `_ <-` for discarding monadic action results to improve code readability and follow idiomatic Haskell style.</b>

Example code before:
```
parsePathDep = do
  _ <- symbol ".package" <* symbol "("
  _ <- optionallyTry (parseKeyValue "name" parseQuotedText)
  path <- parseKeyValue "path" parseQuotedText
  _ <- symbol ")"
  pure $ PathSource path
```

Example code after:
```
parsePathDep = do
  void $ symbol ".package" <* symbol "("
  void $ optionallyTry (parseKeyValue "name" parseQuotedText)
  path <- parseKeyValue "path" parseQuotedText
  void $ symbol ")"
  pure $ PathSource path
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1515#discussion_r1979769763
- https://github.com/fossas/fossa-cli/pull/1524#discussion_r1995868464
</details>


___

<b>Pattern 2: Prefer using applicative operators like `*>` and `$>` over monadic style with `do` notation when sequencing actions without using intermediate results.</b>

Example code before:
```
parseMap = do
  void $ symbol "#"
  void $ symbol "{"
  void $ parseMapPairs `sepBy` symbol ","
  void $ symbol "}"
  pure $ ErlTuple []
```

Example code after:
```
parseMap = 
  symbol "#" *>
    symbol "{" *>
    parseMapPairs `sepBy` symbol "," *>
    symbol "}" $>
    ErlTuple []
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1524#discussion_r1995868464
</details>


___

<b>Pattern 3: Use `[]` instead of `Maybe [a]` when representing optional lists, as empty lists already represent the absence of items.</b>

Example code before:
```
data AdditionalDepData = AdditionalDepData
  { userDefinedDeps :: Maybe [SourceUserDefDep]
  , remoteDeps :: Maybe [SourceRemoteDep]
  }
```

Example code after:
```
data AdditionalDepData = AdditionalDepData
  { userDefinedDeps :: [SourceUserDefDep]
  , remoteDeps :: [SourceRemoteDep]
  }
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1505#discussion_r1955274347
- https://github.com/fossas/fossa-cli/pull/1466#discussion_r1761986591
- https://github.com/fossas/fossa-cli/pull/1466#discussion_r1761993467
- https://github.com/fossas/fossa-cli/pull/1466#discussion_r1762005237
</details>


___

<b>Pattern 4: When parsing JSON with Aeson, use `.!= []` instead of `.:?` with `Maybe` for optional array fields to simplify code and avoid unnecessary `Maybe` handling.</b>

Example code before:
```
parseJSON = withObject "SomeType" $ \obj ->
  SomeType
    <$> obj .: "required_field"
    <*> obj .:? "optional_array"
```

Example code after:
```
parseJSON = withObject "SomeType" $ \obj ->
  SomeType
    <$> obj .: "required_field"
    <*> obj .:? "optional_array" .!= []
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1466#discussion_r1761986591
</details>


___

<b>Pattern 5: Add comprehensive test coverage for new features, including both unit tests and integration tests that verify the feature works as expected.</b>

Example code before:
```
it' "should apply licenseScanPathFilters' only filter correctly" $ do
  let filters =
        LicenseScanPathFilters
          { licenseScanPathFiltersOnly = [GlobFilter "**/*one.txt"]
          , licenseScanPathFiltersExclude = [GlobFilter "**/*something.txt"]
          , licenseScanPathFilterFileExclude = []
          }
```

Example code after:
```
it' "should apply licenseScanPathFilters' only filter correctly" $ do
  let filters =
        LicenseScanPathFilters
          { licenseScanPathFiltersOnly = [GlobFilter "**/*one.txt"]
          , licenseScanPathFiltersExclude = []
          , licenseScanPathFilterFileExclude = []
          }
  
it' "should apply licenseScanPathFilters' exclude filter correctly" $ do
  let filters =
        LicenseScanPathFilters
          { licenseScanPathFiltersOnly = [GlobFilter "**/*.txt"]
          , licenseScanPathFiltersExclude = [GlobFilter "**/*something.txt"]
          , licenseScanPathFilterFileExclude = []
          }
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1535#discussion_r2080296947
- https://github.com/fossas/fossa-cli/pull/1445#discussion_r1661624136
</details>


___

<b>Pattern 6: Include clear, detailed documentation for new features, explaining both how to use them and any interactions with existing features.</b>

Example code before:
```
| [`--static-only-analysis`](../strategies/README.md#static-and-dynamic-strategies) | Do not use third-party tools when analyzing projects. |
```

Example code after:
```
| [`--static-only-analysis`](../strategies/README.md#static-and-dynamic-strategies) | Do not use third-party tools when analyzing projects. |
| `--strict` | Enforces strict analysis to ensure the most accurate results by rejecting fallbacks. When run with `--static-only-analysis`, the most optimal static strategy will be applied without fallbacks. |
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1463#discussion_r1739918456
- https://github.com/fossas/fossa-cli/pull/1463#discussion_r1742317292
- https://github.com/fossas/fossa-cli/pull/1478#discussion_r1825256529
</details>


___

<b>Pattern 7: When making changes to the codebase, update the Changelog with clear descriptions of the changes, including links to the relevant PRs.</b>

Example code before:
```
# FOSSA CLI Changelog

## 3.9.26

- Reports: Add `includeCopyrightList` to JSON attribution report request. This will ensure that all copyrights are included in the JSON attribution report once the FOSSA API starts including them. All other formats of attribution reports will receive all copyrights without needing to add this query param.
```

Example code after:
```
# FOSSA CLI Changelog

## 3.9.26

- Reports: Add `includeCopyrightList` to JSON attribution report request. This will ensure that all copyrights are included in the JSON attribution report once the FOSSA API starts including them. All other formats of attribution reports will receive all copyrights without needing to add this query param. [#1450](https://github.com/fossas/fossa-cli/pull/1450)
- Resolves an issue where git projects cloned with an url including a username were unable to be found when running `fossa test`. [#1451](https://github.com/fossas/fossa-cli/pull/1451)
```

<details><summary>Examples for relevant past discussions:</summary>

- https://github.com/fossas/fossa-cli/pull/1451#discussion_r1681551682
- https://github.com/fossas/fossa-cli/pull/1480#discussion_r1841257573
- https://github.com/fossas/fossa-cli/pull/1473#discussion_r1769063911
</details>


___
