-- | Get the Git remote URL for the repository
getRemoteUrl :: Has Git sig m => m (Maybe Text)
getRemoteUrl = do
  -- Try to get the origin remote URL
  result <- git ["config", "--get", "remote.origin.url"]
  pure $ case result of
    Right url -> Just (Text.strip url)
    Left _ -> Nothing 