module App.Fossa.Config.VPS.Common (collectProjectMetadata) where

import App.Fossa.Config.ConfigFile (
  ConfigFile,
  mergeFileCmdMetadata,
 )
import App.Types (ProjectMetadata)

collectProjectMetadata :: Maybe ConfigFile -> ProjectMetadata -> ProjectMetadata
collectProjectMetadata cfgfile cliMetadata = maybe cliMetadata (mergeFileCmdMetadata cliMetadata) cfgfile
