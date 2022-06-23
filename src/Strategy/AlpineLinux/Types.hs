module Strategy.AlpineLinux.Types (AlpinePackage (..)) where

import Data.Text (Text)

data AlpinePackage = AlpinePackage
  { alpinePackageArchitecture :: Text
  , alpinePackageName :: Text
  , alpinePackageVersion :: Text
  }
  deriving (Show, Eq, Ord)
