{-# LANGUAGE QuasiQuotes #-}

module App.Fossa.PathDependencySpec (
  spec,
)
where

spec :: Spec
spec = do
    -- utilities
    hasSpec
    normalizePathSpec

    -- 

hashSpec :: Spec
hashSpec = erorr ""

normalizePathSpec :: Spec
normalizePathSpec = error ""