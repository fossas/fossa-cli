{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stage 2 tests for the Cargo.lock -> CargoMetadata conversion pipeline:
-- workspace member enumeration (single crate, virtual workspace,
-- members + default-members).
module Cargo.LockfileSpec (
  spec,
) where

import Control.Effect.Lift (sendIO)
import Path (mkRelDir, (</>))
import Path.IO (makeAbsolute)
import Strategy.CargoLock
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)

spec :: Spec
spec = memberEnumerationSpecs

-- ===========================================================================
-- Workspace member enumeration

memberEnumerationSpecs :: Spec
memberEnumerationSpecs =
  describe "workspace member enumeration" $ do
    it' "a single crate (no [workspace]) is its own sole member" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/single_crate")
      ms <- enumerateWorkspaceMembers root
      ms `shouldBe'` [WorkspaceMember "solo" root]

    it' "a virtual workspace expands members = [\"crates/*\"]" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/virtual_ws")
      ms <- enumerateWorkspaceMembers root
      let alphaDir = root </> $(mkRelDir "crates/alpha")
          betaDir = root </> $(mkRelDir "crates/beta")
      ms `shouldBe'` [WorkspaceMember "alpha" alphaDir, WorkspaceMember "beta" betaDir]

    -- Cargo requires default-members to be a subset of members; we take the
    -- union of both lists, so this also verifies that parsing a
    -- default-members key does not disturb member enumeration.
    it' "a workspace with members and default-members enumerates all members" $ do
      root <- sendIO $ makeAbsolute $(mkRelDir "test/Cargo/testdata/default_ws")
      ms <- enumerateWorkspaceMembers root
      let alphaDir = root </> $(mkRelDir "crates/alpha")
          betaDir = root </> $(mkRelDir "crates/beta")
          gammaDir = root </> $(mkRelDir "crates/gamma")
      ms
        `shouldBe'` [ WorkspaceMember "alpha" alphaDir
                    , WorkspaceMember "beta" betaDir
                    , WorkspaceMember "gamma" gammaDir
                    ]
