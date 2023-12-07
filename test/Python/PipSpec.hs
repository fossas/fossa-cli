{-# LANGUAGE QuasiQuotes #-}

module Python.PipSpec (spec) where

import Data.Text (Text)
import Strategy.Python.Pip (PackageMetadata (..), pipShowParser)
import Text.Megaparsec (runParser)
import Text.RawString.QQ (r)

import Test.Hspec

expected :: [PackageMetadata]
expected =
  [ PackageMetadata "certifi" "2023.11.17" []
  , PackageMetadata "charset-normalizer" "3.3.2" []
  , PackageMetadata "idna" "3.4" []
  , PackageMetadata "requests" "2.31.0" ["certifi", "charset-normalizer", "idna", "urllib3"]
  , PackageMetadata "urllib3" "2.0.7" []
  ]

spec :: Spec
spec =
  describe "parse" $
    it "should generate a list of packages" $ do
      runParser pipShowParser "" pipShow `shouldBe` Right expected

pipShow :: Text
pipShow =
  [r|
Name: certifi
Version: 2023.11.17
Summary: Python package for providing Mozilla's CA Bundle.
Home-page: https://github.com/certifi/python-certifi
Author: Kenneth Reitz
Author-email: me@kennethreitz.com
License: MPL-2.0
Location: /envs/test-fossa-cli/lib/python3.10/site-packages
Requires:
Required-by: requests
---
Name: charset-normalizer
Version: 3.3.2
Summary: The Real First Universal Charset Detector. Open, modern and actively maintained alternative to Chardet.
Home-page: https://github.com/Ousret/charset_normalizer
Author: Ahmed TAHRI
Author-email: ahmed.tahri@cloudnursery.dev
License: MIT
Location: /envs/test-fossa-cli/lib/python3.10/site-packages
Requires:
Required-by: requests
---
Name: idna
Version: 3.4
Summary: Internationalized Domain Names in Applications (IDNA)
Home-page:
Author:
Author-email: Kim Davies <kim@cynosure.com.au>
License:
Location: /envs/test-fossa-cli/lib/python3.10/site-packages
Requires:
Required-by: requests
---
Name: requests
Version: 2.31.0
Summary: Python HTTP for Humans.
Home-page: https://requests.readthedocs.io
Author: Kenneth Reitz
Author-email: me@kennethreitz.org
License: Apache 2.0
Location: /envs/test-fossa-cli/lib/python3.10/site-packages
Requires: certifi, charset-normalizer, idna, urllib3
Required-by:
---
Name: urllib3
Version: 2.0.7
Summary: HTTP library with thread-safe connection pooling, file post, and more.
Home-page:
Author:
Author-email: Andrey Petrov <andrey.petrov@shazow.net>
License:
Location: /envs/test-fossa-cli/lib/python3.10/site-packages
Requires:
Required-by: requests
|]
