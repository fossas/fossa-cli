{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Reachability.JarSpec (spec) where

import App.Fossa.Reachability.Jar (callGraphFromJar)
import App.Fossa.Reachability.Types (
  ContentRef (..),
  ParsedJar (..),
 )
import Control.Effect.Lift (sendIO)
import Data.ByteString.Lazy qualified as LB
import Data.Text (Text)
import Data.Text.Encoding qualified as TL
import Path (Abs, File, Path, mkRelFile, (</>))
import Path.IO qualified as PIO
import Test.Effect
import Test.Hspec (Spec, describe)
import Text.RawString.QQ (r)

sampleJarContent :: Text
sampleJarContent =
  [r|C:vuln.project.sample.App java.lang.Object
C:vuln.project.sample.App java.net.URI
C:vuln.project.sample.App java.lang.System
C:vuln.project.sample.App vuln.project.sample.App
C:vuln.project.sample.App java.io.PrintStream
C:vuln.project.sample.App org.dom4j.io.SAXReader
C:vuln.project.sample.App java.lang.Exception
C:vuln.project.sample.App org.dom4j.DocumentException
M:vuln.project.sample.App:<init>() (O)java.lang.Object:<init>()
M:vuln.project.sample.App:main(java.lang.String[]) (O)java.net.URI:<init>(java.lang.String)
M:vuln.project.sample.App:main(java.lang.String[]) (M)java.net.URI:toURL()
M:vuln.project.sample.App:main(java.lang.String[]) (S)vuln.project.sample.App:parse(java.net.URL)
M:vuln.project.sample.App:main(java.lang.String[]) (M)java.io.PrintStream:println(java.lang.Object)
M:vuln.project.sample.App:parse(java.net.URL) (O)org.dom4j.io.SAXReader:<init>()
M:vuln.project.sample.App:parse(java.net.URL) (M)org.dom4j.io.SAXReader:read(java.net.URL)|]

sampleJarFile :: IO (Path Abs File)
sampleJarFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/sample.jar"))

malformedJarFile :: IO (Path Abs File)
malformedJarFile = do
  cwd <- PIO.getCurrentDir
  pure (cwd </> $(mkRelFile "test/Reachability/testdata/malformed.jar"))

sampleJarParsed :: Path Abs File -> ParsedJar
sampleJarParsed path =
  ParsedJar
    { parsedJarPath = path
    , parsedJarContent = (ContentRaw $ LB.fromStrict . TL.encodeUtf8 $ sampleJarContent)
    }

spec :: Spec
spec = describe "callGraphFromJar" $ do
  it' "should get call graph from jar file" $ do
    jarFile <- sendIO sampleJarFile
    resp <- callGraphFromJar jarFile

    resp `shouldBe'` Just (sampleJarParsed jarFile)

  it' "should get not get call graph from corruped jar file" $ do
    jarFile <- sendIO malformedJarFile
    resp <- callGraphFromJar jarFile

    resp `shouldBe'` Nothing
