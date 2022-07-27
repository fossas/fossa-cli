{-# LANGUAGE QuasiQuotes #-}

module Container.OsReleaseSpec (spec) where

import Container.OsRelease (
  OsInfo (OsInfo),
  busyBoxParser,
  centOsOldFmtParser,
  osReleaseParser,
 )
import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation, Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  parse,
 )
import Text.RawString.QQ (r)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

spec :: Spec
spec = do
  describe "example release info files" $ do
    let shouldOsReleaseParseInto = parseMatch osReleaseParser
    let shouldBusyBoxParser = parseMatch busyBoxParser
    let shouldCentOsOldFmtParser = parseMatch centOsOldFmtParser

    it "should parse release info" $ do
      alpine `shouldOsReleaseParseInto` OsInfo "alpine" "3.15.4"
      ubuntu `shouldOsReleaseParseInto` OsInfo "ubuntu" "20.04"
      fedora `shouldOsReleaseParseInto` OsInfo "fedora" "31"
      centOs `shouldOsReleaseParseInto` OsInfo "centos" "8"
      centOs7 `shouldOsReleaseParseInto` OsInfo "centos" "7"
      oracleLinux `shouldOsReleaseParseInto` OsInfo "ol" "8.5"
      amazon2Linux `shouldOsReleaseParseInto` OsInfo "amzn" "2"
      openSuseLeap `shouldOsReleaseParseInto` OsInfo "opensuse-leap" "15.4"

    it "should parse release info from cents os 6 cpe format" $ do
      centOs6SystemReleaseCpe `shouldCentOsOldFmtParser` OsInfo "centos" "6"

    it "should parse release info even with line break" $ do
      alpineWithLineBreak `shouldOsReleaseParseInto` OsInfo "alpine" "3.15.4"

    it "should not parse comments in release info " $ do
      alpineWithComment `shouldOsReleaseParseInto` OsInfo "alpine" "3.15.4"

    it "should parse busybox release info" $ do
      busyBoxContent `shouldBusyBoxParser` OsInfo "busybox" "1.34.1"

alpine :: Text
alpine =
  [r|NAME="Alpine Linux"
ID=alpine
VERSION_ID=3.15.4
PRETTY_NAME="Alpine Linux v3.15"
HOME_URL="https://alpinelinux.org/"
BUG_REPORT_URL="https://bugs.alpinelinux.org/"
|]

alpineWithComment :: Text
alpineWithComment =
  [r|NAME="Alpine Linux"
ID=alpine
# some comment
# VERSION_ID=0
VERSION_ID=3.15.4
PRETTY_NAME="Alpine Linux v3.15"
HOME_URL="https://alpinelinux.org/"
BUG_REPORT_URL="https://bugs.alpinelinux.org/"
|]

alpineWithLineBreak :: Text
alpineWithLineBreak =
  [r|NAME="Alpine Linux"
ID=alpine

VERSION_ID=3.15.4
PRETTY_NAME="Alpine Linux v3.15"
HOME_URL="https://alpinelinux.org/"
BUG_REPORT_URL="https://bugs.alpinelinux.org/"
|]

ubuntu :: Text
ubuntu =
  [r|NAME="Ubuntu"
VERSION="20.04 LTS (Focal Fossa)"
ID=ubuntu
ID_LIKE=debian
PRETTY_NAME="Ubuntu 20.04 LTS"
VERSION_ID="20.04"
HOME_URL="https://www.ubuntu.com/"
SUPPORT_URL="https://help.ubuntu.com/"
BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
PRIVACY_POLICY_URL="https://www.ubuntu.com/legal/terms-and-policies/privacy-policy"
VERSION_CODENAME=focal
UBUNTU_CODENAME=focal
|]

fedora :: Text
fedora =
  [r|NAME=Fedora
VERSION="31 (Container Image)"
ID=fedora
VERSION_ID=31
VERSION_CODENAME=""
PLATFORM_ID="platform:f31"
PRETTY_NAME="Fedora 31 (Container Image)"
ANSI_COLOR="0;34"
LOGO=fedora-logo-icon
CPE_NAME="cpe:/o:fedoraproject:fedora:31"
HOME_URL="https://fedoraproject.org/"
DOCUMENTATION_URL="https://docs.fedoraproject.org/en-US/fedora/f31/system-administrators-guide/"
SUPPORT_URL="https://fedoraproject.org/wiki/Communicating_and_getting_help"
BUG_REPORT_URL="https://bugzilla.redhat.com/"
REDHAT_BUGZILLA_PRODUCT="Fedora"
REDHAT_BUGZILLA_PRODUCT_VERSION=31
REDHAT_SUPPORT_PRODUCT="Fedora"
REDHAT_SUPPORT_PRODUCT_VERSION=31
PRIVACY_POLICY_URL="https://fedoraproject.org/wiki/Legal:PrivacyPolicy"
VARIANT="Container Image"
VARIANT_ID=container
|]

centOs :: Text
centOs =
  [r|NAME="CentOS Linux"
VERSION="8 (Core)"
ID="centos"
ID_LIKE="rhel fedora"
VERSION_ID="8"
PLATFORM_ID="platform:el8"
PRETTY_NAME="CentOS Linux 8 (Core)"
ANSI_COLOR="0;31"
CPE_NAME="cpe:/o:centos:centos:8"
HOME_URL="https://www.centos.org/"
BUG_REPORT_URL="https://bugs.centos.org/"

CENTOS_MANTISBT_PROJECT="CentOS-8"
CENTOS_MANTISBT_PROJECT_VERSION="8"
REDHAT_SUPPORT_PRODUCT="centos"
REDHAT_SUPPORT_PRODUCT_VERSION="8"
|]

centOs7 :: Text
centOs7 =
  [r|NAME="CentOS Linux"
VERSION="7 (Core)"
ID="centos"
ID_LIKE="rhel fedora"
VERSION_ID="7"
PRETTY_NAME="CentOS Linux 7 (Core)"
ANSI_COLOR="0;31"
CPE_NAME="cpe:/o:centos:centos:7"
HOME_URL="https://www.centos.org/"
BUG_REPORT_URL="https://bugs.centos.org/"

CENTOS_MANTISBT_PROJECT="CentOS-7"
CENTOS_MANTISBT_PROJECT_VERSION="7"
REDHAT_SUPPORT_PRODUCT="centos"
REDHAT_SUPPORT_PRODUCT_VERSION="7
|]

centOs6SystemReleaseCpe :: Text
centOs6SystemReleaseCpe = [r|cpe:/o:centos:linux:6:GA|]

oracleLinux :: Text
oracleLinux =
  [r|NAME="Oracle Linux Server"
VERSION="8.5"
ID="ol"
ID_LIKE="fedora"
VARIANT="Server"
VARIANT_ID="server"
VERSION_ID="8.5"
PLATFORM_ID="platform:el8"
PRETTY_NAME="Oracle Linux Server 8.5"
ANSI_COLOR="0;31"
CPE_NAME="cpe:/o:oracle:linux:8:5:server"
HOME_URL="https://linux.oracle.com/"
BUG_REPORT_URL="https://bugzilla.oracle.com/"

ORACLE_BUGZILLA_PRODUCT="Oracle Linux 8"
ORACLE_BUGZILLA_PRODUCT_VERSION=8.5
ORACLE_SUPPORT_PRODUCT="Oracle Linux"
ORACLE_SUPPORT_PRODUCT_VERSION=8.5
|]

amazon2Linux :: Text
amazon2Linux =
  [r|NAME="Amazon Linux"
VERSION="2"
ID="amzn"
ID_LIKE="centos rhel fedora"
VERSION_ID="2"
PRETTY_NAME="Amazon Linux 2"
ANSI_COLOR="0;33"
CPE_NAME="cpe:2.3:o:amazon:amazon_linux:2"
HOME_URL="https://amazonlinux.com/"
|]

openSuseLeap :: Text
openSuseLeap =
  [r|NAME="openSUSE Leap"
VERSION="15.4"
ID="opensuse-leap"
ID_LIKE="suse opensuse"
VERSION_ID="15.4"
PRETTY_NAME="openSUSE Leap 15.4"
ANSI_COLOR="0;32"
CPE_NAME="cpe:/o:opensuse:leap:15.4"
BUG_REPORT_URL="https://bugs.opensuse.org"
HOME_URL="https://www.opensuse.org/"
DOCUMENTATION_URL="https://en.opensuse.org/Portal:Leap"
LOGO="distributor-logo-Leap|]

busyBoxContent :: Text
busyBoxContent =
  [r|BusyBox is copyrighted by many authors between 1998-2015.
        BusyBox is a multi-call binary that combines many common Unix
        link to busybox for each function they wish to use and BusyBox
BusyBox v1.34.1 (2022-06-06 22:12:17 UTC)
syslogd started: BusyBox v1.34.1
|]
