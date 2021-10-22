module System.CGroup.Types (
  -- * CGroup Controllers
  Controller (..),
  resolveCGroupController,
  resolveCGroupController',

  -- * CGroups
  CGroup (..),

  -- * Mounts
  Mount (..),

  -- * Exported for testing
  findMatchingCGroup,
  resolveControllerMountPath,
  tryResolveMount,
  parseMountInfo,
  parseCGroups,
) where

import Control.Exception (throwIO)
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Foldable (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Path
import Text.Megaparsec (Parsec, eof, manyTill, optional, parse, skipMany, some, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

-- | A CGroup controller path for a specific subsystem
newtype Controller a = Controller {unController :: Path Abs Dir}
  deriving (Eq, Ord, Show)

-- | Resolve a CGroup controller's filepath, as viewed by the current process
--
-- see cgroups(7): /proc/self/cgroup is a file that contains information about
-- control groups applied to this process
--
-- see proc(5): /proc/self/mountinfo is a file that contains information about
-- mounts available to this process
--
-- Because these aren't valid paths on Windows, we have to parse them into @Path Abs File@ at runtime
resolveCGroupController :: Text -> IO (Controller a)
resolveCGroupController controller = do
  cgroupPath <- parseAbsFile "/proc/self/cgroup"
  mountinfoPath <- parseAbsFile "/proc/self/mountinfo"
  resolveCGroupController' cgroupPath mountinfoPath controller

-- | Resolve a CGroup controller's filepath, under the given cgroup and mountinfo paths
resolveCGroupController' :: Path Abs File -> Path Abs File -> Text -> IO (Controller a)
resolveCGroupController' cgroupPath mountinfoPath controllerName = do
  cgroups <- parseFile parseCGroups cgroupPath
  mounts <- parseFile parseMountInfo mountinfoPath
  cgroup <- maybe (fail "Couldn't find cgroup for controller") pure (findMatchingCGroup controllerName cgroups)
  resolved <- maybe (fail "Couldn't find mount for cgroup") pure (resolveControllerMountPath controllerName cgroup mounts)

  pure (Controller resolved)

-- | Parse a file
parseFile :: Parser a -> Path b File -> IO a
parseFile parser file = either throwIO pure . parse parser (toFilePath file) =<< TIO.readFile (toFilePath file)

-- | Find a CGroup matching a controller name
--
-- For cgroups version 1, we use @containsController@ to explicitly look for the controller within a cgroup
--
-- For cgroups version 2, we use @emptyControllers@ to find a cgroup without any controllers
--
-- see cgroups(7): /proc/[pid]/cgroup section
findMatchingCGroup :: Text -> [CGroup] -> Maybe CGroup
findMatchingCGroup controllerName = find (\group -> containsController group || emptyControllers group)
  where
    containsController :: CGroup -> Bool
    containsController = (controllerName `elem`) . controlGroupControllers

    emptyControllers :: CGroup -> Bool
    emptyControllers = null . controlGroupControllers

-- | Find a Mount matching a controller name and cgroup, returning the absolute
-- resolved path of a controller
resolveControllerMountPath :: Text -> CGroup -> [Mount] -> Maybe (Path Abs Dir)
resolveControllerMountPath controllerName cgroup = firstMaybe (tryResolveMount controllerName cgroup)

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f = listToMaybe . mapMaybe f

-- | Attempt to match a cgroup controller to a mount, returning the absolute
-- resolved path of the controller
--
-- Returns Nothing if the mount does not match the cgroup controller
--
-- A matching mount must have a filesystem type of "cgroup" and contain the
-- controller name within its "super options".
--
-- Per cgroups(7), the cgroup path is relative to a mount root in the process's
-- mount hierarchy. Notably, a mount root /is not the same as its mount point/.
-- A mount point is the path at which the mount is visible to the process.
--
-- As such, we need to look for a mount whose mount root either..
--
-- - ..exactly matches our cgroup's path, in which case we directly return the
--   mount's mount path; OR
--
-- - ..is a prefix of our cgroup's path, in which case we return the relative
--   path from the mount root appended to the mount's mount path
tryResolveMount :: Text -> CGroup -> Mount -> Maybe (Path Abs Dir)
tryResolveMount controllerName cgroup mount = do
  guard ("cgroup" == mountFilesystemType mount)
  guard (controllerName `elem` mountSuperOptions mount)
  if controlGroupPath cgroup == mountRoot mount
    then Just (mountPoint mount)
    else do
      rel <- stripProperPrefix (mountRoot mount) (controlGroupPath cgroup)
      Just (mountPoint mount </> rel)

-----

-- | A cgroup, as viewed within /proc/[pid]/cgroup
--
-- see cgroups(7): /proc/[pid]/cgroup section
data CGroup = CGroup
  { controlGroupControllers :: [Text]
  , controlGroupPath :: Path Abs Dir
  }
  deriving (Show)

-- | Parse an entire /proc/[pid]/cgroup file into a list of cgroups
parseCGroups :: Parser [CGroup]
parseCGroups = some parseSingleCGroup <* eof

-- | Parse a single cgroup line within /proc/[pid]/cgroup
--
-- hierarchyID:list,of,controllers:path
--
-- In cgroups version 1, a comma-separated list of controllers exists within each group
--
-- In cgroups version 2, the "controllers" section is always an empty string
--
-- see cgroups(7): /proc/[pid]/cgroup section
parseSingleCGroup :: Parser CGroup
parseSingleCGroup =
  CGroup
    <$ takeUntil1P ':' -- ignore hierarchy ID number
    <*> (splitOnIgnoreEmpty "," <$> takeUntilP ':') -- comma-separated list of controllers
    <*> (parseIntoAbsDir =<< takeUntil1P '\n') -- path

-- return the prefix of the input until reaching the supplied character.
-- the character is also consumed as part of this parser.
--
-- this parser succeeds even when the character does not exist in the input
takeUntilP :: Char -> Parser Text
takeUntilP c = takeWhileP Nothing (/= c) <* optional (char c)

-- like 'takeUntilP', but expects a non-empty prefix before the character
takeUntil1P :: Char -> Parser Text
takeUntil1P c = takeWhile1P Nothing (/= c) <* optional (char c)

-- Data.Text.splitOn, but returns empty list on empty haystack, rather than [""]
--
-- >>> Data.Text.splitOn "foo" ""
-- [""]
--
-- >>> splitOnIgnoreEmpty "foo" ""
-- []
splitOnIgnoreEmpty :: Text -> Text -> [Text]
splitOnIgnoreEmpty _ "" = []
splitOnIgnoreEmpty s str = Text.splitOn s str

--------------

-- | A mount, as viewed within /proc/[pid]/mountinfo
--
-- see proc(5): /proc/[pid]/mountinfo section
data Mount = Mount
  { mountId :: Text
  , mountParentId :: Text
  , mountStDev :: Text
  , mountRoot :: Path Abs Dir
  , mountPoint :: Path Abs Dir
  , mountOptions :: Text
  , mountTags :: [Text]
  , mountFilesystemType :: Text
  , mountSource :: Text
  , mountSuperOptions :: [Text]
  }
  deriving (Show)

-- | Parse an entire /proc/[pid]/mountinfo file into a list of mounts
parseMountInfo :: Parser [Mount]
parseMountInfo = some parseSingleMount <* eof

-- | Parse a single mount line within /proc/[pid]/mountinfo
--
-- Fields are space-separated
--
-- see proc(5): /proc/[pid]/mountinfo section
parseSingleMount :: Parser Mount
parseSingleMount =
  Mount
    <$> field -- id
    <*> field -- parent id
    <*> field -- st_dev
    <*> (parseIntoAbsDir =<< field) -- mount root
    <*> (parseIntoAbsDir =<< field) -- mount point
    <*> field -- mount options
    <*> field `manyTill` separator -- optional mount tags, terminated by "-"
    <*> field -- filesystem type
    <*> field -- mount source
    <*> (splitOnIgnoreEmpty "," <$> field) -- super options
    <* optional (char '\n')

type Parser = Parsec Void Text

-- a field in the mountinfo file, terminated by whitespace
field :: Parser Text
field = lexeme $ takeWhile1P Nothing (not . isSpace)

-- separator after optional mount tags ("-")
separator :: Parser Char
separator = lexeme $ char '-'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (skipMany (char ' '))

parseIntoAbsDir :: Text -> Parser (Path Abs Dir)
parseIntoAbsDir = either (fail . show) pure . parseAbsDir . toString
