{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This modules provides marshalling facilities from XML to datatypes
--
-- It's modeled closely after Aeson's @FromJSON@, with a couple of differences.
--
-- Aeson's @(.:)@ is replaced by three parsing primitives:
--
-- - 'attr', which parses string attributes of an element
-- - 'child', which looks for the first child element with a given name
-- - 'children', which looks for all child elements with a given name
--
-- Use 'parseXML' to parse from a strict Text to your specific datatype
--
-- === Optionality
--
-- In Aeson, we use @(.:?) :: FromJSON a => Object -> Text -> Parser (Maybe a)@
-- to allow for elements that might not exist. In 'FromXML' parsers, we use a
-- slightly different pattern with 'optional' from 'Alternative':
--
-- > data Foo = Foo Text
-- >
-- > instance FromXML Foo where
-- >   parseElement el = do
-- >     maybeBaz <- optional (child "baz" el)
-- >     case maybeBaz of
-- >       Nothing -> pure (Foo "defaultbaz")
-- >       Just baz -> pure (Foo baz)
--
-- Similar to Aeson, which has @(.:!) :: Parser (Maybe a) -> a -> Parser a@ as
-- a helper for defaults, we have 'defaultsTo' which is meant to be used infix:
--
-- > instance FromXML Foo where
-- >   parseElement el = do
-- >     baz <- optional (child "baz" el) `defaultsTo` "defaultbaz"
-- >     pure (Foo baz)
--
-- Or with Functor/Applicative syntax:
--
-- > instance FromXML Foo where
-- >   parseElement el =
-- >     Foo <$> optional (child "baz" el) `defaultsTo` "defaultbaz"
module Parse.XML (
  -- * Types
  FromXML (..),
  Parser (..),
  parseXML,

  -- * Parsing an XML element
  attr,
  child,
  children,
  content,

  -- * Helper functions
  defaultsTo,

  -- * Error formatting
  ParseError (..),
  ParsePath,
  xmlErrorPretty,
) where

import Prelude

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.NonDet.Church
import Control.Carrier.Reader
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Text.XML.Light qualified as XML

-- | A type that can be converted from XML
--
-- This is largely modeled after Aeson's FromJSON class, and has similar
-- semantics. XML is a little more flexible (everything is modeled as Elements),
-- so the failure modes are a little less precise.
class FromXML a where
  parseElement :: XML.Element -> Parser a

instance FromXML XML.Element where
  parseElement = pure

instance FromXML T.Text where
  parseElement = content

instance FromXML v => FromXML (M.Map T.Text v) where
  parseElement el = M.fromList <$> traverse mkSingle (XML.elChildren el)
    where
      mkSingle e = do
        let key :: T.Text
            key = T.pack (XML.qName (XML.elName e))
        value <- parseElement e
        pure (key, value)

newtype Parser a = Parser {unParser :: ReaderC ParsePath (ErrorC ParseError Identity) a}
  deriving (Functor, Applicative, Monad)

instance Alternative Parser where
  ma <|> mb = Parser (unParser ma `catchError` (\(_ :: ParseError) -> unParser mb))
  empty = Parser $ ask >>= \path -> throwError (UnknownError path "Parser.empty")

instance MonadFail Parser where
  fail msg = Parser $ ask >>= \path -> throwError (UnknownError path msg)

runParser :: String -> Parser a -> Either ParseError a
runParser rootName = run . runError . runReader [rootName] . unParser

data ParseError
  = -- | A 'child' element was missing
    ParseElementMissing ParsePath String
  | -- | An 'attr' was missing
    ParseAttrMissing ParsePath String
  | -- | The input 'T.Text' didn't contain valid XML
    ParseXMLDocFailed
  | -- | A custom error, likely invoked via 'fail'
    UnknownError ParsePath String
  deriving (Eq, Ord, Show)

-- | Pretty-print a ParseError into strict 'T.Text'
xmlErrorPretty :: ParseError -> T.Text
xmlErrorPretty = \case
  ParseElementMissing path childName -> "Missing child at " <> renderPath path <> "; childName: " <> T.pack childName
  ParseAttrMissing path attrName -> "Missing attribute at " <> renderPath path <> "; attrName: " <> T.pack attrName
  UnknownError path err -> "UnknownError at " <> renderPath path <> "; err: " <> T.pack err
  ParseXMLDocFailed -> "parseXMLDoc failed"

-- | Render a ParsePath as 'T.Text'
renderPath :: ParsePath -> T.Text
renderPath [] = "[no path]"
renderPath xs = (\path -> "[" <> path <> "]") . T.intercalate "." . map T.pack . reverse $ xs

-- | Reversed parse path. As we parse into elements, we /prepend/ them to this list
type ParsePath = [String]

-- | Parse a 'FromXML' value from a strict 'T.Text'
parseXML :: FromXML a => T.Text -> Either ParseError a
parseXML inp =
  case XML.parseXMLDoc inp of
    Nothing -> Left ParseXMLDocFailed
    Just root -> runParser (XML.qName (XML.elName root)) (parseElement root)

-- | Parse an attribute from an XML Element. This will fail if the attribute
-- does not exist
--
-- For example, given the following as @el@:
--
-- > <myelement attrOne="foo" attrTwo="bar"></myelement>
--
-- this would succeed:
--
-- > (,) <$> attr "attrOne" el <*> attr "attrTwo" el
attr :: String -> XML.Element -> Parser T.Text
attr attrName el =
  case XML.findAttrBy (\elName -> XML.qName elName == attrName) el of
    Nothing -> Parser $ ask >>= \path -> throwError (ParseAttrMissing path attrName)
    Just a -> pure (T.pack a)

-- | Find a child of an XML Element by its name. This will fail if a child with
-- the given name does not exist
--
-- For example, given the following as @el@:
--
-- > <foo><bar></bar></foo>
--
-- @child "bar" el@ would succeed.
--
-- If there are multiple children with the same name, this returns the first
child :: FromXML a => String -> XML.Element -> Parser a
child childName el =
  case XML.filterChildName (\elName -> XML.qName elName == childName) el of
    Nothing -> Parser $ ask >>= \path -> throwError (ParseElementMissing path childName)
    Just a -> subparse childName a

-- | Find all children of an XML element with a given name. __This never
-- fails__, and will return an empty list if no elements are found.
--
-- For example, given the following as @el@:
--
-- > <foo><bar></bar><bar></bar></foo>
--
-- @children "bar" el@ would produce a two-element list.
-- @children "baz" el@ would produce an empty list.
children :: FromXML a => String -> XML.Element -> Parser [a]
children name = traverse (subparse name) . XML.filterChildrenName (\elName -> XML.qName elName == name)

-- | Get the string content from an XML Element
--
-- For example, given the following as @el@:
--
-- > <foo>bar baz quux</foo>
--
-- @content el@ would produce "bar baz quux"
content :: XML.Element -> Parser T.Text
content = pure . T.pack . XML.strContent

-- | Helper function to default an optional field to a specific value
--
-- This is meant to be used in conjunction with 'optional' from 'Alternative'
--
-- For example, given this datatype and parser:
--
-- > data Foo = Foo Text
-- >
-- > instance FromXML Foo where
-- >   parseElement el = Foo <$> optional (child "bar" el >>= content) `defaultsTo` "baz"
--
-- If the "bar" child exists, we produce the content of that element. Otherwise,
-- we produce "baz"
defaultsTo :: Parser (Maybe a) -> a -> Parser a
defaultsTo fa a = fmap (fromMaybe a) fa

-- parse a child element, and add its name to the parse path
subparse :: FromXML a => String -> XML.Element -> Parser a
subparse path el = Parser $ local (path :) (unParser (parseElement el))
