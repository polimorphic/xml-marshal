{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Text.XML.Marshal
    ( FromXml, FromXmlList, ToXml, ToXmlList
    , contents, fromXml, fromXmlList, toXml, toXmlList
    , (..:), (.:)
    ) where

import Control.Monad ((<=<), (>=>))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Text.Read (readMaybe)
import Text.XML
    ( Element, Name, Node(NodeComment, NodeContent, NodeElement, NodeInstruction)
    , elementName, elementNodes
    )


class FromXml a where
    fromXml :: [Node] -> Either Text a

class FromXmlList a where
    fromXmlList :: [Node] -> Either Text [a]

class ToXml a where
    toXml :: a -> [Node]

class ToXmlList a where
    toXmlList :: [a] -> [Node]


instance FromXmlList a => FromXml [a] where
    fromXml = fromXmlList

instance ToXmlList a => ToXml [a] where
    toXml = toXmlList


instance FromXml Node where
    fromXml [] = Left "No nodes found"
    fromXml [n] = pure n
    fromXml _ = Left "More than one node found"

instance FromXmlList Node where
    fromXmlList = pure

instance ToXml Node where
    toXml = pure

instance ToXmlList Node where
    toXmlList = id


instance FromXml Element where
    fromXml = fromXml >=> \case
        [] -> Left "No elements found"
        [e] -> pure e
        _ -> Left "More than one element found"

instance FromXmlList Element where
    fromXmlList [] = pure []
    fromXmlList (NodeElement e : ns) = (e :) <$> fromXml ns
    fromXmlList (_ : ns) = fromXml ns

instance ToXml Element where
    toXml e = [NodeElement e]

instance ToXmlList Element where
    toXmlList = fmap NodeElement


instance FromXml Text where
    fromXml (NodeElement e : ns) = (<>) <$> fromXml (elementNodes e) <*> fromXml ns
    fromXml (NodeInstruction _ : ns) = fromXml ns
    fromXml (NodeContent t : ns) = (t <>) <$> fromXml ns
    fromXml (NodeComment _ : ns) = fromXml ns
    fromXml [] = pure ""

instance ToXml Text where
    toXml t = [NodeContent t]


instance FromXmlList Char where
    fromXmlList = fmap T.unpack . fromXml

instance ToXmlList Char where
    toXmlList = toXml . T.pack


instance FromXml Int where
    fromXml = maybeToEither "Invalid Int" . readMaybe <=< fromXml

instance ToXml Int where
    toXml = toXml . show


instance FromXml Day where
    fromXml = maybeToEither "Invalid Day" . parseTimeM True defaultTimeLocale "%Y-%m-%d" <=< fromXml

instance ToXml Day where
    toXml = toXml . formatTime defaultTimeLocale "%Y-%m-%d"


instance FromXml UTCTime where
    fromXml = maybeToEither "Invalid UTCTime"
            . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
          <=< fromXml

instance ToXml UTCTime where
    toXml = toXml . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"


contents :: FromXml a => Element -> Either Text a
contents = fromXml . elementNodes

(.:) :: FromXml a => Element -> Name -> Either Text a
e .: k = e ..: k >>= \case
    [] -> Left "No matching elements found"
    [x] -> pure x
    _ -> Left "More than one matching element found"

(..:) :: FromXml a => Element -> Name -> Either Text [a]
e ..: k = traverse (fromXml . toXml) . filter ((== k) . elementName) =<< contents e


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right
