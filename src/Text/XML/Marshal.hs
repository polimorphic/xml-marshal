{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Text.XML.Marshal
    ( FromXmlDocument, FromXmlFragment, ListFromXmlFragment
    , ListToXmlFragment, ToXmlDocument, ToXmlFragment
    , contents, fromXmlDocument, fromXmlFragment, getAttribute
    , listFromXmlFragment, listToXmlFragment, lookupAttribute
    , toXmlDocument, toXmlFragment
    , (..:), (.:), (.:?)
    ) where

import Control.Monad ((<=<), (>=>))
import Data.Map ((!?))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Text.Read (readMaybe)
import Text.XML
    ( Document, Element, Name, Node(NodeComment, NodeContent, NodeElement, NodeInstruction)
    , elementAttributes, elementName, elementNodes
    )

class FromXmlDocument a where
    fromXmlDocument :: Document -> Either Text a

class FromXmlFragment a where
    fromXmlFragment :: [Node] -> Either Text a

class ListFromXmlFragment a where
    listFromXmlFragment :: [Node] -> Either Text [a]

class ListToXmlFragment a where
    listToXmlFragment :: [a] -> [Node]

class ToXmlDocument a where
    toXmlDocument :: a -> Document

class ToXmlFragment a where
    toXmlFragment :: a -> [Node]


instance ListFromXmlFragment a => FromXmlFragment [a] where
    fromXmlFragment = listFromXmlFragment

instance ListToXmlFragment a => ToXmlFragment [a] where
    toXmlFragment = listToXmlFragment


instance FromXmlFragment Node where
    fromXmlFragment [] = Left "No nodes found"
    fromXmlFragment [n] = pure n
    fromXmlFragment _ = Left "More than one node found"

instance ListFromXmlFragment Node where
    listFromXmlFragment = pure

instance ToXmlFragment Node where
    toXmlFragment = pure

instance ListToXmlFragment Node where
    listToXmlFragment = id


instance FromXmlFragment Element where
    fromXmlFragment = fromXmlFragment >=> \case
        [] -> Left "No elements found"
        [e] -> pure e
        _ -> Left "More than one element found"

instance ListFromXmlFragment Element where
    listFromXmlFragment [] = pure []
    listFromXmlFragment (NodeElement e : ns) = (e :) <$> fromXmlFragment ns
    listFromXmlFragment (_ : ns) = fromXmlFragment ns

instance ToXmlFragment Element where
    toXmlFragment e = [NodeElement e]

instance ListToXmlFragment Element where
    listToXmlFragment = fmap NodeElement


instance FromXmlFragment Text where
    fromXmlFragment (NodeElement e : ns) = (<>) <$> fromXmlFragment (elementNodes e) <*> fromXmlFragment ns
    fromXmlFragment (NodeInstruction _ : ns) = fromXmlFragment ns
    fromXmlFragment (NodeContent t : ns) = (t <>) <$> fromXmlFragment ns
    fromXmlFragment (NodeComment _ : ns) = fromXmlFragment ns
    fromXmlFragment [] = pure ""

instance ToXmlFragment Text where
    toXmlFragment t = [NodeContent t]


instance ListFromXmlFragment Char where
    listFromXmlFragment = fmap T.unpack . fromXmlFragment

instance ListToXmlFragment Char where
    listToXmlFragment = toXmlFragment . T.pack


instance FromXmlFragment Int where
    fromXmlFragment = maybeToEither "Invalid Int" . readMaybe <=< fromXmlFragment

instance ToXmlFragment Int where
    toXmlFragment = toXmlFragment . show


instance FromXmlFragment Day where
    fromXmlFragment = maybeToEither "Invalid Day"
                    . parseTimeM True defaultTimeLocale "%Y-%m-%d"
                  <=< fromXmlFragment

instance ToXmlFragment Day where
    toXmlFragment = toXmlFragment . formatTime defaultTimeLocale "%Y-%m-%d"


instance FromXmlFragment UTCTime where
    fromXmlFragment = maybeToEither "Invalid UTCTime"
                    . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                  <=< fromXmlFragment

instance ToXmlFragment UTCTime where
    toXmlFragment = toXmlFragment . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"


contents :: FromXmlFragment a => Element -> Either Text a
contents = fromXmlFragment . elementNodes

(.:) :: FromXmlFragment a => Element -> Name -> Either Text a
e .: k = e .:? k >>= \case
    Nothing -> Left "No matching elements found"
    Just x -> pure x

(.:?) :: FromXmlFragment a => Element -> Name -> Either Text (Maybe a)
e .:? k = e ..: k >>= \case
    [] -> pure Nothing
    [x] -> pure $ Just x
    _ -> Left "More than one matching element found"

(..:) :: FromXmlFragment a => Element -> Name -> Either Text [a]
e ..: k = traverse (fromXmlFragment . toXmlFragment) . filter ((== k) . elementName) =<< contents e

getAttribute :: Name -> Element -> Either Text Text
getAttribute n e = maybe (Left "No matching attribute found") pure $ lookupAttribute n e

lookupAttribute :: Name -> Element -> Maybe Text
lookupAttribute n e = elementAttributes e !? n


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right
