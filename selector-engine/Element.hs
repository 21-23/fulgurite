module Element
  ( Element(..)
  , ElementData(..)
  , Crumb(..)
  , Zipper
  , Attr
  , element
  , getAttrValue
  , toZipper
  , getElement
  , firstChild
  , nextSibling
  , prevSibling
  , zipperFold
  , zipperFoldM
  , parent
  , gotoRoot
  ) where

import Prelude hiding (words)
import Data.Char (isAlpha, isAscii)
import Data.Functor (void)
import Control.Monad (join)

import Data.Text (Text, pack, words)
import Text.Megaparsec
import Text.Megaparsec.Char

import Parsing (Parser)
import Data.Either (rights)

type Attr = (Text, Maybe [Text])

data ElementData = EData
  { name :: Text
  , attributes :: [Attr]
  } deriving stock Show

data Element = Element
  { content    :: ElementData
  , children   :: [Element]
  } deriving stock Show

attribute :: Parser Attr
attribute = do
  name <- pack <$> some (satisfy isAlpha <|> char '-')
  mValue <- optional $ do
    void "="
    value <- pack <$> (try (some (satisfy isAlpha)) <|> "\"" *> manyTill (satisfy isAscii) "\"")
    pure $ words value
  pure (name, mValue)

element :: Parser Element
element = do
  void "<"
  name <- pack <$> some (satisfy isAlpha)
  space
  attrs <- many (try attribute <* space)
  let eData = EData name attrs
      singleTag = do
        void "/"
        space
        void ">"
        pure $ Element eData []
      twoTags = do
        void ">"
        children <- many $ try $
          (Left <$> some (anySingleBut '<')) <|> (Right <$> element)
        void "</"
        void $ string name
        void ">"
        pure $ Element eData (rights children)
  try singleTag <|> twoTags

getAttrValue :: Text -> Element -> Maybe [Text]
getAttrValue attrName Element { content = EData { attributes } } =
  join $ lookup attrName attributes

data Crumb = Crumb
  { elemContent    :: ElementData
  , siblingsBefore :: [Element]
  , siblingsAfter  :: [Element]
  } deriving stock Show

type Zipper = (Element, [Crumb])

toZipper :: Element -> Zipper
toZipper _element = (_element, [])

getElement :: Zipper -> Element
getElement = fst

firstChild :: Zipper -> Maybe Zipper
firstChild (Element { children = [] }, _) = Nothing
firstChild (Element { content, children = first : cs }, crumbs) =
  Just (first, Crumb content [] cs : crumbs)

nextSibling :: Zipper -> Maybe Zipper
nextSibling (_, []              ) = Nothing
nextSibling (_, Crumb _ _ [] : _) = Nothing
nextSibling (e, (Crumb content ls (e' : rs)) : crumbs) =
  Just (e', Crumb content (e : ls) rs : crumbs)

prevSibling :: Zipper -> Maybe Zipper
prevSibling (_, []) = Nothing
prevSibling (_, Crumb _ [] _ : _) = Nothing
prevSibling (e, (Crumb content (e' : ls) rs): crumbs) =
  Just (e', Crumb content ls (e : rs) : crumbs)

zipperFold :: (a -> Zipper -> a) -> a -> Zipper -> a
zipperFold f z zipper = go child sibling
  where
    go Nothing  Nothing  = z'
    go Nothing  (Just s) = zipperFold f z' s
    go (Just c) Nothing  = zipperFold f z' c
    go (Just c) (Just s) = zipperFold f (zipperFold f z' c) s
    child                = firstChild zipper
    sibling              = nextSibling zipper
    z'                   = f z zipper

zipperFoldM :: Monad m => (a -> Zipper -> m a) -> a -> Zipper -> m a
zipperFoldM f z = zipperFold f' (pure z)
  where
    f' ma zipper = ma >>= flip f zipper

parent :: Zipper -> Maybe Zipper
parent (_, []) = Nothing
parent (_element, Crumb content ls rs : crumbs) =
  Just (Element content (reverse ls ++ _element : rs), crumbs)

gotoRoot :: Zipper -> Zipper
gotoRoot zipper = maybe zipper gotoRoot $ parent zipper
