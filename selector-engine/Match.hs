module Match
  ( parseSelector
  , parseElements
  , matchElement
  ) where

import Prelude hiding (words, unwords)
import Control.Applicative ((<|>), some)
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.List.NonEmpty (fromList)
import Data.Functor (($>), void)
import Data.Maybe (fromMaybe)

import Data.Text (Text, words, unwords, isPrefixOf, isInfixOf, isSuffixOf, breakOn)
import Text.Megaparsec (parse, errorBundlePretty)

import Element
    ( Zipper,
      Crumb(Crumb, siblingsBefore, siblingsAfter),
      Element(..),
      ElementData(EData, attributes, name),
      Attr,
      getAttrValue,
      toZipper,
      zipperFold,
      parent,
      gotoRoot, prevSibling )
import qualified Element
import Selector (Selector(..), Combinator(..))
import qualified Selector
import NExpr (NExpr(Constant), isN)
import Data.Bifunctor (Bifunctor(first))

mnot :: Maybe a -> Maybe ()
mnot Nothing = Just ()
mnot _       = Nothing

eqAttr :: Attr -> Attr -> Bool
eqAttr (nameA, Nothing) (nameB, _      ) = nameA == nameB
eqAttr _                (_    , Nothing) = False
eqAttr (nameA, Just valueA) (nameB, Just valueB) =
  nameA == nameB && valueA == valueB

matchAncestor :: Selector -> Zipper -> Maybe Element
matchAncestor selector zipper = do
  parentZipper <- parent zipper
  match selector parentZipper <|> matchAncestor selector parentZipper

matchPreceding :: Selector -> Zipper -> Maybe Element
matchPreceding selector zipper = do
  siblingZipper <- prevSibling zipper
  match selector siblingZipper <|> matchPreceding selector siblingZipper

match :: Selector -> Zipper -> Maybe Element
match Universal (element, _) = pure element

match (Type elName) (element@Element {content = EData {name}}, _) = do
  guard $ name == elName
  pure element

match (Class className) (element, _) = do
  classList <- getAttrValue "class" element
  guard $ elem className classList
  pure element

match (Id elemId) zipper = match (Attr "id" (Just elemId)) zipper

match (Attr name mValue) (element@Element {content = EData {attributes}}, _) = do
  guard $ any (eqAttr (name, words <$> mValue)) attributes
  pure element

match (AttrIncludes attrName str) (element, _) = do
  attrValue <- getAttrValue attrName element
  guard $ elem str attrValue
  pure element

match (AttrDashMatch attrName str) (element, _) = do
  attrValue <- getAttrValue attrName element
  guard $ fst (breakOn "-" (unwords attrValue)) == str
  pure element

match (AttrPrefixMatch attrName str) (element, _) = do
  attrValue <- getAttrValue attrName element
  guard $ str `isPrefixOf` unwords attrValue
  pure element

match (AttrSuffixMatch attrName str) (element, _) = do
  attrValue <- getAttrValue attrName element
  guard $ str `isSuffixOf` unwords attrValue
  pure element

match (AttrSubstrMatch attrName str) (element, _) = do
  attrValue <- getAttrValue attrName element
  guard $ str `isInfixOf` unwords attrValue
  pure element

match (Combined combinator selectorLeft selectorRight) zipper =
  case combinator of
    Child -> do
      parentZipper <- parent zipper
      match selectorLeft parentZipper *> match selectorRight zipper
    Descendant ->
      matchAncestor selectorLeft zipper *> match selectorRight zipper
    ImmediateSibling -> do
      siblingZipper <- prevSibling zipper
      match selectorLeft siblingZipper *> match selectorRight zipper
    Sibling ->
      matchPreceding selectorLeft zipper *> match selectorRight zipper

match (And selectors) zipper = foldl1 (*>) $ (`match` zipper) <$> selectors

match (Or selectors) zipper = asum $ (`match` zipper) <$> selectors

match (Not selector) zipper@(element, _) = mnot (match selector zipper) $> element

match PseudoClassLink zipper = match (Type "a") zipper

match PseudoClassVisited _ = Nothing
match PseudoClassHover _ = Nothing
match PseudoClassActive _ = Nothing
match PseudoClassFocus _ = Nothing
match PseudoClassTarget _ = Nothing

match (PseudoClassLang value) zipper@(element, _)
  =   match (AttrDashMatch "lang" value) zipper
  <|> (matchAncestor (AttrDashMatch "lang" value) zipper $> element)

match PseudoClassEnabled zipper@(element, _) =
  match isFormElement zipper *> mnot (match PseudoClassDisabled zipper) $> element
    where
      isFormElement = Or $ fromList
        [ Type "button"
        , Type "input"
        , Type "select"
        , Type "textarea"
        , Type "optgroup"
        , Type "option"
        , Type "fieldset"
        ]

{-
  https://html.spec.whatwg.org/multipage/semantics-other.html#concept-element-disabled
-}
match PseudoClassDisabled zipper@(element, _)
  =   match isDisableableElement zipper *> match hasDisabledAttr zipper
  <|> isDescendantOfADisabledFieldset *> isNotDescendantOfFirstLegend $> element
  <|> match isDisabledOptGroup zipper
  <|> match isOption zipper
      *> (match hasDisabledAttr zipper
      <|> isDescendantOfADisabledOptGroup $> element)
  <|> match isDisabledFieldset zipper
    where
      isDisableableElement = Or $ fromList
        [ Type "button"
        , Type "input"
        , Type "select"
        , Type "textarea"
        ]

      hasDisabledAttr = Attr "disabled" Nothing

      isDisabledFieldset = And $ fromList [Type "fieldset", hasDisabledAttr]

      isDescendantOfADisabledFieldset =
        matchAncestor isDisabledFieldset zipper

      isFirstLegend = And $ fromList [Type "legend", PseudoClassFirstChild]

      isNotDescendantOfFirstLegend =
        mnot $ matchAncestor isFirstLegend zipper

      isDisabledOptGroup = And $ fromList [Type "optgroup", hasDisabledAttr]

      isOption = Type "option"

      isDescendantOfADisabledOptGroup =
        matchAncestor isDisabledOptGroup zipper

{-
  https://html.spec.whatwg.org/multipage/semantics-other.html#selector-checked
-}
match PseudoClassChecked zipper
  =   match (And $ fromList [Type "input", Attr "type" (Just "checkbox"), Attr "checked" Nothing]) zipper
  <|> match (And $ fromList [Type "input", Attr "type" (Just "radio"), Attr "checked" Nothing]) zipper
  <|> match (And $ fromList [Type "option", Attr "selected" Nothing]) zipper

{-
  https://html.spec.whatwg.org/multipage/semantics-other.html#selector-indeterminate
-}
match PseudoClassIndeterminate zipper@(element, _)
  = formElement <|> progressElement
      where
        formElement = do
          void $ match (And $ fromList
            [ Type "input"
            , Attr "type" (Just "radio")
            , Not (Attr "checked" Nothing)
            ])
            zipper
          groupName <- getAttrValue "name" element
          let rootZipper = gotoRoot zipper
              radioGroup = matchTree
                (And $ fromList
                  [ Type "input"
                  , Attr "type" (Just "radio")
                  , Attr "name" (Just $ unwords groupName)
                  , Attr "checked" Nothing
                  ]
                )
                rootZipper
            in case radioGroup of
                 [] -> pure element
                 _  -> Nothing

        progressElement = match (And $ fromList [Type "progress", Not (Attr "value" Nothing)]) zipper

match PseudoClassRoot zipper = match (Type "html") zipper

{-
  https://www.w3.org/TR/2018/REC-selectors-3-20181106/#nth-child-pseudo
-}
match (PseudoClassNthChild _) (_, []) = Nothing
match (PseudoClassNthChild nExpr) (element, Crumb {siblingsBefore} : _) = do
  guard $ isN nExpr (length siblingsBefore + 1)
  pure element

match (PseudoClassNthLastChild _) (_, []) = Nothing
match (PseudoClassNthLastChild nExpr) (element, Crumb { siblingsAfter } : _) = do
  guard $ isN nExpr (length siblingsAfter + 1)
  pure element

match (PseudoClassNthOfType _) (_, []) = Nothing
match (PseudoClassNthOfType nExpr) (element@Element { content = EData { name } }, Crumb { siblingsBefore } : _) = do
  guard $ isN nExpr (length precedingOfTheSameType + 1)
  pure element
    where
      precedingOfTheSameType = filter
        (\Element { content = EData { name = siblingName } } -> siblingName == name)
        siblingsBefore

match (PseudoClassNthLastOfType _) (_, []) = Nothing
match (PseudoClassNthLastOfType nExpr) (element@Element { content = EData { name } }, Crumb { siblingsAfter } : _) = do
  guard $ isN nExpr (length precedingOfTheSameType + 1)
  pure element
    where
      precedingOfTheSameType = filter
        (\Element { content = EData { name = siblingName } } -> siblingName == name)
        siblingsAfter

match PseudoClassFirstChild zipper = match (PseudoClassNthChild (Constant 1)) zipper
match PseudoClassLastChild zipper = match (PseudoClassNthLastChild (Constant 1)) zipper
match PseudoClassFirstOfType zipper = match (PseudoClassNthOfType (Constant 1)) zipper
match PseudoClassLastOfType zipper = match (PseudoClassNthLastOfType (Constant 1)) zipper

match PseudoClassOnlyChild zipper =
  match (And $ fromList [PseudoClassFirstChild, PseudoClassLastChild]) zipper

match PseudoClassOnlyOfType zipper =
  match (And $ fromList [PseudoClassFirstOfType, PseudoClassLastOfType]) zipper

match PseudoClassEmpty (element@Element { children }, _) = do
  guard $ null children
  pure element

matchTree :: Selector -> Zipper -> [Element]
matchTree selector =
  fromMaybe [] . zipperFold (\xs zipper -> ((:) <$> match selector zipper <*> xs) <|> xs) (Just [])

parseElements :: Text -> Either String [Element]
parseElements =  first errorBundlePretty . parse (some Element.element) ""

parseSelector :: Text -> Either String Selector
parseSelector = first errorBundlePretty . parse Selector.selectors_group ""

matchElement :: Element -> Selector -> [Element]
matchElement element selector = matchTree selector $ toZipper element
