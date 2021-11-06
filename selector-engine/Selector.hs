module Selector
  ( Selector(..)
  , Combinator(..)
  , upto
  , countRange
  , charClass
  , inClass
  , notInClass
  , nonascii
  , unicode
  , escape
  , nmchar
  , name
  , nmstart
  , ident
  , num
  , nl
  , string1
  , string2
  , string_
  , w
  , not_
  , function
  , plus
  , greater
  , comma
  , tilde
  , dimension
  , namespace_prefix
  , element_name
  , type_selector
  , universal
  , class_
  , id_
  , attrIncludes
  , attrDashMatch
  , attrPrefixMatch
  , attrSuffixMatch
  , attrSubstrMatch
  , attrEquals
  , fromAttrOp
  , attrib
  , negation_arg
  , parenExpr
  , pseudoClass
  , negation
  , simple_selector_sequence
  , combinator
  , selector
  , selectors_group
  ) where

import Data.Text (Text, singleton, pack)
import Data.Char (isAscii, isAlphaNum)
import Data.Foldable (fold, Foldable (foldl'))
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..), some1)

import Text.Megaparsec
import Text.Megaparsec.Char

import Parsing (Parser, tryOrEmpty, n, o, t)
import NExpr (NExpr, nExpr)

data Selector
    = Universal
    | Type Text
    | Class Text
    | Id Text
    | Attr Text (Maybe Text)
    -- "~="             return INCLUDES;
    -- "|="             return DASHMATCH;
    -- "^="             return PREFIXMATCH;
    -- "$="             return SUFFIXMATCH;
    -- "*="             return SUBSTRINGMATCH;
    | AttrIncludes Text Text
    | AttrDashMatch Text Text
    | AttrPrefixMatch Text Text
    | AttrSuffixMatch Text Text
    | AttrSubstrMatch Text Text
    | Combined Combinator Selector Selector
    | And (NonEmpty Selector)
    | Or (NonEmpty Selector)
    -- pseudo-classes
    | Not Selector -- can't be nested
    | PseudoClassLink
    | PseudoClassVisited
    | PseudoClassHover
    | PseudoClassActive
    | PseudoClassFocus
    | PseudoClassTarget
    | PseudoClassLang Text
    | PseudoClassEnabled
    | PseudoClassDisabled
    | PseudoClassChecked
    | PseudoClassIndeterminate
    | PseudoClassRoot
    | PseudoClassNthChild NExpr
    | PseudoClassNthLastChild NExpr
    | PseudoClassNthOfType NExpr
    | PseudoClassNthLastOfType NExpr
    | PseudoClassFirstChild
    | PseudoClassLastChild
    | PseudoClassFirstOfType
    | PseudoClassLastOfType
    | PseudoClassOnlyChild
    | PseudoClassOnlyOfType
    | PseudoClassEmpty
    deriving stock Show

{- https://www.w3.org/TR/2018/REC-selectors-3-20181106/#w3cselgrammar -}
{-
  %option case-insensitive

  ident     [-]?{nmstart}{nmchar}*
  name      {nmchar}+
  nmstart   [_a-z]|{nonascii}|{escape}
  nonascii  [^\0-\177]
  unicode   \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
  escape    {unicode}|\\[^\n\r\f0-9a-f]
  nmchar    [_a-z0-9-]|{nonascii}|{escape}
  num       [0-9]+|[0-9]*\.[0-9]+
  string    {string1}|{string2}
  string1   \"([^\n\r\f\\"]|\\{nl}|{nonascii}|{escape})*\"
  string2   \'([^\n\r\f\\']|\\{nl}|{nonascii}|{escape})*\'
  invalid   {invalid1}|{invalid2}
  invalid1  \"([^\n\r\f\\"]|\\{nl}|{nonascii}|{escape})*
  invalid2  \'([^\n\r\f\\']|\\{nl}|{nonascii}|{escape})*
  nl        \n|\r\n|\r|\f
  w         [ \t\r\n\f]*

  D         d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
  E         e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
  N         n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
  O         o|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
  T         t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
  V         v|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\v

  %%

  [ \t\r\n\f]+     return S;

  "~="             return INCLUDES;
  "|="             return DASHMATCH;
  "^="             return PREFIXMATCH;
  "$="             return SUFFIXMATCH;
  "*="             return SUBSTRINGMATCH;
  {ident}          return IDENT;
  {string}         return STRING;
  {ident}"("       return FUNCTION;
  {num}            return NUMBER;
  "#"{name}        return HASH;
  {w}"+"           return PLUS;
  {w}">"           return GREATER;
  {w}","           return COMMA;
  {w}"~"           return TILDE;
  ":"{N}{O}{T}"("  return NOT;
  @{ident}         return ATKEYWORD;
  {invalid}        return INVALID;
  {num}%           return PERCENTAGE;
  {num}{ident}     return DIMENSION;
  "<!--"           return CDO;
  "-->"            return CDC;

  \/\*[^*]*\*+([^/*][^*]*\*+)*\/                    /* ignore comments */

  .                return *yytext;
 -}

upto :: Int -> Parser a -> Parser [a]
upto _n p | _n > 0 = (:) <$> try p <*> upto (_n - 1) p <|> pure []
upto _ _           = pure []

countRange :: Int -> Int -> Parser a -> Parser [a]
countRange _n m p = count _n p <> upto (m - _n) p

charClass :: String -> String
charClass = go
  where
    go (a : '-' : b : xs) = [a .. b] ++ go xs
    go (x           : xs) = x : go xs
    go _                  = ""

inClass :: String -> Char -> Bool
inClass classString = (`elem` charClass classString)

notInClass :: String -> Char -> Bool
notInClass classString = (`notElem` charClass classString)

-- nonascii  [^\0-\177]
nonascii :: Parser Char
nonascii = satisfy (not . isAscii)

-- unicode   \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
unicode :: Parser Text
unicode
  =  "\\"
  <> (pack <$> countRange 1 6 (satisfy isAlphaNum))
  <> tryOrEmpty ("\r\n" <> (singleton <$> spaceChar))

-- escape    {unicode}|\\[^\n\r\f0-9a-f]
escape :: Parser Text
escape
  =   try unicode
  <|> "\\" <> (singleton <$> satisfy (notInClass "\n\r\f0-9a-fA-F"))

-- nmchar    [_a-z0-9-]|{nonascii}|{escape}
nmchar :: Parser Text
nmchar
  =   try (singleton <$> satisfy (inClass "_a-zA-Z0-9-"))
  <|> try (singleton <$> nonascii)
  <|> escape

-- name      {nmchar}+
name :: Parser Text
name = fold <$> some nmchar

-- nmstart   [_a-z]|{nonascii}|{escape}
nmstart :: Parser Text
nmstart
  =   try (singleton <$> satisfy (inClass "_a-zA-Z"))
  <|> try (singleton <$> nonascii)
  <|> escape

 -- ident     [-]?{nmstart}{nmchar}*
ident :: Parser Text
ident
  =  tryOrEmpty "-"
  <> nmstart
  <> (fold <$> many nmchar)

-- num       [0-9]+|[0-9]*\.[0-9]+
num :: Parser Text
num
  = pack <$> digits
    where
      digits
        =   try (many digitChar <> (pure <$> char '.') <> some digitChar)
        <|> some digitChar

-- nl        \n|\r\n|\r|\f
nl :: Parser Text
nl
  =   try "\n"
  <|> try "\r\n"
  <|> try "\r"
  <|> "\f"

--  string1   \"([^\n\r\f\\"]|\\{nl}|{nonascii}|{escape})*\"
string1 :: Parser Text
string1 = between "\"" "\"" (fold <$> many characters)
            where
              characters
                =   try (singleton <$> satisfy (notInClass "\n\r\f\""))
                <|> try ("\\" <> nl)
                <|> try (singleton <$> nonascii)
                <|> escape

--   string2   \'([^\n\r\f\\']|\\{nl}|{nonascii}|{escape})*\'
string2 :: Parser Text
string2 = between "'" "'" (fold <$> many characters)
            where
              characters
                =   try (singleton <$> satisfy (notInClass "\n\r\f'"))
                <|> try ("\\" <> nl)
                <|> try (singleton <$> nonascii)
                <|> escape

-- string    {string1}|{string2}
string_ :: Parser Text
string_ = try string1 <|> string2

-- w         [ \t\r\n\f]*
w :: Parser Text
w = fold <$> many (singleton <$> satisfy (inClass " \t\r\n\f"))

-- ":"{N}{O}{T}"("  return NOT;
not_ :: Parser Text
not_ = ":" <> n <> o <> t <> "("

-- {ident}"("       return FUNCTION;
function :: Parser Text
function = ident <> "("

-- {w}"+"           return PLUS;
-- {w}">"           return GREATER;
-- {w}","           return COMMA;
-- {w}"~"           return TILDE;

plus :: Parser Text
plus = w <> "+"

greater :: Parser Text
greater = w <> ">"

comma :: Parser Text
comma = w <> ","

tilde :: Parser Text
tilde = w <> "~"

-- {num}{ident}     return DIMENSION;
dimension :: Parser Text
dimension = num <> ident

-- -----------------------------------------
{-
  namespace_prefix
    : [ IDENT | '*' ]? '|'
    ;
-}
namespace_prefix :: Parser Text
namespace_prefix =
  tryOrEmpty (try ident <|> "*") <> "|"

element_name :: Parser Text
element_name = ident

{-
  type_selector
    : [ namespace_prefix ]? element_name
    ;
-}
type_selector :: Parser Selector
type_selector
  = tryOrEmpty namespace_prefix
  >> Type <$> element_name

{-
  universal
    : [ namespace_prefix ]? '*'
    ;
-}
universal :: Parser Selector
universal
  = tryOrEmpty namespace_prefix
  >> "*"
  >> pure Universal

{-
  class
    : '.' IDENT
    ;
-}
class_ :: Parser Selector
class_ = "." >> Class <$> ident

-- "#"{name}        return HASH;
id_ :: Parser Selector
id_ = "#" >> Id <$> name

-- "~="             return INCLUDES;
-- "|="             return DASHMATCH;
-- "^="             return PREFIXMATCH;
-- "$="             return SUFFIXMATCH;
-- "*="             return SUBSTRINGMATCH;

attrIncludes :: Text
attrIncludes = "~="

attrDashMatch :: Text
attrDashMatch = "|="

attrPrefixMatch :: Text
attrPrefixMatch = "^="

attrSuffixMatch :: Text
attrSuffixMatch = "$="

attrSubstrMatch :: Text
attrSubstrMatch = "*="

attrEquals :: Text
attrEquals = "="

fromAttrOp :: Text -> Text -> Text -> Selector
fromAttrOp attrName op value
  | op == attrIncludes    = AttrIncludes attrName value
  | op == attrDashMatch   = AttrDashMatch attrName value
  | op == attrPrefixMatch = AttrPrefixMatch attrName value
  | op == attrSuffixMatch = AttrSuffixMatch attrName value
  | op == attrSubstrMatch = AttrSubstrMatch attrName value
  | op == attrEquals      = Attr attrName (Just value)
  | otherwise             = Attr attrName (Just value)

{-
  attrib
    : '[' S* [ namespace_prefix ]? IDENT S*
          [ [ PREFIXMATCH |
              SUFFIXMATCH |
              SUBSTRINGMATCH |
              '=' |
              INCLUDES |
              DASHMATCH ] S* [ IDENT | STRING ] S*
          ]? ']'
    ;
-}

attrib :: Parser Selector
attrib = do
  void "["
  space
  attrName <- ident
  space
  mAttrValue <- optional $ do
      operator <- try (string attrPrefixMatch)
                  <|> try (string attrEquals)
                  <|> try (string attrSuffixMatch)
                  <|> try (string attrSubstrMatch)
                  <|> try (string attrIncludes)
                  <|> string attrDashMatch
      space
      value <- try ident <|> string_
      space
      pure (operator, value)
  void "]"
  pure $ case mAttrValue of
    Nothing                -> Attr attrName Nothing
    Just (operator, value) -> fromAttrOp attrName operator value

{-
  negation_arg
    : type_selector | universal | HASH | class | attrib | pseudo
    ;
-}
negation_arg :: Parser Selector
negation_arg
  =   try type_selector
  <|> try universal
  <|> try id_
  <|> try class_
  <|> try attrib
  <|> pseudoClass

parenExpr :: Parser a -> Parser a
parenExpr = between "(" ")"

pseudoClass :: Parser Selector
pseudoClass = do
  void ":"
  try (      "link"             >> pure PseudoClassLink)
    <|> try ("visited"          >> pure PseudoClassVisited)
    <|> try ("hover"            >> pure PseudoClassHover)
    <|> try ("active"           >> pure PseudoClassActive)
    <|> try ("focus"            >> pure PseudoClassFocus)
    <|> try ("target"           >> pure PseudoClassTarget)
    <|> try ("lang"             >> space >> PseudoClassLang <$> (parenExpr ident))
    <|> try ("enabled"          >> pure PseudoClassEnabled)
    <|> try ("disabled"         >> pure PseudoClassDisabled)
    <|> try ("checked"          >> pure PseudoClassChecked)
    <|> try ("indeterminate"    >> pure PseudoClassIndeterminate)
    <|> try ("root"             >> pure PseudoClassRoot)
    <|> try ("nth-child"        >> space >> PseudoClassNthChild      <$> (parenExpr nExpr))
    <|> try ("nth-last-child"   >> space >> PseudoClassNthLastChild  <$> (parenExpr nExpr))
    <|> try ("nth-of-type"      >> space >> PseudoClassNthOfType     <$> (parenExpr nExpr))
    <|> try ("nth-last-of-type" >> space >> PseudoClassNthLastOfType <$> (parenExpr nExpr))
    <|> try ("first-child"      >> pure PseudoClassFirstChild)
    <|> try ("last-child"       >> pure PseudoClassLastChild)
    <|> try ("first-of-type"    >> pure PseudoClassFirstOfType)
    <|> try ("last-of-type"     >> pure PseudoClassLastOfType)
    <|> try ("only-child"       >> pure PseudoClassOnlyChild)
    <|> try ("only-of-type"     >> pure PseudoClassOnlyOfType)
    <|>     ("empty"            >> pure PseudoClassEmpty)

{-
  negation
    : NOT S* negation_arg S* ')'
    ;
-}
negation :: Parser Selector
negation = do
  void not_
  space
  _selector <- negation_arg
  space
  void ")"
  pure $ Not _selector

{-
  simple_selector_sequence
    : [ type_selector | universal ]
      [ HASH | class | attrib | pseudo | negation ]*
    | [ HASH | class | attrib | pseudo | negation ]+
    ;
-}
simple_selector_sequence :: Parser Selector
simple_selector_sequence =
  try withBase <|> standalones
    where
      withBase = do
        baseSelector <- try type_selector <|> universal
        more <- many standalone
        pure $ case more of
          []        -> baseSelector
          selectors -> And $ baseSelector :| selectors

      standalones = do
        several <- some1 standalone
        pure $ case several of
          _selector :| [] -> _selector
          selectors -> And selectors

      standalone
        =   try id_
        <|> try class_
        <|> try attrib
        <|> try pseudoClass
        <|> negation

data Combinator
  = ImmediateSibling
  | Child
  | Sibling
  | Descendant
  deriving stock Show

{-
  combinator
    /* combinators can be surrounded by whitespace */
    : PLUS S* | GREATER S* | TILDE S* | S+
    ;
-}
combinator :: Parser Combinator
combinator
  =   (try (plus    <* space) >> pure ImmediateSibling)
  <|> (try (greater <* space) >> pure Child)
  <|> (try (tilde   <* space) >> pure Sibling)
  <|>      (some spaceChar    >> pure Descendant)

{-
  selector
    : simple_selector_sequence [ combinator simple_selector_sequence ]*
  ;
-}
selector :: Parser Selector
selector = do
  first <- simple_selector_sequence
  more <- many ((,) <$> combinator <*> simple_selector_sequence)
  pure $ case more of
    []    -> first
    pairs -> combined first
                where
                  {- magic lambda trick! -}
                  combined = foldl' (\sf (c, sprev) snext -> Combined c (sf snext) sprev) id pairs


-- selectors_group
--   : selector [ COMMA S* selector ]*
--   ;
selectors_group :: Parser Selector
selectors_group = do
  first <- selector
  more <- many $ comma >> space >> selector
  pure $ case more of
    []        -> first
    selectors -> Or $ first :| selectors


-- >>> parse selector "" "* * * *"
-- Right (Combined Descendant Universal (Combined Descendant Universal (Combined Descendant Universal Universal)))

-- >>> parse selector "" "a+b+c+d"
-- Right (Combined ImmediateSibling (Combined ImmediateSibling (Combined ImmediateSibling (Type "a") (Type "b")) (Type "c")) (Type "d"))
