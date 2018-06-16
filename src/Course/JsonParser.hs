{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.JsonParser where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.JsonValue
import           Course.List
import           Course.Monad
import           Course.MoreParser
import           Course.Optional
import           Course.Parser

-- $setup
-- >>> :set -XOverloadedStrings

-- A special character is one of the following:
-- * \b  Backspace (ascii code 08)
-- * \f  Form feed (ascii code 0C)
-- * \n  New line
-- * \r  Carriage return
-- * \t  Tab
-- * \v  Vertical tab
-- * \'  Apostrophe or single quote (only valid in single quoted json strings)
-- * \"  Double quote (only valid in double quoted json strings)
-- * \\  Backslash character
data SpecialCharacter =
  BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving (Eq, Ord, Show)

-- NOTE: This is not inverse to @toSpecialCharacter@.
fromSpecialCharacter :: SpecialCharacter -> Char
fromSpecialCharacter BackSpace      = chr 0x08
fromSpecialCharacter FormFeed       = chr 0x0C
fromSpecialCharacter NewLine        = '\n'
fromSpecialCharacter CarriageReturn = '\r'
fromSpecialCharacter Tab            = '\t'
fromSpecialCharacter VerticalTab    = '\v'
fromSpecialCharacter SingleQuote    = '\''
fromSpecialCharacter DoubleQuote    = '"'
fromSpecialCharacter Backslash      = '\\'

-- NOTE: This is not inverse to @fromSpecialCharacter@.
toSpecialCharacter :: Char -> Optional SpecialCharacter
toSpecialCharacter c =
  let table = ('b'  , BackSpace)      :.
              ('f'  , FormFeed)       :.
              ('n'  , NewLine)        :.
              ('r'  , CarriageReturn) :.
              ('t'  , Tab)            :.
              ('v'  , VerticalTab)    :.
              ('\'' , SingleQuote)    :.
              ('"'  , DoubleQuote)    :.
              ('\\' , Backslash)      :.
              Nil
  in snd <$> find ((==) c . fst) table

-- | Parse a JSON string. Handle double-quotes, special characters,
--   hexadecimal characters. See http://json.org for the full list
--   of control characters in JSON.
--
-- /Tip:/ Use `hex`, `fromSpecialCharacter`, `between`, `is`, `charTok`, `toSpecialCharacter`.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString :: Parser Chars
jsonString = between (is '"') (charTok '"') (list others)
  where
    others = character >>= \c ->
      case c of
        '\\' ->
          character >>= \case
          d | d == 'u'  -> hex
            | otherwise -> case toSpecialCharacter d of
                Full x -> return $ fromSpecialCharacter x
                Empty  -> unexpectedCharParser d
        '"'  -> unexpectedCharParser c
        _    -> return c

-- | Parse a JSON rational.
--
-- /Tip:/ Use @readFloats@.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber :: Parser Rational
jsonNumber = list character >>= \s ->
  case readFloats s of
    Empty       -> constantParser $ UnexpectedString s
    Full (x, r) -> constantParser $ Result r x

-- | Parse a JSON true literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue :: Parser Chars
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse :: Parser Chars
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull :: Parser Chars
jsonNull = stringTok "null"

-- | Parse a JSON array.
--
-- /Tip:/ Use `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray :: Parser (List JsonValue)
jsonArray = betweenSepbyComma '[' ']' jsonValue

-- | Parse a JSON object.
--
-- /Tip:/ Use `jsonString`, `charTok`, `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject :: Parser Assoc  -- Parser (List (Chars, JsonValue))
jsonObject = betweenSepbyComma '{' '}' attributes
  where
    attributes :: Parser (Chars, JsonValue)
    attributes = (,) <$> jsonString <* charTok ':' <*> jsonValue
    -- attributes = do
    --   jstring <- jsonString <* charTok ':'
    --   jvalue  <- jsonValue
    --   return (jstring, jvalue)

-- | Parse a JSON value.
--
-- /Tip:/ Use `spaces`, `jsonNull`, `jsonTrue`, `jsonFalse`, `jsonArray`, `jsonString`, `jsonObject` and `jsonNumber`.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue :: Parser JsonValue
jsonValue = spaces *> values
  where
    values = JsonNull     <$  jsonNull
         ||| JsonTrue     <$  jsonTrue
         ||| JsonFalse    <$  jsonFalse
         ||| JsonArray    <$> jsonArray
         ||| JsonString   <$> jsonString
         ||| JsonObject   <$> jsonObject
         ||| JsonRational <$> jsonNumber

-- | Read a file into a JSON value.
--
-- /Tip:/ Use @System.IO#readFile@ and `jsonValue`.
readJsonValue :: FilePath -> IO (ParseResult JsonValue)
readJsonValue fp = do
  contents <- readFile fp
  return $ parse jsonValue contents
