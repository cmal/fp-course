{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.JsonParser where

import Course.Core
import Course.Parser
import Course.MoreParser
import Course.JsonValue
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
import qualified System.IO as IO

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
fromSpecialCharacter ::
  SpecialCharacter
  -> Char
fromSpecialCharacter BackSpace =
  chr 0x08
fromSpecialCharacter FormFeed =
  chr 0x0C
fromSpecialCharacter NewLine =
  '\n'
fromSpecialCharacter CarriageReturn =
  '\r'
fromSpecialCharacter Tab =
  '\t'
fromSpecialCharacter VerticalTab =
  '\v'
fromSpecialCharacter SingleQuote =
  '\''
fromSpecialCharacter DoubleQuote =
  '"'
fromSpecialCharacter Backslash =
  '\\'

-- NOTE: This is not inverse to @fromSpecialCharacter@.
toSpecialCharacter ::
  Char
  -> Optional SpecialCharacter
toSpecialCharacter c =
  let table = ('b', BackSpace) :.
              ('f', FormFeed) :.
              ('n', NewLine) :.
              ('r', CarriageReturn) :.
              ('t', Tab) :.
              ('v', VerticalTab) :.
              ('\'', SingleQuote) :.
              ('"' , DoubleQuote) :.
              ('\\', Backslash) :.
              Nil
  in snd <$> find ((==) c . fst) table

-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal characters. See http://json.org for the full list of control characters in JSON.
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
jsonString ::
  Parser Chars
jsonString =
  -- error "todo: Course.JsonParser#jsonString"
  between quote quote (list (hexOrSpecialCharParser ||| (noneof $ listh "\\\"")))

backSlashParser :: Parser Char
backSlashParser = is '\\'

specialCharParser :: Parser Char
specialCharParser = P $ \input -> case input of
                                    Nil -> UnexpectedEof
                                    (x:.xs) -> case toSpecialCharacter x of
                                                 Empty -> UnexpectedEof
                                                 Full c -> Result xs $ fromSpecialCharacter c

hexOrSpecialCharParser :: Parser Char
-- hexOrSpecialCharParser = backSlashParser >>= (const $ pure id <*> (hexu ||| specialCharParser))
hexOrSpecialCharParser = backSlashParser *> (hexu ||| specialCharParser)

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
jsonNumber ::
  Parser Rational
jsonNumber = P $ \input -> case readFloats input of
                             Empty -> UnexpectedString input
                             Full (x, y) -> Result y x
  -- error "todo: Course.JsonParser#jsonNumber"



-- | Parse a JSON true literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue ::
  Parser Chars
jsonTrue = string "true"
  -- error "todo: Course.JsonParser#jsonTrue"

-- | Parse a JSON false literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse ::
  Parser Chars
jsonFalse = string "false"
  -- error "todo: Course.JsonParser#jsonFalse"

-- | Parse a JSON null literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull ::
  Parser Chars
jsonNull = string "null"
  -- error "todo: Course.JsonParser#jsonNull"

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
jsonArray ::
  Parser (List JsonValue)
jsonArray = betweenSepbyComma '[' ']' jsonValue
  -- error "todo: Course.JsonParser#jsonArray"

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
jsonObject ::
  Parser Assoc
jsonObject = betweenSepbyComma '{' '}' jsonKv
  -- error "todo: Course.JsonParser#jsonObject"

-- type Assoc = List (Chars, JsonValue)

jsonKv :: Parser (Chars, JsonValue)
jsonKv = (spaces *> jsonString <* spaces) >>= (\k -> pure (\v -> (k, v)) <*> ((is ':') *> jsonValue <* spaces))

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
jsonValue ::
  Parser JsonValue
jsonValue = spaces *> (jsonNull1 |||
                       jsonTrue1 |||
                       jsonFalse1 |||
                       jsonString1 |||
                       jsonArray1 |||
                       jsonObject1 |||
                       jsonNumber1 |||
                       jsonString1)
             <* spaces
   -- error "todo: Course.JsonParser#jsonValue"

(<*>~~) ::
  Parser (a -> b)
  -> Parser a
  -> Parser b
(<*>~~) f a =
  f <*> spaces *> a



jsonNull1 :: Parser JsonValue
jsonNull1 = (pure $ const JsonNull) <*>~~ jsonNull

jsonTrue1 :: Parser JsonValue
jsonTrue1 = (pure $ const JsonTrue) <*>~~ jsonTrue

jsonFalse1 :: Parser JsonValue
jsonFalse1 = (pure $ const JsonFalse) <*>~~ jsonFalse

jsonArray1 :: Parser JsonValue
jsonArray1 = (pure $ JsonArray) <*>~~ jsonArray

jsonString1 :: Parser JsonValue
jsonString1 = (pure $ JsonString) <*>~~ jsonString

jsonObject1 :: Parser JsonValue
jsonObject1 = (pure $ JsonObject) <*>~~ jsonObject

jsonNumber1 :: Parser JsonValue
jsonNumber1 = (pure $ JsonRational) <*>~~ jsonNumber


-- | Read a file into a JSON value.
--
-- /Tip:/ Use @System.IO#readFile@ and `jsonValue`.
readJsonValue ::
  FilePath
  -> IO (ParseResult JsonValue)
readJsonValue path = parse jsonValue <$> readFile path
  -- error "todo: Course.JsonParser#readJsonValue"
