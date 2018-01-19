module Lib
    ( 
    ) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue = B Bool 
               | S String
               | A [JSONValue]
               | O [(String, JSONValue)]
               deriving (Eq, Show)

ws :: Parser String
ws = many (oneOf " \t\n")

lexeme p = p <* ws


alwaysTrue :: Parser Bool
alwaysTrue = pure True

boolTrue :: Parser Bool
boolTrue = (lexeme $ string "true") *> alwaysTrue

boolFalse :: Parser Bool
boolFalse = (lexeme $ string "false") *> (pure False)

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser JSONValue
jsonBool = (lexeme (B <$> bool)) <* eof
-- we are mapping the data constructor over our value

stringLiteral :: Parser String
stringLiteral = 
  (lexeme $ char '"') *> (many (noneOf ['"'])) <* (lexeme $ char '"')

jsonString :: Parser JSONValue
jsonString = lexeme $ S <$> stringLiteral

-- the sequence operators <* and *> allow us to run operations and discard their
-- results. So here, aslong as we find opening and closing quote marks we just
-- return the other characters between them

array :: Parser [JSONValue]
array = (lexeme $ char '[') 
        *>
        ( jsonValue `sepBy` (lexeme $ char ',') ) 
        <*
        (lexeme $ char ']')
jsonArray :: Parser JSONValue
jsonArray = lexeme (A <$> array)

jsonValue :: Parser JSONValue
jsonValue = lexeme $ jsonBool <|> jsonString <|> jsonArray <|> jsonObject

objectEntry :: Parser (String, JSONValue)
objectEntry = do
  key <- stringLiteral
  lexeme $ char ':'
  value <- jsonValue
  return (key, value)

jsonObject = O <$> ((lexeme $ char '{')
                   *>
                   ( objectEntry `sepBy` (lexeme $ char ',') )
                   <* (lexeme $ char '}'))


object :: String 
object = "{\"somekey\":\"somevalue\",\"wooP\":true}"

main :: IO ()
main = do
  x <- parseFromFile jsonValue "src/data.txt"
  case x of
    Right (O x') -> do
      print $ lookup "name" x'
      print $ lookup "age" x'
