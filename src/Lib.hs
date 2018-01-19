module Lib
    ( matchTrue
    ) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue = B Bool 
               | S String
               | A [JSONValue]
               deriving (Eq, Show)


matchTrue :: Parser String
matchTrue = string "true"

alwaysTrue :: Parser Bool
alwaysTrue = pure True

boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue

boolFalse :: Parser Bool
boolFalse = (string "false") *> (pure False)

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser JSONValue
jsonBool = B <$> bool
-- we are mapping the data constructor over our value

stringLiteral :: Parser String
stringLiteral = 
  char '"' *> (many (noneOf ['"'])) <* char '"'

jsonString :: Parser JSONValue
jsonString = S <$> stringLiteral

-- the sequence operators <* and *> allow us to run operations and discard their
-- results. So here, aslong as we find opening and closing quote marks we just
-- return the other characters between them

array :: Parser [JSONValue]
array = (char '[') 
        *>
        ( jsonValue `sepBy` (char ',') ) 
        <*
        (char ']')
jsonArray :: Parser JSONValue
jsonArray = A <$> array

jsonValue :: Parser JSONValue
jsonValue = jsonBool <|> jsonString <|> jsonArray

main :: IO ()
main = do
print $
  parse jsonValue "test parser" "[\"heleo\",true]"
print $ parse jsonValue "test parser" "true"
print $ parse jsonValue "test parser" "\"heleo\""
print $
  parse jsonValue "test parser" "[\"heleo\",true,[true,false]]"

