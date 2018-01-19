module Lib
    ( matchTrue
    ) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

matchTrue :: Parser String
matchTrue = string "true"

