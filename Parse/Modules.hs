module Parse.Modules where

import System.Environment
import System.IO
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Parse.Basics
import Parse.Expressions
import Parse.Declarations
import Parse.Statements

data VModule = Module Identifier [Declaration] deriving (Show, Eq)
instance GetIdentifier VModule where
    getIdentifier (Module a _) = Just a
     
parseModule = do
    _ <- rword "module" <|> rword "macromodule"
    name <- identifier
    decls <- parens $ declaration `sepBy` comma
    _ <- semicolon
    return $ Module name decls