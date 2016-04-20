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

data VModule = Module Identifier [Declaration] [Declaration] -- module's name, module parameters (optional), module ports
               | DangerousModule Identifier [Declaration] [Identifier] deriving (Show, Eq) -- really half-hearted support for what im calling deprecated syntax
instance GetIdentifiers VModule where
    getIdentifiers (Module a b c) = [a] ++ concatMap getIdentifiers b ++ concatMap getIdentifiers c
    getIdentifiers (DangerousModule a b c) = [a] ++ c ++ concatMap getIdentifiers b
    
    getIdentifierDeclarations (Module a b c) = [a] ++ concatMap getIdentifierDeclarations b ++ concatMap getIdentifierDeclarations c
    getIdentifierDeclarations (DangerousModule a b c) = [a] ++ c ++ concatMap getIdentifierDeclarations b
    
    getIdentifierUtilizations (Module a b c) = [] ++ concatMap getIdentifierUtilizations b ++ concatMap getIdentifierUtilizations c
    getIdentifierUtilizations (DangerousModule a b c) = [] ++ concatMap getIdentifierUtilizations b
    

parseModule = try parseModule' <|> parseDangerousModule

parseDangerousModule = do
    _ <- rword "module" <|> rword "macromodule"
    name <- identifier
    p <- optional moduleParameters
    idents <- parens $ identifier `sepBy` comma
    _ <- semicolon
    let unpackedParams = fromMaybe [] p
    return $ DangerousModule name unpackedParams idents
     
parseModule' = do
    _ <- rword "module" <|> rword "macromodule"
    name <- identifier
    p <- optional moduleParameters
    decls <- parens $ declaration `sepBy` comma
    _ <- semicolon
    let unpackedParams = fromMaybe [] p
    return $ Module name unpackedParams decls
    
moduleParameters = do
    _ <- symbol "#"
    parens $ declaration `sepBy` comma