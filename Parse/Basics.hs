module Parse.Basics where

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

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
    where   lineComment = L.skipLineComment"//"
            blockComment = L.skipBlockComment "/*" "*/"

lexeme     = L.lexeme sc
symbol     = L.symbol sc

parens     = between (symbol "(") (symbol ")")
brackets   = between (symbol "[") (symbol "]")
angles     = between (symbol "{") (symbol "}") 
semicolon  = symbol ";"
comma      = symbol ","
colon      = symbol ":" 
eq         = symbol "="
concEq     = symbol "<="
idChar     = alphaNumChar <|> char '_' <|> char '$'
idHeadChar = letterChar <|> char '_'

wrap a b = a >>= \c -> return $ b c

-- parsed data types know their location. for a given element, their location is the start of the element
-- for now, we are only handling unescaped identifiers. escaped identifiers are some horrifying trash
data Identifier = Identifier String SourcePos deriving (Show, Eq)
identifier :: Parser Identifier
identifier =  lexeme (p >>= check)
    where p         =
                    do 
                        location <- getPosition
                        head <- idHeadChar
                        body <- many idChar                        
                        sc
                        return $ Identifier (head : body) location
          check (Identifier x y)   = if x `elem` reservedWords
                      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                      else return $ Identifier x y

rword :: String -> Parser ()
rword w = string w *> notFollowedBy idChar *> sc

reservedWords :: [String]
reservedWords = ["localparam", "param", "parameter", "begin", "if", "else", "end", "always", "module", "endmodule", "input", "output", "inout", "wire", "reg", "integer"]
