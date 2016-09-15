module Parse.Basics where

import System.Environment
import System.IO
import Data.Char
import Data.Maybe
import Data.List
import Data.Either
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
    where   lineComment = L.skipLineComment "//"
            blockComment = L.skipBlockComment "/*" "*/"
            
withRecovery' a = withRecovery recover (Right <$> a) 
    where recover err = Left err <$ skippable

lexeme     = L.lexeme sc
symbol     = L.symbol sc

parens     = between (symbol "(") (symbol ")")
brackets   = between (symbol "[") (symbol "]")
angles     = between (symbol "{") (symbol "}") 
quotes     = between (symbol "\"") (symbol "\"")
-- semicolon  = withRecovery recover (Right <$> symbol ";")
--         where recover err = Left err <$ skippable
semicolon = symbol ";"

skippable :: Parser ()   
skippable =
    do a <- manyTill anyChar eol
       return ()
            
comma      = symbol ","
colon      = symbol ":" 
eq         = symbol "="
concEq     = symbol "<="
at         = symbol "@"
idChar     = alphaNumChar <|> char '_' <|> char '$'
idHeadChar = letterChar <|> char '_'

wrap :: Monad m => m a -> (a -> r) -> m r
wrap a b = a >>= \c -> return $ b c

wrapStatement :: Monad m => m a -> m [a]
wrapStatement a = a >>= \b -> return [b]


-- parsed data types know their location. for a given element, their location is the start of the element
-- for now, we are only handling unescaped identifiers. escaped identifiers are some horrifying trash
data Identifier = Identifier String SourcePos
                | CompositeIdentifier [Identifier] SourcePos deriving (Show)
instance Eq Identifier where
    (Identifier a _) == (Identifier b _) = a == b
    (CompositeIdentifier a _) == (CompositeIdentifier b _) = a == b

class GetIdentifiers a where
    getIdentifiers :: a -> [Identifier]
    getIdentifierDeclarations :: a -> [Identifier]
    getIdentifierUtilizations :: a -> [Identifier]

identifier :: Parser Identifier
identifier = try identifier' <|> try composite

idName :: Parser String
idName = do
        x <- idHeadChar
        xs <- many idChar
        let name = x:xs
        _ <- if name `elem` reservedWords then unexpected ("keyword " ++ show name ++ " cannot be an identifier") else sc
        return $ x:xs

composite :: Parser Identifier
composite =
    do
        location <- getPosition
        a <-angles (identifier `sepBy1` comma)
        sc
        return $ CompositeIdentifier a location

identifier' :: Parser Identifier
identifier' =  
    do 
        location <- getPosition
        name <- try idName
        sc
        return $ Identifier (name) location


rword :: String -> Parser ()
rword w = string w *> notFollowedBy idChar *> sc

reservedWords :: [String]
reservedWords = ["localparam", "param", "parameter", "begin", "if", "else", "end", "always", "module", "endmodule", "input", "output", "inout", "wire", "reg", "integer", "assign", "default"]
