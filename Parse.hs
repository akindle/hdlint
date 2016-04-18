module Parse where

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
 
main :: IO ()
main = do
    handle <- openFile "hello.v" ReadMode
    contents <- hGetContents handle
    let parsed = parse parser "hello.v" contents
    -- let identifiers = filter (\a -> isDecl a || isLocalparam a) . concat
    -- let uniqueIdentifiers = nub . identifiers
    -- let conflicts a = identifiers a \\ uniqueIdentifiers a
    -- let allMatches a = identifiers a `intersect` conflicts a
    case parsed of
        Left err -> print err
        Right o -> print o
    hClose handle

parser :: Parser [VerilogThing]
parser = sc *> many things <* eof

things = vdecl <|> vstatement

data VerilogThing = VStatement Statement
                    | VDecl Declaration
                    deriving (Show, Eq)
instance GetIdentifier VerilogThing where
    getIdentifier (VStatement a) = getIdentifier a
    getIdentifier (VDecl a) = getIdentifier a
    
vdecl = wrap declaration VDecl
vstatement = wrap statement VStatement

  

commaSepStatements :: String -> Parser a -> Parser [a]
commaSepStatements a b = rword a *> b `sepBy` comma <* semicolon 