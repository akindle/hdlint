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
import Parse.Modules
 
main :: IO ()
main = do
    handle <- openFile "hello.v" ReadMode
    contents <- hGetContents handle
    let parsed = parse parser "hello.v" contents
    -- let identifiers = filter (\a -> isDecl a || isLocalparam a) . concat
    -- let uniqueIdentifiers = nub . identifiers
    -- let conflicts a = identifiers a \\ uniqueIdentifiers a
    -- let allMatches a = identifiers a `intersect` conflicts a
    let unreferencedIdentifiers a = declaredIdentifiers a \\ referencedIdentifiers a
    let undeclaredIdentifiers a = nub (referencedIdentifiers a) \\ declaredIdentifiers a
    case parsed of
        Left err -> print err
        Right o -> print o
    hClose handle
    
declaredIdentifiers :: [VerilogThing] -> [Maybe Identifier]
declaredIdentifiers a = map getIdentifier $ filter isDecl a

referencedIdentifiers :: [VerilogThing] -> [Maybe Identifier]
referencedIdentifiers a = map getIdentifier $ filter isStatement a

isDecl (VDecl _) = True
isDecl _ = False

isStatement (VStatement _) = True
isStatement _ = False

parser :: Parser [VerilogThing]
parser = sc *> many vmod <* eof

things = try vdecl <|> vstatement

data VerilogThing = VStatement Statement
                    | VDecl Declaration 
                    | VMod VModule [VerilogThing]
                    deriving (Show, Eq)
instance GetIdentifier VerilogThing where
    getIdentifier (VStatement a) = getIdentifier a
    getIdentifier (VDecl a) = getIdentifier a
    getIdentifier (VMod a b) = getIdentifier a
    
vmod = 
    do
    a <- parseModule
    b <- many things
    _ <- rword "endmodule"
    return $ VMod a b
vdecl = wrap declaration VDecl
vstatement = wrap statement VStatement
--vmod = wrap parseModule VMod

  

commaSepStatements :: String -> Parser a -> Parser [a]
commaSepStatements a b = rword a *> b `sepBy` comma <* semicolon 
