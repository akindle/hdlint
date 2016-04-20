module Main where

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

import Parse.Basics
import Parse.Expressions
import Parse.Declarations
import Parse.Statements
import Parse.Modules
 
main :: IO ()
main = do
    args <- getArgs
    let openRead a = openFile a ReadMode 
    handles <- mapM openRead args
    contents <- mapM hGetContents handles
    --a <- map processFile args
    let pairs = zip args contents
    let parsed = uncurry (parse parser)
    let parses = mapM parsed pairs
    let unreferencedIdentifiers a = declaredIdentifiers a \\ referencedIdentifiers a
    let undeclaredIdentifiers a = nub (referencedIdentifiers a) \\ declaredIdentifiers a
    _ <- mapM_ print parses
    print "hello"          
    
declaredIdentifiers :: [VerilogThing] -> [Identifier]
declaredIdentifiers a = concatMap getIdentifiers $ filter isDecl a

referencedIdentifiers :: [VerilogThing] -> [Identifier]
referencedIdentifiers a = concatMap getIdentifiers $ filter isStatement a

isDecl (VDecl _) = True
isDecl _ = False

isStatement (VStatement _) = True
isStatement _ = False

type Recoverable = [Either ParseError VerilogThing] 
parser :: Parser Recoverable
parser = sc *> many (withRecovery' vmod) <* eof 

simplify' a = case parse parser "" a of
            Left _ -> fail "oops"
            Right c -> c

simplify a = head $ simplify' a


data VerilogThing = VStatement Statement
                    | VDecl Declaration 
                    | VMod VModule [RecoverableThing] 
                    | VPreprocessor
                    deriving (Show, Eq)


instance GetIdentifiers VerilogThing where
    getIdentifiers (VStatement a) = getIdentifiers a
    getIdentifiers (VDecl a) = getIdentifiers a
    getIdentifiers (VMod a b) = getIdentifiers a ++ concatMap getIdentifiers (rights b)

    getIdentifierDeclarations (VStatement a) = getIdentifierDeclarations a
    getIdentifierDeclarations (VDecl a) = getIdentifierDeclarations a
    getIdentifierDeclarations (VMod a b) = getIdentifierDeclarations a ++ concatMap getIdentifierDeclarations (rights b)

    getIdentifierUtilizations (VStatement a) = getIdentifierUtilizations a
    getIdentifierUtilizations (VDecl a) = getIdentifierUtilizations a
    getIdentifierUtilizations (VMod a b) = getIdentifierUtilizations a ++ concatMap getIdentifierUtilizations (rights b)
    
type RecoverableThing = Either ParseError VerilogThing
things :: Parser RecoverableThing
things = withRecovery' (vdecl <|> vstatement <|> vpreprocessor)

unrecoverableThings :: Parser VerilogThing
unrecoverableThings = vdecl <|> vstatement <|> vpreprocessor

vmod :: Parser VerilogThing
vmod = 
    do
    _ <- sc
    a <- parseModule
    b <- manyTill things (rword "endmodule") 
    _ <- sc
    return $ VMod a b
    
vdecl = wrap declaration VDecl
vstatement = wrap statement VStatement 

vpreprocessor =
    do
        _ <- symbol "`"
        _ <- manyTill anyChar eol
        return VPreprocessor
  

commaSepStatements :: String -> Parser a -> Parser [a]
commaSepStatements a b = rword a *> b `sepBy` comma <* semicolon 
