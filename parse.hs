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

 
main :: IO ()
main = do
    handle <- openFile "hello.v" ReadMode
    contents <- hGetContents handle
    let parsed = parseCSV "hello.v" contents
    let identifiers = filter (\a -> isDecl a || isLocalparam a) . concat
    let uniqueIdentifiers = nub . identifiers
    let conflicts a = identifiers a \\ uniqueIdentifiers a
    let allMatches a = identifiers a `intersect` conflicts a
    case parsed of
        Left err -> print err
        Right o -> print . allMatches $ o
    hClose handle


getIdentifier :: Statement -> Identifier
getIdentifier (Decl (Reg a _)) = a
getIdentifier (Decl (Wire a _)) = a

getRight :: Either b [[Statement]] -> [[Statement]]
getRight = either (const [[Ignore]]) id
 

parseCSV :: String -> String -> Either ParseError [[Statement]]
parseCSV = parse parser 

data PortConnection = In Connection
                    | Out Connection
                    | Inout Connection
                    deriving (Show, Eq)

data Assign = Concurrent Identifier AExpression
            | Blocking Identifier AExpression
            deriving (Eq)
instance Show Assign where
    show (Concurrent a b) = show a ++ " <= " ++ show b ++ ";"
    show (Blocking a b) = show a ++ " = " ++ show b ++ ";"

data Connection = Reg Identifier Range
                | Wire Identifier Range
                deriving (Eq)
instance Show Connection where
    show (Reg name range) = "reg " ++ show range ++ " " ++ show name ++ ";"
    show (Wire name range) = "wire " ++ show range ++ " " ++ show name ++ ";"

data Statement = Seq [Statement] 
                | Port Identifier [PortConnection]
                | Localparam Identifier VerilogNumeric
                | Decl Connection
                | Assignment Assign
                | Ignore
instance Show Statement where
    show (Seq [x]) = "hi"
    show (Port a b) = "module " ++ show a ++ "(" ++ show b ++ ");"
    show (Localparam name number) = "localparam " ++ show name ++ " = " ++ show number ++ ";"
    show (Decl a) = show a
    show (Assignment a) = show a
    show _ = ""
instance Eq Statement where
    Decl a == Decl b = duplicateConn a b
    Port a _ == Port b _ = a == b
    Localparam a _ == Localparam b _ = a == b
    Localparam a _ == Decl (Reg b _) = a == b
    Localparam a _ == Decl (Wire b _) = a == b
    Decl (Reg b _) == Localparam a _ = a == b
    Decl (Wire b _) == Localparam a _ = a == b
    _ == _ = False

duplicateConn :: Connection -> Connection -> Bool
duplicateConn (Reg a _) (Reg b _) = a == b
duplicateConn (Reg a _) (Wire b _) = a == b
duplicateConn (Wire a _) (Reg b _) = a == b
duplicateConn (Wire a _) (Wire b _) = a == b


duplicateDecl :: Statement -> Statement -> Bool
duplicateDecl (Decl a) (Decl b) = a == b
duplicateDecl _ _ = False

isDecl :: Statement -> Bool
isDecl (Decl _) = True
isDecl _ = False

isLocalparam :: Statement -> Bool
isLocalparam (Localparam _ _) = True
isLocalparam _ = False

isPort :: Statement -> Bool
isPort (Port _ _) = True
isPort _ = False


parser :: Parser [[Statement]]
parser = sc *> many statement <* eof

statement :: Parser [Statement]
statement = localparams <|> declarations <|> modules <|> try (wrapStatement assignment) <|> ignored

modules :: Parser [Statement]
modules = 
    do  rword "module"
        name <- identifier
        symbol "("
        ports <- port `sepBy` comma
        symbol ")"
        semicolon
        return [Port name ports]

port :: Parser PortConnection
port = inPort <|> outPort <|> inOutPort

outPort :: Parser PortConnection
outPort = 
    do  rword "output"
        c <- connection
        return $ Out c

inOutPort :: Parser PortConnection
inOutPort = 
    do  rword "inout"
        c <- wire -- no inout registers
        return $ Inout c

inPort :: Parser PortConnection
inPort = 
    do  rword "input"
        c <- connection
        return $ In c

commaSepStatements :: String -> Parser a -> Parser [a]
commaSepStatements a b = rword a *> b `sepBy` comma <* semicolon

connection :: Parser Connection
connection = (rword "reg" >> register) <|> (rword "wire" >> wire)


ignored :: Parser [Statement]
ignored = ignorable <* sc

ignorable :: Parser [Statement]
ignorable = 
    do  some $ noneOf ";\r\n"
        optional semicolon
        return [Ignore]

wrapStatement :: Monad m => m a -> m [a]
wrapStatement a = a >>= \b -> return [b]

assignment :: Parser Statement
assignment = try (wrap concAssign Assignment) <|> wrap blockAssign Assignment

concAssign :: Parser Assign
concAssign =
    do  lvalue <- identifier
        concEq
        rvalue <- aExpression
        semicolon
        return $ Concurrent lvalue rvalue
        
blockAssign :: Parser Assign
blockAssign =
    do  optional $ rword "assign"
        lvalue <- identifier
        many selection
        eq
        rvalue <- aExpression
        semicolon
        return $ Blocking lvalue rvalue
        

localparams :: Parser [Statement]
localparams = commaSepStatements "localparam" localparam

localparam :: Parser Statement
localparam =
     do paramName <- identifier
        eq
        paramValue <- numeric
        return $ Localparam paramName paramValue

declarations :: Parser [Statement]
declarations = registers <|> wires <|> integers

registers :: Parser [Statement]
registers = commaSepStatements "reg" (wrap register Decl)

regLike :: (Identifier -> Range -> r) -> Parser r
regLike a =
    do  size <- optional range
        name <- identifier
        additionalSizes <- many range
        let actualSize = fromMaybe (rangeConstant 0 0) size
        return $ a name actualSize
        
register :: Parser Connection
register = regLike Reg
        
integers :: Parser [Statement]
integers = commaSepStatements "integer" (wrap integer Decl)

integer :: Parser Connection
integer =
    do  name <- identifier
        return $ Reg name $ rangeConstant 31 0


wires :: Parser [Statement]
wires = commaSepStatements "wire" (wrap wire Decl)

wire :: Parser Connection
wire = regLike Wire 

rangeConstant :: (Show a) => a -> a -> Range
rangeConstant a b = Range (Number (Dec 32 (show a) False)) (Number (Dec 32 (show b) False))

