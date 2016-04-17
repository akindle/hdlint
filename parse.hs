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

    
    -- how do we cover concatenation with this? it's a bit 
    -- like an infix operator (,) but requires being in {}
    -- keep in mind replication ops { #{}}
    -- this covers boolean expressions as well (eg x == 5)
    -- syntactically-valid boolExprs ie if(12'd13) is legal in verilog
    -- according to a few internet sources (ie http://web.engr.oregonstate.edu/~traylor/ece474/lecture_verilog/beamer/verilog_operators.pdf)
    -- boolean operators reduce to [1'b0, 1'b1]; looks like zero -> false, nonzero -> true
    -- so "Expr" can safely be used for conditional parsing
    -- that is, all if() statements are of the form if(Expr)
    -- and boolean operators are just another infix operator
    -- basically just read the above link a lot while implementing and also some other sources    
--data Expr = Variable -- or really just any named thing (reg, wire, localparam, parameter, etc)
  --          | Number -- any numeric constant
    --        | Neg Expr -- negation of any other sort of expression
      --      | Operation Operator Expr Expr          
            
data AExpression = Var Identifier Selection 
                | Number VerilogNumeric 
                | Unary UOp AExpression
                | ABinary AOp AExpression AExpression 
                deriving (Eq)
instance Show AExpression where
    show (Var a b) = show a ++ show b
    show (Number a) = show a
    show (Unary a b) = show a ++ show b
    show (ABinary a b c) = show b ++ " " ++ show a ++ " " ++ show c

data UOp = RedAnd 
        | RedOr 
        | RedNand 
        | RedNor
        | RedXor 
        | RedXNor
        | Pos
        | Neg
        | BitNot
        | LogicNot
        deriving (Show, Eq)
data AOp =  Add 
        | Subtract 
        | BitOr
        | BitAnd 
        | LShift 
        | RShift
        | Mult
        | Div
        | Mod
        | LThan
        | LEq
        | GThan
        | GEq
        | Equal
        | NotEqual
        | BitXor
        | BitXNor
        | LogicAnd
        | LogicOr
        deriving (Show, Eq) 
    
aOperators :: [[Operator Parser AExpression]]
aOperators =
    [[Prefix (symbol "~" *> pure (Unary BitNot)), Prefix (symbol "!" *> pure (Unary LogicNot))],
    
    [Prefix (symbol "&" *> pure (Unary RedAnd)), 
    Prefix (symbol "|" *> pure (Unary RedOr)),
    Prefix (symbol "~&" *> pure (Unary RedNand)),
    Prefix (symbol "~|" *> pure (Unary RedNor)), 
    Prefix (symbol "^" *> pure (Unary RedXor)), 
    Prefix (symbol "~^" *> pure (Unary RedXNor))],
    
          
    [Prefix (symbol "-" *> pure (Unary Neg)), Prefix (symbol "+" *> pure (Unary Pos))],
    
    [InfixL (symbol "*" *> pure (ABinary Mult)),InfixL (symbol "%" *> pure (ABinary Mod)),InfixL (symbol "/" *> pure (ABinary Div))],
    
    [InfixL (symbol "+" *> pure (ABinary Add)),InfixL (symbol "-" *> pure (ABinary Subtract))],
    
    [InfixL (symbol "<<" *> pure (ABinary LShift)), InfixL (symbol ">>" *> pure (ABinary RShift))],

    [InfixL (symbol "<" *> pure (ABinary LThan)), InfixL (symbol "<=" *> pure (ABinary LEq)), InfixL (symbol ">" *> pure (ABinary GThan)), InfixL (symbol ">=" *> pure (ABinary GEq))],
    
    [InfixL (symbol "==" *> pure (ABinary Equal)), InfixL (symbol "!=" *> pure (ABinary NotEqual))],

    [InfixL (symbol "&" *> pure (ABinary BitAnd))],
    
    [InfixL (symbol "^" *> pure (ABinary BitXor)), InfixL (symbol "~^" *> pure (ABinary BitXNor)), InfixL (symbol "^~" *> pure (ABinary BitXNor))],
    
     [InfixL (symbol "|" *> pure (ABinary BitOr))],
     [InfixL (symbol "&&" *> pure (ABinary LogicAnd)), InfixL (symbol "||" *> pure (ABinary LogicOr))]
    ]

data Range = Range AExpression AExpression deriving (Eq)
instance Show Range where
    show (Range a b) = "[" ++ show a ++ ":" ++ show b ++ "]"

data Selection = RSel Range
                | Sel AExpression
                | ImplicitSelection
                deriving (Eq)
instance Show Selection where
    show (RSel a) = show a
    show (Sel b) = "[" ++ show b ++ "]"
    show ImplicitSelection = ""

data PortConnection = In Connection
                    | Out Connection
                    | Inout Connection
                    deriving (Show, Eq)

data VerilogNumeric = Hex Int String 
                    | Oct Int String 
                    | Bin Int String 
                    | Dec Int String 
                    deriving (Eq)

instance Show VerilogNumeric where
    show (Hex a b) = bits a ++ "h" ++ b
    show (Oct a b) = bits a ++ "o" ++ b
    show (Bin a b) = bits a ++ "b" ++ b
    show (Dec a b) = bits a ++ b
bits a = if a == 32 then "" else show a ++ "'"

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


rword :: String -> Parser ()
rword w = string w *> notFollowedBy idChar *> sc

reservedWords :: [String]
reservedWords = ["localparam", "param", "parameter", "begin", "if", "else", "end", "always", "module", "endmodule", "input", "output", "inout", "wire", "reg", "integer"]


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

wrap a b = a >>= \c -> return $ b c

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

rangeConstant a b = Range (Number (Dec 32 (show a))) (Number (Dec 32 (show b)))


aExpression :: Parser AExpression
aExpression = makeExprParser aTerm aOperators


aTerm :: Parser AExpression
aTerm = parens aExpression
        <|> Var     <$> identifier <*> selection
        <|> Number  <$> numeric


-- a verilog literal is of the form:
-- <size>'<signed><radix>value
-- where <> items are optional
-- size is the number of binary bits; default is 32 bits
-- signed indicates if the value is signed - s or S/default is unsigned
-- radix is the radix of the number - b/B o/O h/H d/D - default is decimal
-- "value" depends on radix
-- b: 01xXzZ_?
-- o: 0-7xXzZ_?
-- d: 0-9_
-- h: 0-9Aa-FfXxZz_?
-- size too small: truncates MSBs
-- size too big: MSBs are filled:
--  left-most bit 0 or 1: 0-extend
--  left-most bit x or z: x or z-extend
--  http://web.engr.oregonstate.edu/~traylor/ece474/lecture_verilog/beamer/verilog_number_literals.pdf
--  they are yucky
--
-- a number of the form <bits>'<type><value>, as handled by VerilogNumeric
-- an undecorated decimal number of the form <value> (implicitly 32 bits?)
-- a previously-defined localparam (ie an identifier)
--literal :: Parser Literal
--literal = identifier <|> numeric
numericSize :: Parser String
numericSize = try $ many digitChar <* char '\''

numeric :: Parser VerilogNumeric
numeric =
    do  bits <- optional numericSize
        signed <- optional $ char' 's'
        mode <- optional letterChar
        value <- many $ oneOf validHex
        sc
        let b = read $ fromMaybe "32" bits :: Int
        let radix = fromMaybe 'd' mode
        let res = createNumber radix b value
        case res of
            Left a -> fail a
            Right b -> return b

createNumber :: Char -> Int -> String -> Either String VerilogNumeric
createNumber a b c = case toUpper a of
    'H' -> validateNumber $ Hex b c
    'B' -> validateNumber $ Bin b c
    'O' -> validateNumber $ Oct b c
    'D' -> validateNumber $ Dec b c
    _ -> Left $ show a ++ " is not a legal numeric radix"


validateNumber :: VerilogNumeric -> Either String VerilogNumeric 
validateNumber (Hex a b) = if all (`elem` validHex) b then Right $ Hex a b else Left $ "invalid hex character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validHex) b)
validateNumber (Bin a b) = if all (`elem` validBin) b then Right $ Bin a b else Left $ "invalid binary character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validBin) b)
validateNumber (Oct a b) = if all (`elem` validOct) b then Right $ Oct a b else Left $ "invalid decimal character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validOct) b)
validateNumber (Dec a b) = if all (`elem` validDec) b then Right $ Dec a b else Left $ "invalid octal character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validDec) b)

validHex = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'] ++ validUnknowns ++ validSeparators
validDec = ['0'..'9'] ++ validSeparators
validOct = ['0'..'7'] ++ validUnknowns ++ validSeparators
validBin = ['0', '1'] ++ validUnknowns ++ validSeparators

validSeparators = ['_']
validUnknowns = ['z', 'Z', 'x', 'X', '?']

range :: Parser Range
range = 
    do  symbol "["
        optional sc
        top <- aExpression
        optional sc
        colon
        optional sc
        bottom <- aExpression
        optional sc
        symbol "]"
        return $ Range top bottom

selection :: Parser Selection
selection = try (wrap range RSel) <|> selection'

selection' :: Parser Selection
selection' =
    do  symbol "["
        optional sc
        sel <- aExpression
        optional sc
        symbol "]"
        return $ Sel sel

numericToInteger :: VerilogNumeric -> Int
numericToInteger (Hex _ a) = sum . map (\(x, y) -> 16^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Oct _ a) = sum . map (\(x, y) -> 8^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Bin _ a) = sum . map (\(x, y) -> 2^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Dec _ a) = sum . map (\(x, y) -> 10^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a

