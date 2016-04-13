module Parse where

import System.Environment
import System.IO
import Data.Char
import Data.Maybe
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

--file = endBy csvFile eof
--csvFile = endBy line eol'
--line = sepBy cell (char ',')
--cell = many (noneOf ",\n\r") 
--eol' =      try (string "\n\r")  
--        <|> try (string "\r\n")
--        <|> string "\r"
--        <|> string "\n"
--        <?> "end of line"

main :: IO ()
main = do
    handle <- openFile "hello.v" ReadMode
    contents <- hGetContents handle
    let parsed = parseCSV "hello.v" contents
    case parsed of
        Left err -> print err
        Right o -> print o
    hClose handle

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
dot        = symbol "."
eq         = symbol "="
idChar     = alphaNumChar <|> char '_'
idHeadChar = letterChar <|> char '_'

identifier =  lexeme (p >>= check)
    where p         = (:) <$> idHeadChar <*> many idChar <* sc
          check x   = if x `elem` reservedWords
                      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                      else return x

data AExpression = Var String 
                | Number VerilogNumeric 
                | Neg AExpression 
                | ABinary AOp AExpression AExpression 
                deriving (Eq)
instance Show AExpression where
    show (Var a) = show a
    show (Number a) = show a
    show (Neg a) = "-" ++ (show a)
    show (ABinary a b c) = (show b) ++ " " ++ (show a) ++ " " ++ (show c)

data AOp = Add 
        | Subtract 
        deriving (Eq)
instance Show AOp where
    show Add = "+"
    show Subtract = "-"

data Range = Range AExpression AExpression deriving (Eq)
instance Show Range where
    show (Range a b) = "[" ++ (show a) ++ ":" ++ (show b) ++ "]"

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
    show (Hex a b) = (bits a) ++ "h" ++ b
    show (Oct a b) = (bits a) ++ "o" ++ b
    show (Bin a b) = (bits a) ++ "b" ++ b
    show (Dec a b) = (bits a) ++ b
bits a = if(a == 32) then "" else (show a ++ "'")

data Connection = Reg String Range
                | Wire String Range
                deriving (Eq)
instance Show Connection where
    show (Reg name range) = "reg " ++ (show range) ++ " " ++ name ++ ";"
    show (Wire name range) = "wire " ++ (show range) ++ " " ++ name ++ ";"

data Statement = Seq [Statement] 
                | Port String [PortConnection]
                | Localparam String VerilogNumeric
                | Decl Connection
                | Ignore
                deriving (Eq)
instance Show Statement where
    show (Seq [x]) = "hi"
    show (Port a b) = "module " ++ a ++ "(" ++ show b ++ ")"
    show (Localparam name number) = "localparam " ++ name ++ " = " ++ (show number) ++ ";"
    show (Decl a) = show a
    show _ = ""

rword :: String -> Parser ()
rword w = string w *> notFollowedBy idChar *> sc

reservedWords :: [String]
reservedWords = ["localparam", "param", "parameter", "begin", "if", "else", "end", "always"]

parser :: Parser [[Statement]]
parser = sc *> many statement <* eof

statement :: Parser [Statement]
statement = localparams <|> registers <|> modules <|> ignored

modules :: Parser [Statement]
modules = 
    do  rword "module"
        name <- identifier
        symbol "("
        ports <- port `sepBy` comma
        return $ [Port name ports]

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
        c <- wire' -- no inout registers
        return $ Inout c

inPort :: Parser PortConnection
inPort = 
    do  rword "input"
        c <- connection
        return $ In c

connection :: Parser Connection
connection = register' <|> wire'

ignored :: Parser [Statement]
ignored = ignorable <* sc

ignorable :: Parser [Statement]
ignorable = 
    do  some $ noneOf ";\r\n"
        optional semicolon
        return [Ignore]

registers :: Parser [Statement]
registers = rword "reg" *> register `sepBy` comma <* semicolon

register :: Parser Statement
register =
    do  size <- optional range
        name <- identifier
        additionalSizes <- many range
        let actualSize = fromMaybe (Range (Number (Dec 32 "0")) (Number (Dec 32 "0"))) size
        return $ Decl (Reg name actualSize)

wire' :: Parser Connection
wire' =
    do  rword "wire"
        size <- optional range
        name <- identifier
        additionalSizes <- many range
        let actualSize = fromMaybe (Range (Number (Dec 32 "0")) (Number (Dec 32 "0"))) size
        return $ Wire name actualSize

register' :: Parser Connection
register' =
    do  rword "reg"
        size <- optional range
        name <- identifier
        additionalSizes <- many range
        let actualSize = fromMaybe (Range (Number (Dec 32 "0")) (Number (Dec 32 "0"))) size
        return $ Reg name actualSize

localparams :: Parser [Statement]
localparams = rword "localparam" *> localparam `sepBy` comma <* semicolon

localparam :: Parser Statement
localparam =
     do paramName <- identifier
        eq
        paramValue <- numeric
        --let pos = getParserState paramName
        return $ Localparam paramName paramValue


aOperators :: [[Operator Parser AExpression]]
aOperators =
    [[Prefix (symbol "-" *> pure Neg)],
     [InfixL (symbol "+" *> pure (ABinary Add)),
      InfixL (symbol "-" *> pure (ABinary Subtract)) ]
    ]

aExpression :: Parser AExpression
aExpression = makeExprParser aTerm aOperators

aTerm :: Parser AExpression
aTerm = parens aExpression
        <|> Var     <$> identifier
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


numericToInteger :: VerilogNumeric -> Int
numericToInteger (Hex _ a) = sum . map (\(x, y) -> 16^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Oct _ a) = sum . map (\(x, y) -> 8^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Bin _ a) = sum . map (\(x, y) -> 2^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Dec _ a) = sum . map (\(x, y) -> 10^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a

