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
        Left err -> putStrLn $ show err
        Right o -> putStrLn $ show o
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

identifier =  lexeme (p >>= check)
    where p         = (:) <$> letterChar <*> many idChar <* sc
          check x   = if x `elem` reservedWords
                      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                      else return x

data Width = Width Int Int deriving (Show, Eq)
data PortConnection = InWire Width String | OutWire Width String | OutReg Width String | Inout Width String deriving (Show, Eq)

data Statement = Seq [Statement] 
                | Port String [PortConnection]
                | Localparam String VerilogNumeric
                | Reg String Range
                | Ignore
                deriving (Show, Eq)

rword :: String -> Parser ()
rword w = string w *> notFollowedBy idChar *> sc

reservedWords :: [String]
reservedWords = ["localparam", "param", "parameter", "begin", "if", "else", "end", "always"]

parser :: Parser [[Statement]]
parser = sc *> many statement <* eof

statement :: Parser [Statement]
statement = localparams <|> registers <|> ignored

ignored :: Parser [Statement]
ignored = ignorable <* sc

ignorable :: Parser [Statement]
ignorable = 
    do  some $ noneOf ";\r\n"
        optional semicolon
        return [Ignore]

registers :: Parser [Statement]
registers =  (:) <$> register <*> many register' <* semicolon

register' :: Parser Statement
register' =
    do  comma
        size <- optional range
        name <- identifier
        additionalSizes <- many range
        let actualSize = fromMaybe (Range "0" "0") size
        return $ Reg name actualSize

register :: Parser Statement
register =
    do  rword "reg"
        size <- optional range
        name <- identifier
        additionalSizes <- many range
        let actualSize = fromMaybe (Range "0" "0") size
        return $ Reg name actualSize

data Range = Range String String deriving (Show, Eq)

data Literal = Named String | Numeric VerilogNumeric deriving (Show, Eq)

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

data VerilogNumeric = Hex Int String | Oct Int String | Bin Int String | Dec Int String deriving (Show, Eq)
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
validateNumber (Hex a b) = if all (`elem` validHex) b then Right $ Hex a b else Left $ "invalid hex character in " ++ show b
validateNumber (Bin a b) = if all (`elem` validBin) b then Right $ Bin a b else Left $ "invalid hex character in " ++ show b
validateNumber (Oct a b) = if all (`elem` validOct) b then Right $ Oct a b else Left $ "invalid hex character in " ++ show b
validateNumber (Dec a b) = if all (`elem` validDec) b then Right $ Dec a b else Left $ "invalid hex character in " ++ show b

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
        top <- many idChar
        optional sc
        colon
        optional sc
        bottom <- many idChar
        optional sc
        symbol "]"
        return $ Range top bottom


localparams :: Parser [Statement]
localparams = (:) <$> localparam <*> many localparam' <* semicolon

localparam' :: Parser Statement
localparam' =
    do  comma 
        paramName <- identifier
        eq
        paramValue <- numeric
        return $ Localparam paramName paramValue

localparam :: Parser Statement
localparam =
     do rword "localparam"
        paramName <- identifier
        eq
        paramValue <- numeric
        return $ Localparam paramName paramValue


numericToInteger :: VerilogNumeric -> Int
numericToInteger (Hex _ a) = sum . map (\(x, y) -> 16^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Oct _ a) = sum . map (\(x, y) -> 8^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Bin _ a) = sum . map (\(x, y) -> 2^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a
numericToInteger (Dec _ a) = sum . map (\(x, y) -> 10^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a

