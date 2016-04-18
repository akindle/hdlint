module Parse.Expressions where

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
            

aExpression :: Parser AExpression
aExpression = makeExprParser aTerm aOperators

-- note: this probably does not work
-- because this is not really how the angle brackets in a replication work
-- this will almost certainly break on nested replication/concatenation operations
replication :: Parser AExpression
replication = do
    symbol "{"
    repCount <- aExpression -- eugh kind of
    symbol "{"
    repExpr <- aExpression
    symbol "}"
    symbol "}"
    return $ Replication repCount repExpr
                
aTerm :: Parser AExpression
aTerm = parens aExpression
        <|> Var     <$> identifier <*> selection
        <|> replication
        <|> Concat  <$> angles aExpression <* comma *> aExpression
        <|> Number  <$> numeric
        <|> Ternary <$> aExpression <* symbol "?" *> aExpression <* colon *> aExpression

data AExpression = Var Identifier Selection 
                | Replication AExpression AExpression
                | Concat AExpression AExpression
                | Number VerilogNumeric 
                | Unary UOp AExpression
                | ABinary AOp AExpression AExpression 
                | Ternary AExpression AExpression AExpression
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

-- what a mess of operator declarations
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
    do  size <- optional numericSize
        signed <- optional $ char' 's'
        mode <- optional letterChar
        value <- many $ oneOf validHex
        sc
        let b = read $ fromMaybe "32" size :: Int
        let radix = fromMaybe 'd' mode
        let sign = fromMaybe '\0' signed == 's'
        let res = createNumber radix b value sign
        case res of
            Left a -> fail a
            Right b -> return b

createNumber :: Char -> Int -> String -> Bool -> Either String VerilogNumeric
createNumber a b c d = case toUpper a of
    'H' -> validateNumber $ Hex b c d
    'B' -> validateNumber $ Bin b c d
    'O' -> validateNumber $ Oct b c d
    'D' -> validateNumber $ Dec b c d
    _ -> Left $ show a ++ " is not a legal numeric radix"


validateNumber :: VerilogNumeric -> Either String VerilogNumeric 
validateNumber (Hex a b c) = if all (`elem` validHex) b then Right $ Hex a b c else Left $ "invalid hex character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validHex) b)
validateNumber (Bin a b c) = if all (`elem` validBin) b then Right $ Bin a b c else Left $ "invalid binary character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validBin) b)
validateNumber (Oct a b c) = if all (`elem` validOct) b then Right $ Oct a b c else Left $ "invalid decimal character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validOct) b)
validateNumber (Dec a b c) = if all (`elem` validDec) b then Right $ Dec a b c else Left $ "invalid octal character in " ++ show b ++ " specifically: " ++ show (filter (`notElem` validDec) b)

validHex = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'] ++ validUnknowns ++ validSeparators
validDec = ['0'..'9'] ++ validSeparators
validOct = ['0'..'7'] ++ validUnknowns ++ validSeparators
validBin = ['0', '1'] ++ validUnknowns ++ validSeparators

validSeparators = ['_']
validUnknowns = ['z', 'Z', 'x', 'X', '?']

range :: Parser Range
range = 
    do  symbol "[" 
        top <- aExpression 
        colon 
        bottom <- aExpression 
        symbol "]"
        return $ Range top bottom

selection :: Parser Selection
selection = try (wrap range RSel) <|> selection'

selection' :: Parser Selection
selection' =
    do  symbol "[" 
        sel <- aExpression 
        symbol "]"
        return $ Sel sel

numericToInteger :: VerilogNumeric -> Int
numericToInteger (Hex c a b) = if b && (digitToInt (head a) >= 8) then -1 else 1 * (sum . map (\(x, y) -> 16^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a)
numericToInteger (Oct c a b) = if b && (digitToInt (head a) >= 4) then -1 else 1 * (sum . map (\(x, y) -> 8^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a)
numericToInteger (Bin c a b) = if b && (digitToInt (head a) >= 1) then -1 else 1 * (sum . map (\(x, y) -> 2^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a)
numericToInteger (Dec c a b) = if b && (False) then -1 else 1 * (sum . map (\(x, y) -> 10^x * y) . zip [0..] . reverse . map digitToInt $ filter (/='_') a)


data VerilogNumeric = Hex Int String Bool
                    | Oct Int String Bool
                    | Bin Int String Bool
                    | Dec Int String Bool
                    deriving (Eq)

instance Show VerilogNumeric where
    show (Hex a b c) = bits a ++ if c then "s" else "" ++ "h" ++ b
    show (Oct a b c) = bits a ++ if c then "s" else "" ++ "o" ++ b
    show (Bin a b c) = bits a ++ if c then "s" else "" ++ "b" ++ b
    show (Dec a b c) = bits a ++ if c then "s" else "" ++ b
bits a = if a == 32 then "" else show a ++ "'"

data Selection = RSel Range
                | Sel AExpression
                | ImplicitSelection
                deriving (Eq)
instance Show Selection where
    show (RSel a) = show a
    show (Sel b) = "[" ++ show b ++ "]"
    show ImplicitSelection = ""

data Range = Range AExpression AExpression deriving (Eq)
instance Show Range where
    show (Range a b) = "[" ++ show a ++ ":" ++ show b ++ "]"
    
rangeConstant :: (Show a) => a -> a -> Range
rangeConstant a b = Range (Number (Dec 32 (show a) False)) (Number (Dec 32 (show b) False))

