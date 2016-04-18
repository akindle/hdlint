module Parse.Declarations where

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

-- declarations are:
-- localparams
-- parameters
-- regs
-- wires
-- inputs
-- outputs
-- inouts
-- a declaration is an identifier and some baggage like dimensions or an expression
-- there are many subtypes of nets (wires): http://verilog.renerta.com/source/vrg00030.htm
-- gonna just ignore those for now
data Direction = Internal | Input | Output | Inout deriving (Show, Eq)

data Declaration = Localparam Identifier AExpression
                | Parameter Identifier AExpression
                | Reg Identifier [Range] Direction
                | Wire Identifier [Range] Direction
                deriving (Show, Eq)
                
declaration :: Parser Declaration
declaration = localparam
            <|> parameter
            <|> regLike "wire" Wire
            <|> regLike "reg" Reg
            
localparam :: Parser Declaration
localparam = paramLike "localparam" Localparam

parameter :: Parser Declaration
parameter = paramLike "parameter" Parameter <|> paramLike "param" Parameter

paramLike :: String -> (Identifier -> AExpression -> r) -> Parser r
paramLike a b = do
    _ <- rword a
    _ <- eq
    name <- identifier
    value <- aExpression
    return $ b name value

direction :: Parser Direction
direction = direction' "input" Input
        <|> direction' "output" Output
        <|> direction' "inout" Inout

direction' :: String -> b -> Parser b
direction' a b =
    do
        _ <- rword a
        return b

regLike :: String -> (Identifier -> [Range] -> Direction -> r) -> Parser r
regLike a b = 
    do
    dir <- optional direction
    _ <- rword a
    firstRange <- optional range
    name <- identifier
    restRange <- many range
    let rangeHead = fromMaybe (rangeConstant 0 0) firstRange
    let rangeList = rangeHead:restRange 
    let dirResult = fromMaybe Internal dir
    return $ b name rangeList dirResult