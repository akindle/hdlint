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
-- integers
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
                | Collection [Declaration]
                deriving (Show, Eq)
                
instance GetIdentifier Declaration where
    getIdentifier (Localparam a _) = Just a
    getIdentifier (Parameter a _) = Just a
    getIdentifier (Reg a _ _) = Just a
    getIdentifier (Wire a _ _) = Just a
    

declaration :: Parser Declaration
declaration = localparam
            <|> parameter
            <|> try (regLikeSet "wire" Wire)
            <|> try (regLikeSet "reg" Reg)
            <|> try (portLike "wire" Wire)
            <|> try (portLike "reg" Reg)
            <|> integer
            
localparam :: Parser Declaration
localparam = paramLike "localparam" Localparam

parameter :: Parser Declaration
parameter = paramLike "parameter" Parameter <|> paramLike "param" Parameter

paramLike :: String -> (Identifier -> AExpression -> r) -> Parser r
paramLike a b = do
    _ <- rword a
    name <- identifier
    _ <- eq
    value <- aExpression
    _ <- semicolon
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

regLikeSet a b =
    do
        _ <- optional $ rword a 
        bs <- regLike' b Internal `sepBy` comma
        _ <- semicolon
        return $ Collection bs

regLike' b c =
    do
        fRange <- optional range
        name <- identifier
        restRange <- many range
        let rangeHead = fromMaybe (rangeConstant 0 0) fRange
        let rangeList = rangeHead:restRange
        return $ b name rangeList c
        
portLike :: String -> (Identifier -> [Range] -> Direction -> r) -> Parser r
portLike a b = 
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
    
    -- we're going to treat integers as syntactic sugar for 32-bit registers
integer :: Parser Declaration
integer = do
    dir <- optional direction -- can integers be outputs?
    _ <- rword "integer"
    name <- identifier
    vector <- many range
    _ <- semicolon
    let rangeList = rangeConstant 31 0 :vector
    let dirResult = fromMaybe Internal dir
    return $ Reg name rangeList dirResult