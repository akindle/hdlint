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
                | Ambiguous Identifier [Range] Direction
                | Collection [Declaration]
                | Genvar Identifier
                deriving (Show, Eq)
                
instance GetIdentifiers Declaration where
    getIdentifiers (Localparam a b) = [a] ++ getIdentifiers b
    getIdentifiers (Parameter a b) = [a] ++ getIdentifiers b
    getIdentifiers (Reg a b _) = [a] ++ concatMap getIdentifiers b
    getIdentifiers (Wire a b _) = [a] ++ concatMap getIdentifiers b
    getIdentifiers (Collection a) = concatMap getIdentifiers a
    getIdentifiers (Genvar a) = [a]
    getIdentifiers _ = []

    getIdentifierDeclarations (Localparam a _) = [a]
    getIdentifierDeclarations (Parameter a _) = [a]
    getIdentifierDeclarations (Reg a _ _) = [a] 
    getIdentifierDeclarations (Wire a _ _) = [a]
    getIdentifierDeclarations (Ambiguous a _ _) = [a]
    getIdentifierDeclarations (Collection a) = concatMap getIdentifierDeclarations a
    getIdentifierDeclarations (Genvar a) = [a]
    getIdentifierDeclarations _ = []

    getIdentifierUtilizations (Localparam _ a) = getIdentifierUtilizations a
    getIdentifierUtilizations (Parameter _ a) = getIdentifierUtilizations a
    getIdentifierUtilizations (Collection a) = concatMap getIdentifierUtilizations a
    getIdentifierUtilizations (Reg _ a _) = concatMap getIdentifierUtilizations a
    getIdentifierUtilizations (Wire _ a _) = concatMap getIdentifierUtilizations a
    getIdentifierUtilizations (Ambiguous _ a _) = concatMap getIdentifierUtilizations a
    getIdentifierUtilizations _ = []
    

declaration :: Parser Declaration
declaration = try localparam
            <|> try parameter
            <|> try moduleParameter
            <|> try (regLikeSet "wire" Wire)
            <|> try (regLikeSet "reg" Reg)
            <|> try (portLike "wire" Wire)
            <|> try (portLike "reg" Reg)
            <|> try ambiguousPort
            <|> try integer
            <|> try genvar <?> "declaration/port"
            
localparam :: Parser Declaration
localparam = paramList "localparam" Localparam

parameter :: Parser Declaration
parameter = paramList "parameter" Parameter <|> paramList "param" Parameter

paramList a b = do
    _ <- rword a
    items <- paramList' b `sepBy` comma
    _ <- semicolon
    return $ Collection items

paramList' b = do
    name <- identifier
    _ <- eq
    value <- aExpression
    return $ b name value

-- this works very poorly for comma separated lists of declarations
paramLike :: String -> (Identifier -> AExpression -> r) -> Parser r
paramLike a b = do
    _ <- rword a
    name <- identifier
    _ <- eq
    value <- aExpression
    _ <- semicolon
    return $ b name value

moduleParameter :: Parser Declaration
moduleParameter = do
    _ <- rword "parameter"
    name <- identifier
    _ <- eq
    value <- aExpression
    return $ Parameter name value
    
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
        
ambiguousPort :: Parser Declaration
ambiguousPort = 
    do
        dir <- direction
        firstRange <- optional range
        name <- identifier
        restRange <- many range
        let rangeHead = fromMaybe (rangeConstant 0 0) firstRange
        let rangeList = rangeHead:restRange  
        return $ Ambiguous name rangeList dir
        
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
    
genvar :: Parser Declaration
genvar = do
    _ <- rword "genvar"
    name <- identifier
    _ <- semicolon
    return $ Genvar name

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
