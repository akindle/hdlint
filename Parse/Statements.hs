module Parse.Statements where

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

data CaseType = Default | X | Z deriving (Eq, Show)
data Sensitivity = Posedge Identifier | Negedge Identifier | Level Identifier | Wildcard deriving (Eq, Show)

instance GetIdentifier Sensitivity where
    getIdentifier (Posedge a) = Just a
    getIdentifier (Negedge a) = Just a
    getIdentifier (Level a) = Just a
    getIdentifier _ = Nothing
    
data Statement = ConcurrentAssign Identifier [Selection] AExpression
                | SequentialAssign Identifier [Selection] AExpression
                | Process [Sensitivity] Statement
                | Begin [Statement]
                | If {condition :: AExpression, true :: Statement, false :: Maybe Statement} -- else/else if -> via false
                | CaseSet CaseType AExpression [Statement]
                | Case AExpression Statement
                deriving (Eq, Show)
                
instance GetIdentifier Statement where
    getIdentifier (ConcurrentAssign a _ _) = Just a
    getIdentifier (SequentialAssign a _ _) = Just a
    getIdentifier _ = Nothing
    
statement :: Parser Statement
statement = concurrentAssign 
            <|> sequentialAssign 
            <|> process 
            <|> begin 
            <|> ifStatement 
            <|> beginCase 
            <|> caseItem

concurrentAssign :: Parser Statement
concurrentAssign = do
    name <- identifier
    sel <- many selection
    _ <- concEq
    val <- aExpression
    _ <- semicolon
    return $ ConcurrentAssign name sel val
    
sequentialAssign :: Parser Statement
sequentialAssign = do
    name <- identifier
    sel <- many selection
    _ <- eq
    val <- aExpression
    _ <- semicolon
    return $ SequentialAssign name sel val
    
process :: Parser Statement
process = do
    _ <- rword "always"
    _ <- at
    sensitivity <- parens sensitivityList
    contents <- begin
    return $ Process sensitivity contents

begin :: Parser Statement
begin = do
    _ <- rword "begin"
    statements <- many statement
    _ <- rword "end"
    return $ Begin statements
    
ifStatement :: Parser Statement
ifStatement = do
    _ <- rword "if"
    condition <- parens aExpression
    truePart <- statement
    falsePart <- optional $ rword "else" *> statement
    return $ If condition truePart falsePart

beginCase :: Parser Statement
beginCase = do
    caseType <- caseStart
    condition <- aExpression
    _ <- rword "begin"
    contents <- many statement
    _ <- rword "endcase"
    return $ CaseSet caseType condition contents
    
caseItem :: Parser Statement
caseItem = do
    caseName <- aExpression
    _ <- colon
    content <- statement
    return $ Case caseName content

caseStart :: Parser CaseType
caseStart = (rword "case" >> return Default)
            <|> (rword "casex" >> return X) 
            <|> (rword "casez" >> return Z)

sensitivityList :: Parser [Sensitivity]
sensitivityList = sensitivity `sepBy` rword "or"

sensitivity :: Parser Sensitivity
sensitivity = wildCard
            <|> edge "posedge" Posedge
            <|> edge "negedge" Negedge 
            <|> level Level
level :: (Identifier -> r) -> Parser r
level a = do
    name <- identifier
    return $ a name

edge :: String -> (Identifier -> b) -> Parser b
edge a b = do
    _ <- rword a
    name <- identifier
    return $ b name
    
wildCard :: Parser Sensitivity
wildCard = do
    _ <- symbol "*"
    return Wildcard