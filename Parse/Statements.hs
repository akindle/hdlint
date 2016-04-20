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
import Parse.Declarations

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
                | Begin (Maybe Identifier) [Statement]
                | If {condition :: AExpression, true :: Statement, false :: Maybe Statement} -- else/else if -> via false
                | CaseSet CaseType AExpression [Statement]
                | Case AExpression Statement
                | Forloop AExpression AExpression AExpression Statement
                | Generate Statement 
                | Decl Declaration -- yolo
                | ModuleInstantiation Identifier (Maybe Identifier) [Connection]
                deriving (Eq, Show)

instance GetIdentifier Statement where
    getIdentifier (ConcurrentAssign a _ _) = Just a
    getIdentifier (SequentialAssign a _ _) = Just a
    getIdentifier _ = Nothing

data Connection = Conn Identifier (Maybe AExpression) deriving (Eq, Show)

connection :: Parser Connection
connection = do
    _ <- symbol "."
    a <- identifier
    _ <- symbol "("
    b <- optional aExpression
    _ <- symbol ")"
    return $ Conn a b
                
moduleInst :: Parser Statement
moduleInst = do
    instantiatedModule <- identifier
    instanceName <- optional identifier
    _ <- symbol "("
    connections <- connection `sepBy` comma
    _ <- symbol ")"
    _ <- semicolon
    return $ ModuleInstantiation instantiatedModule instanceName connections
    
statement :: Parser Statement
statement =      process 
            <|>  begin 
            <|>  ifStatement 
            <|>  try continuousAssign
            <|>  try concurrentAssign 
            <|>  try sequentialAssign 
            <|>  try moduleInst
            <|>  forStatement
            <|>  beginCase 
            <|>  try caseItem    
            <|>  generate
            <|>  wrap declaration Decl

generate :: Parser Statement
generate = do
    _ <- rword "generate"
    a <- statement
    _ <- rword "endgenerate"
    return $ Generate a

continuousAssign :: Parser Statement
continuousAssign = do
    _ <- rword "assign"
    sequentialAssign

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
    contents <- statement
    return $ Process sensitivity contents

hello = between (rword "begin") (rword "end")

begin :: Parser Statement
begin = do
    _ <- rword "begin"
    name <- optional (colon *> identifier)
    statements <- try $ many statement
    _ <- rword "end"
    return $ Begin name statements


forStatement :: Parser Statement
forStatement = do
    _ <- rword "for"
    _ <- symbol "("
    initial <- aExpression
    semicolon
    check <- aExpression
    semicolon
    iterator <- aExpression
    _ <- symbol ")"
    content <- statement
    return $ Forloop initial check iterator content
    
ifStatement :: Parser Statement
ifStatement = do
    _ <- rword "if"
    condition <- aExpression
    truePart <- statement
    falsePart <- optional $ rword "else" *> statement
    return $ If condition truePart falsePart

beginCase :: Parser Statement
beginCase = do
    caseType <- caseStart
    condition <- parens aExpression
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
caseStart = try (rword "case" >> return Default)
            <|> try (rword "casex" >> return X) 
            <|> try (rword "casez" >> return Z)

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
