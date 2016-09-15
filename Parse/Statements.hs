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

data CaseType = Exact | X | Z deriving (Eq, Show)
data Sensitivity = Posedge Identifier | Negedge Identifier | Level Identifier | Wildcard deriving (Eq, Show)

instance GetIdentifiers Sensitivity where
    getIdentifiers (Posedge a) = [a]
    getIdentifiers (Negedge a) = [a]
    getIdentifiers (Level a) = [a]
    getIdentifiers _ = []
    getIdentifierUtilizations = getIdentifiers
    getIdentifierDeclarations _ = []
    
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

instance GetIdentifiers Statement where
    getIdentifiers (ConcurrentAssign a b c) = [a] ++ concatMap getIdentifiers b ++ getIdentifiers c
    getIdentifiers (SequentialAssign a b c) = [a] ++ concatMap getIdentifiers b ++ getIdentifiers c
    getIdentifiers (Process a b) = concatMap getIdentifiers a ++ getIdentifiers b
    getIdentifiers (Begin (Just a) b) = [a] ++ concatMap getIdentifiers b
    getIdentifiers (Begin (Nothing) b) = concatMap getIdentifiers b
    getIdentifiers (If a b (Just c)) = getIdentifiers a ++ getIdentifiers b ++ getIdentifiers c
    getIdentifiers (If a b (Nothing)) = getIdentifiers a ++ getIdentifiers b
    getIdentifiers (CaseSet _ a b) = getIdentifiers a ++ concatMap getIdentifiers b
    getIdentifiers (Case a b) = getIdentifiers a ++ getIdentifiers b
    getIdentifiers (Forloop a b c d) = getIdentifiers a ++ getIdentifiers b ++ getIdentifiers c ++ getIdentifiers d
    getIdentifiers (Generate a) = getIdentifiers a
    getIdentifiers (Decl a) = getIdentifiers a -- this may be a very bad idea
    getIdentifiers (ModuleInstantiation a (Just b) c) = [a, b] ++ concatMap getIdentifiers c
    getIdentifiers (ModuleInstantiation a (Nothing) c) = [a] ++ concatMap getIdentifiers c

    getIdentifierDeclarations (Process _ a) = getIdentifierDeclarations a
    getIdentifierDeclarations (Begin (Just a) b) = [a] ++ concatMap getIdentifierDeclarations b
    getIdentifierDeclarations (Begin (Nothing) b) = concatMap getIdentifierDeclarations b
    getIdentifierDeclarations (If a b (Just c)) = getIdentifierDeclarations b ++ getIdentifierDeclarations c
    getIdentifierDeclarations (If a b (Nothing)) = getIdentifierDeclarations b
    getIdentifierDeclarations (CaseSet _ _ a) = concatMap getIdentifierDeclarations a
    getIdentifierDeclarations (Case _ a) = getIdentifierDeclarations a
    getIdentifierDeclarations (Forloop _ _ _ a) = getIdentifierDeclarations a
    getIdentifierDeclarations (Generate a) = getIdentifierDeclarations a
    getIdentifierDeclarations (Decl a) = getIdentifierDeclarations a
    getIdentifierDeclarations (ModuleInstantiation _ (Just b) _) =  [b]
    getIdentifierDeclarations _ = []

    getIdentifierUtilizations (ConcurrentAssign a b c) = [a] ++ concatMap getIdentifierUtilizations b ++ getIdentifierUtilizations c
    getIdentifierUtilizations (SequentialAssign a b c) = [a] ++ concatMap getIdentifierUtilizations b ++ getIdentifierUtilizations c
    getIdentifierUtilizations (Process a b) = concatMap getIdentifierUtilizations a ++ getIdentifierUtilizations b
    getIdentifierUtilizations (Begin _ a) = concatMap getIdentifierUtilizations a
    getIdentifierUtilizations (If a b (Just c)) = getIdentifierUtilizations a ++ getIdentifierUtilizations b ++ getIdentifierUtilizations c
    getIdentifierUtilizations (If a b (Nothing)) = getIdentifierUtilizations a ++ getIdentifierUtilizations b
    getIdentifierUtilizations (CaseSet _ a b) = getIdentifierUtilizations a ++ concatMap getIdentifierUtilizations b
    getIdentifierUtilizations (Case a b) = getIdentifierUtilizations a ++ getIdentifierUtilizations b
    getIdentifierUtilizations (Forloop a b c d) = getIdentifierUtilizations a ++ getIdentifierUtilizations b ++ getIdentifierUtilizations c ++ getIdentifierUtilizations d
    getIdentifierUtilizations (Generate a) = getIdentifierUtilizations a
    getIdentifierUtilizations (Decl a) = getIdentifierUtilizations a
    getIdentifierUtilizations (ModuleInstantiation a _ b) = [a] ++ concatMap getIdentifierUtilizations b

data Connection = Conn Identifier (Maybe AExpression) deriving (Eq, Show)
instance GetIdentifiers Connection where
    getIdentifiers (Conn a (Just b)) = [a] ++ getIdentifiers b
    getIdentifiers (Conn a (Nothing)) = [a] 
    getIdentifierDeclarations _ = []
    getIdentifierUtilizations = getIdentifiers

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

begin :: Parser Statement
begin = do
    _ <- rword "begin"
    name <- optional (colon *> identifier)
    statements <- try $ manyTill statement (rword "end")
    return $ Begin name statements


forStatement :: Parser Statement
forStatement = do
    _ <- rword "for"
    _ <- symbol "("
    initial <- aExpression
    _ <- semicolon
    check <- aExpression
    _ <- semicolon
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
    condition <- aExpression
    contents <- manyTill caseItem (rword "endcase")
    return $ CaseSet caseType condition contents
    
caseItem :: Parser Statement
caseItem = do
    caseName <- (aExpression <|> (rword "default" >> return Default))
    _ <- colon
    content <- statement
    return $ Case caseName content


caseStart :: Parser CaseType
caseStart = try (rword "case" >> return Exact)
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
