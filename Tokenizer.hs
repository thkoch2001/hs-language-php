module Tokenizer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import StringParse
import Control.Applicative ((<*))

data PHPValue = PHPString String
              | PHPInt Integer
              | PHPFloat Double
              | PHPBool Bool
              | PHPNull
              deriving (Show)

data PHPVariable = PHPVariable String | PHPVariableVariable String deriving (Show)
data FunctionCall = FunctionCall String | FunctionCallVar PHPVariable deriving (Show)

data PHPExpr = Literal PHPValue
             | Variable PHPVariable
             | Assign PHPVariable PHPExpr
             | Neg PHPExpr
             | Not PHPExpr
             | BinaryExpr BinOp PHPExpr PHPExpr
             | UnaryExpr UnaryType UnaryOp PHPVariable
             | Call FunctionCall [PHPExpr]
             | Isset [PHPVariable]
             | Print PHPExpr
             deriving (Show)

data UnaryType = Before | After deriving (Show)
data UnaryOp = Increment | Decrement deriving (Show)

mkUnaryOp :: String -> UnaryOp
mkUnaryOp "++" = Increment
mkUnaryOp "--" = Decrement
mkUnaryOp _ = error "Invalid unary op"

data BinOp = Add | Subtract | Multiply | Divide | Modulo | And | Or | Greater | Less | Equals | StrictEquals | Concat deriving (Show)

data ElseExpr = Else PHPStmt
              | ElseIf PHPExpr PHPStmt (Maybe ElseExpr)
              deriving (Show)

data FunctionArgumentDef = FunctionArgumentDef { argName :: String
                                               , argDefault :: Maybe PHPValue
                                               }
                                               deriving (Show)

data ParseResult = PlainText String | PHPCode PHPStmt deriving (Show)

data StaticVar = StaticVar String (Maybe PHPValue) deriving (Show)

data PHPStmt = Seq [PHPStmt]
             | Expression PHPExpr
             | If PHPExpr PHPStmt (Maybe ElseExpr)
             | Function String [FunctionArgumentDef] PHPStmt
             | Return PHPExpr
             | While PHPExpr PHPStmt
             | For [PHPExpr] [PHPExpr] [PHPExpr] PHPStmt
             | Echo [PHPExpr]
             | Global PHPVariable
             | Static [StaticVar]
             deriving (Show)


langDef = emptyDef { Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.commentLine = "//"
                   , Token.identStart = letter
                   , Token.identLetter = alphaNum <|> char '_'
                   , Token.reservedNames = [ "if", "else", "elseif", "while", "break", "do", "for", "continue"
                                           , "true", "false", "null", "and", "or", "class", "function", "return"
                                           , "<?php", "?>", "echo", "print"
                                           ]
                   , Token.reservedOpNames = [ "=", "==", "===", "->", ".", "+", "-", "*", "/", "%", "<", ">"
                                             , "and", "or", "||", "&&", "!", "++", "--" 
                                             ]
                   , Token.caseSensitive = False
                   }

lexer = Token.makeTokenParser langDef

phpString = stringLit lexer '"' <|> stringLit lexer '\''

identifier = Token.identifier lexer
reserved = Token.reserved lexer
float = Token.float lexer
stringTok = phpString
reservedOp = Token.lexeme lexer . string
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser [ParseResult]
whileParser = many (parsePHPCode <|> parsePlainText)

phpEof = try $ do
    optional $ char '\n'
    eof

parsePlainText :: Parser ParseResult
parsePlainText = liftM PlainText $ do
    c <- anyChar
    har <- manyTill anyChar (lookAhead (reserved "<?php") <|> phpEof)
    return (c : har)

parsePHPCode :: Parser ParseResult
parsePHPCode = do
    reserved "<?php"
    seq <- sequenceOfStmt
    optional (string "?>") <|> phpEof
    return $ PHPCode seq

sequenceOfStmt = liftM Seq $ many1 oneStatement

statementZeroOrMore = liftM Seq $ many oneStatement

statementZeroOrOne = liftM Seq $ option [] (liftM (:[]) oneStatement)

-- Match a valid PHP end of statement.
-- Must have ; after expression, unless closing tag
-- ?> comes immediately after
phpEnd = semi <|> try (string "?>")

-- Parse a single PHP statement
oneStatement :: Parser PHPStmt
oneStatement = choice [ ifStmt
                        , functionStmt
                        , returnStmt
                        , whileStmt
                        , forStmt
                        , echoStmt
                        , globalStmt
                        , staticStmt
                        , stmtExpr ]
    -- Special case for an expression that's a statement
    -- Expressions can be used without a semicolon in the end in ifs or whatever, 
    -- but a valid statement expression needs a semi in the end
    where stmtExpr = liftM Expression phpExpression <* phpEnd

staticStmt :: Parser PHPStmt
staticStmt = reserved "static" >> liftM Static (sepBy staticArg (Token.symbol lexer ",")) <* semi
    where staticArg = do
                char '$'
                name <- identifier
                defValue <- optionMaybe $ do
                    Token.symbol lexer "="
                    phpValue

                return $ StaticVar name defValue

globalStmt :: Parser PHPStmt
globalStmt = reserved "global" >> liftM Global plainVariableExpr <* semi

echoStmt :: Parser PHPStmt
echoStmt = do
        reserved "echo"
        -- echo take one arg only if parens are used, otherwise 1 or more
        args <- liftM (:[]) (parens phpExpression) <|> argList
        phpEnd
        return $ Echo args
    where argList = sepBy phpExpression (Token.symbol lexer ",")

returnStmt :: Parser PHPStmt
returnStmt = reserved "return" >> liftM Return phpExpression <* phpEnd

functionStmt :: Parser PHPStmt
functionStmt = do
        reserved "function"
        name <- identifier
        argDefs <- parens $ sepBy argDefExpr (optional (Token.symbol lexer ","))
        body <- braces statementZeroOrMore
        return $ Function name argDefs body
    where
        argDefExpr = do
            char '$'
            name <- identifier
            defValue <- optionMaybe $ do
                Token.symbol lexer "="
                phpValue

            return $ FunctionArgumentDef name defValue

whileStmt :: Parser PHPStmt
whileStmt = reserved "while" >> liftM2 While phpExpression (braces statementZeroOrMore <|> oneStatement)

forStmt :: Parser PHPStmt
forStmt = do
    reserved "for"
    (init, cond, iter) <- parens $ do
        minit <- sepBy phpExpression (Token.symbol lexer ",")
        semi
        mcond <- sepBy phpExpression (Token.symbol lexer ",")
        semi
        miter <- sepBy phpExpression (Token.symbol lexer ",")
        return (minit, mcond, miter)
    body <- braces statementZeroOrMore <|> do { s <- statementZeroOrOne; semi; return s }
    return $ For init cond iter body

ifStmt :: Parser PHPStmt
ifStmt = do
    reserved "if"
    cond <- parens phpExpression
    stmt1 <- braces statementZeroOrMore <|> oneStatement
    cont <- optionMaybe (elseIfStmt <|> elseStmt)
    return $ If cond stmt1 cont

elseStmt :: Parser ElseExpr
elseStmt = reserved "else" >> liftM Else (braces statementZeroOrMore <|> oneStatement)

elseIfStmt :: Parser ElseExpr
elseIfStmt = do
    reserved "elseif"
    cond <- parens phpExpression
    stmt <- braces statementZeroOrMore <|> oneStatement
    cont <- optionMaybe (elseIfStmt <|> elseStmt)
    return $ ElseIf cond stmt cont

assignExpr :: Parser PHPExpr
assignExpr = liftM2 Assign (plainVariableExpr <* reservedOp "=") phpExpression

plainVariableExpr :: Parser PHPVariable
plainVariableExpr = try varVarExpr <|> normalVariableExpr
    where
        varVarExpr = char '$' >> char '$' >> fmap PHPVariableVariable identifier

normalVariableExpr :: Parser PHPVariable
normalVariableExpr = char '$' >> fmap PHPVariable identifier

phpExpression :: Parser PHPExpr
phpExpression = buildExpressionParser phpOperators phpTerm

phpOperators = [ [Infix (reservedOp "*" >> return (BinaryExpr Multiply)) AssocLeft]
               , [Infix (reservedOp "/" >> return (BinaryExpr Divide)) AssocLeft]
               , [Infix (reservedOp "+" >> return (BinaryExpr Add)) AssocLeft]
               , [Infix (reservedOp "-" >> return (BinaryExpr Subtract)) AssocLeft]
               , [Infix (reservedOp "." >> return (BinaryExpr Concat)) AssocLeft]
               , [Infix (reservedOp "==" >> return (BinaryExpr Equals)) AssocLeft]
               , [Infix (reservedOp "===" >> return (BinaryExpr StrictEquals)) AssocLeft]
               , [Prefix (reservedOp "!" >> return Not)]
               , [Infix (reservedOp "&&" >> return (BinaryExpr And)) AssocLeft]
               , [Infix (reservedOp "||" >> return (BinaryExpr Or)) AssocLeft]
               , [Infix (reservedOp "<" >> return (BinaryExpr Less)) AssocLeft]
               , [Infix (reservedOp ">" >> return (BinaryExpr Greater)) AssocLeft]
               ]

phpTerm = choice [ parens phpExpression
                ,  try issetExpr
                ,  try printExpr
                ,  try functionCallExpr
                ,  try assignExpr
                ,  variableExpr
                ,  liftM Literal phpValue ]

issetExpr :: Parser PHPExpr
issetExpr = reserved "isset" >> liftM Isset (parens $ sepBy1 plainVariableExpr (Token.symbol lexer ","))

variableExpr :: Parser PHPExpr
variableExpr = do
        prefixOp <- unaryOp
        var <- plainVariableExpr
        case prefixOp of
          Just op -> return $ UnaryExpr Before (mkUnaryOp op) var
          Nothing -> do
              postOp <- unaryOp
              case postOp of
                Nothing -> return $ Variable var
                Just op -> return $ UnaryExpr After (mkUnaryOp op) var
    where
        unaryOp = optionMaybe (try (reservedOp "++") <|> try (reservedOp "--"))

functionCallExpr :: Parser PHPExpr
functionCallExpr = try varCall <|> nameCall
    where
        varCall = do
            var <- plainVariableExpr
            args <- parens argList
            return $ Call (FunctionCallVar var) args
        nameCall = do
            name <- identifier
            args <- parens argList
            return $ Call (FunctionCall name) args
        argList = sepBy phpExpression (Token.symbol lexer ",")

printExpr :: Parser PHPExpr
printExpr = reserved "print" >> liftM Print phpExpression

phpValue :: Parser PHPValue
phpValue = choice [ reserved "true" >> return (PHPBool True)
                  , reserved "false" >> return (PHPBool False)
                  , reserved "null" >> return PHPNull
                  , liftM (either PHPInt PHPFloat) (Token.naturalOrFloat lexer)
                  , liftM PHPString stringTok ]

parseString :: String -> [ParseResult]
parseString str = case parse whileParser "" str of
                    Left e -> error $ show e
                    Right r -> case last r of
                                 PlainText "\n" -> init r
                                 _              -> r
