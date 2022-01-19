
module Parser where
import Text.Megaparsec
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Control.Monad.State
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.List
import Types
import Eval

parseExpr :: Parser Expr
parseExpr = choice [ 
                     parseInt,
                     parseBool,
                     parseOp,
                     parseCall,
                     parseList,
                     parseVar
                   ]

parseInt :: Parser Expr
parseInt = NumLit . NumberAtom <$> lexeme (L.decimal)     

parseBool :: Parser Expr
parseBool = BoolLit . BoolAtom <$> (lexeme . try) (parseStr' >>= bool)
        where 
                parseStr' = symbol "true" <|> symbol "false"
                bool "true"  = return True
                bool "false" = return False

parseOp :: Parser Expr 
parseOp = try . parens $ do
        op <- symbolOperationParser
        (opToExpr op) <$> many parseExpr

symbolOperationParser :: Parser String
symbolOperationParser = choice $ fmap symbol symbolList

symbolList :: [String]
symbolList= [
             "+", "-",
             "<=", "<",
             "=", "*"
            ] 
opToExpr :: String -> ([Expr] -> Expr)
opToExpr sym = case lookup sym listOfSymbols of
                 Just a -> a
                 Nothing -> \_ -> NumLit NIL
        where
                listOfSymbols = [
                                 ("+",Sum), ("-",Subtr),
                                 ("*", Product), ("<",Types.LT),
                                 ("<=", LTEQ), ("=", Types.EQ) 
                                ] 

parseList :: Parser Expr
parseList = try . parens $ do
        notFollowedBy (symbolOperationParser)
        List <$> (many parseExpr)

parseVar :: Parser Expr
parseVar = do
        var <- identify
        return $ Var var

assignVar :: Parser Statement
assignVar = do 
        symbol "set"
        variable <- identify
        value <- parseInt <|> parseBool
        return $ Assign variable value


identify :: Parser String
identify = (lexeme . try) (word >>= check)
        where
                word = (:) <$> C.letterChar <*> many C.alphaNumChar
                check x = if x `elem` reserved
                             then fail $ "keyword " ++ show x ++ " cannot be identifier"
                             else return x

spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 lineComment blockComment
        where
                lineComment = L.skipLineComment ";"
                blockComment = L.skipBlockComment ";|" "|;"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Tokens String -> Parser (Tokens String)
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensSquare :: Parser a -> Parser a
parensSquare = between (symbol "[") (symbol "]")

string :: Parser a -> Parser a
string = between (symbol "\"") (symbol "\"")

reserved :: [String]
reserved = ["defun", "set", "defvar", "if", "true", "false", "print"]

parseStatement :: Parser Statement
parseStatement = choice [parsePrint, parseAssign, parseIf, parseLet, parseDefun, parseWhile,  parseExprStatement ]
-- order matters

parseProgram :: Parser [Statement]
parseProgram = many parseStatement

parseExprStatement :: Parser Statement
parseExprStatement = Expression <$> parseExpr

--TODO
parsePrint :: Parser Statement
parsePrint = try . parens $ do
        symbol "print"
        statement <- parseStatement
        return $ Print statement

parseAssign :: Parser Statement
parseAssign = try . parens $ do 
        symbol "set"
        var <- identify
        expr <- parseExpr
        return $ Assign var expr

parseLet :: Parser Statement
parseLet = try . parens $ do
        symbol "let"
        localVars <- many parseAssign
        statement <- parseStatement
        return $ Let localVars statement

parseWhile :: Parser Statement
parseWhile = try . parens $ do
        symbol "while"
        pred <- parseExpr
        body <- many parseStatement
        return $ While pred body

parseIf :: Parser Statement
parseIf = try . parens $ do
        symbol "if"
        pred <- parseExpr
        statementTrue  <- parseStatement
        statementFalse <- parseStatement
        return $ If pred statementTrue statementFalse

parseDefun :: Parser Statement
parseDefun = try. parens $ do 
        symbol "defun"
        name <- identify
        args <- parensSquare $ many identify
        body <- many parseStatement
        return $ DeFunc name args body

parseCall :: Parser Expr
parseCall = try . parens $ do
        name <- identify 
        args <- many parseExpr
        return $ Call name args
          

         
        
testProgram' :: String
testProgram' = unlines [ 
                     --    "(set i (+ 1 2))\n",
                     --    "(set j (+ 2 3))\n",
                     --    "(if (< i j) (print true) (print false))\n",
                     --    "(let (set i 22)\n (set j 33)\n (print j))\n",
                         "(defun fib [i]\n (if (= i 1) 0 \n (if (= i 2) 1 \n 3 ))) (print (fib 0))"
                        --if (= i 1) (0)\n (if (= i 2) (1) \n (+ (fib (- n 1)) (fib (- n 2)))))"
                       ]

testProgramSum :: String
testProgramSum = "(print (! 2 (+ true 3) 3))"

testProgFib :: String
testProgFib = " (defun fib [n]\n (if (= n 1) 0 \n (if (= n 2) 1 \n (+ (fib (- n 1)) (fib (- n 2)))))) (print (fib 1))"

test = "(defun f [i] (if (= i 0) 1 (if (= i 1) 2  (- i 1))))"

--[DeFunc "fib" ["i"] 
--[If (EQ [Var "i",NumLit 1]) (Expression (NumLit 0)) (If (EQ [Var "i",NumLit 2]) (Expression (NumLit 1)) (Expression (NumLit 3)))],Print (Expression (Call "fib" [NumLit 0]))]

t' = parseTest parseProgram test
runProgram = do
        source <- readFile "source.lisp"
        case parse parseProgram "" source of
               Left e  -> putStr (errorBundlePretty e)
               Right ast -> do 
                       comp <- runExceptT $ evalStateT (evalProgram ast) []
                       case comp of 
                              Left e -> print e
                              Right a -> print a

