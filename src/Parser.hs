{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

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
-- data SExpression a = Atom a | List [SExpression a]



--evalExpr' :: Env -> Expr -> EvalM Value
--evalExpr' env (Var var) = evalVar' var
--evalExpr' env (Sum s) = return $ foldl (add) (NumberAtom 0) . map (\x -> evalExpr' x) $ s

--add :: Env ->  EvalM Value -> EvalM Value -> Either String Value
--add env x y = do
--        x' <- runExceptT $ evalStateT x env
--        y' <- runExceptT $ evalStateT y env
--        return $ _ + _


--evalExpr'
--evalExpr' = do
--        env' <- y
--        case env' of 
--          Left s -> print s
--          Right p -> do
--                res <- z env'
--                case res of 
--                  Left s -> print s
--                  Right p -> print p
--        where
--            x = [("asd",NumberAtom 2)]
--            y = runExceptT $ execStateT (evalDefineVar ("asad",NumberAtom 3)) x
--            z = runExceptT $ evalStateT (evalVar' "asad") 
--
--evalExpr'' s = do 
--        x <- runExceptT $ evalStateT (evalDefineVar s >> evalVar' s') []
--        case x of 
--          Left s -> print s 
--          Right s -> print s
--        
--        where (s', v) = s
        
--evalSubtr :: Expr -> EvalM Value
--evalSubtr (Sum []) = 0
--evalSubtr (Sum xs) = undefined

--evalProduct :: Expr -> Value
--evalProduct (Product [x]) = NumberAtom x 
--evalProduct (Product xs)  = NumberAtom $ foldl (*) 1 xs


                

-- TODO
--parseString :: Parser Expr
--parseString = StringAtom <$> string (many (C.alphaNumChar <|> C.spaceChar))
--parseString = String' <$> string (many (C.alphaNumChar <|> C.spaceChar))

parseExpr :: Parser Expr
parseExpr = choice [ 
                     parseInt,
                     parseBool,
                     parseOp,
                     parseList,
                     parseVar
                   ]

parseInt :: Parser Expr
parseInt = NumLit . NumberAtom <$> lexeme ( L.signed spaceConsumer L.decimal)     

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
        var <- lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar
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

string :: Parser a -> Parser a
string = between (symbol "\"") (symbol "\"")

reserved :: [String]
reserved = ["defun", "set", "defvar", "if", "true", "false", "print"]

parseStatement :: Parser Statement
parseStatement = choice [parsePrint, parseAssign, parseIf, parseExprStatement]
-- order matters

parseProgram :: Parser [Statement]
parseProgram = many parseStatement

parseExprStatement :: Parser Statement
parseExprStatement = Expression <$> parseExpr

--TODO
parseWhile :: Parser Statement
parseWhile = undefined

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

parseIf :: Parser Statement
parseIf = try . parens $ do
        symbol "if"
        pred <- parseExpr
        statementTrue <- parseStatement
        statementFalse <- parseStatement
        return $ If pred statementTrue statementFalse


testProgram' :: String
testProgram' = unlines [ 
                         "(set i (+ 1 2))\n",
                         "(set j (+ 2 3))\n",
                         "(if (< i j) (print true) (print false))\n" 
                       ]

testProgramSum :: String
testProgramSum = "(print (! 2 (+ true 3) 3))"


runProgram = case parse parseProgram "" testProgramSum of
               Left e  -> print e
               Right ast -> do 
                       comp <- runExceptT $ evalStateT (evalProgram ast) []
                       case comp of 
                              Left e -> print e
                              Right a -> print a






