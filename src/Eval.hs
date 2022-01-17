{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Eval where
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

evalExpr :: Expr -> Value
evalExpr (BoolLit v) = v 
evalExpr (NumLit v) = v 
evalExpr s@(Sum _) = evalSum s
evalExpr s@(Subtr _) = evalSubtr s
evalExpr p@(Product _) = evalProduct p


-- (+ 2 3 4) = 9
--

evalSum :: Expr -> Value
evalSum (Sum []) = NumberAtom 0
evalSum (Sum x)  = foldl (+) (NumberAtom 0) . map (\x -> evalSExprNum x) $ x

evalSExprNum :: Expr -> Value
evalSExprNum x = case evalExpr x of
                   n@(NumberAtom x) -> n
                   _ -> error "Expression is not a number"

evalSubtr :: Expr -> Value
evalSubtr (Subtr []) = NumberAtom 0
evalSubtr (Subtr x)  = foldl (-) (NumberAtom 0) . map (\x -> evalExpr x) $ x

evalProduct :: Expr -> Value
evalProduct (Product []) = NumberAtom 1
evalProduct (Product x)  = foldl (*) (NumberAtom 1) . map (\x -> evalExpr x) $ x

evalEq :: Expr -> Value
evalEq (Types.EQ [])     = error "Too few arguments"
evalEq (Types.EQ [x])    = BoolAtom True
evalEq (Types.EQ [x,xx]) = BoolAtom $ (evalExpr x) == (evalExpr xx)
evalEq (Types.EQ _)      = NIL

evalLT :: Expr -> Value
evalLT (Types.EQ [])       = error "Too few arguments"
evalLT (Types.EQ [x])      = BoolAtom True
evalLT (Types.EQ [x,xx])   = BoolAtom $ (evalExpr x)< (evalExpr xx)
evalLT (Types.EQ _)        = NIL

unpackBoolAtom :: Value -> Bool
unpackBoolAtom (BoolAtom b) = b

evalDivision :: Expr -> Value
evalDivision (Division []) = NumberAtom 1
--evalDivision (Divison x) =  foldl (\) (NumberAtom 1) . map (\x -> evalExpr x) $ x
-- TODO check eval expr 

declareVar :: Env -> String -> Expr -> Env
declareVar env vname expr = (vname ,evalExpr expr) : env

evalVar :: Env -> String -> Value
evalVar env vname = case lookup vname env of
                      Just (v) -> v
                      Nothing -> error $ "Variable: " ++ vname ++ " is not declared"

evalCheckNum x = undefined

evalMExpr :: Env -> Expr -> EvalM Value
evalMExpr env expr = execStateT (return $ env) (evalSum expr)

--evalSum' :: Expr -> String
--evalSum' (Sum []) = show $ NumberAtom 0
--evalSum' (Sum x)  = show $ foldl (+) (NumberAtom 0) . map (\x -> evalSExprNum x) $ x

evalVar' :: Expr -> EvalM Value
evalVar' (Var variable) = do
        env <- get
        case lookup variable env of 
          Just c -> return $ c
          Nothing -> throwError $ "Variable " ++ variable ++ " is not defined"

evalNum' :: Expr -> EvalM Value
evalNum' e = do 
        e' <- evalExpr' e
        case e' of 
          v@(NumberAtom n) -> return v
          _                -> throwError "Not a number"
           
evalBool' :: Expr -> EvalM Value
evalBool' e = do 
        e' <- evalExpr' e
        case e' of 
          v@(BoolAtom n) -> return v
          _              -> throwError "Not a boolean"

evalFunc :: Expr -> EvalM Value
evalFunc (Function n args body argsnum ) = do
        if gotArgs /= argsnum 
           then
              throwError errmsg
        else undefined 
                 where
                   errmsg = "Function " ++ n ++ " expects " ++ (show argsnum) ++ " arguments, but got: " ++ show gotArgs
                   gotArgs = length args

execBody :: [Expr] -> [Expr] -> EvalM Value
execBody args body = do
        env <- get
        undefined       
        
fromValueToBool :: Value -> Bool
fromValueToBool (BoolAtom b) = b

evalExpr' :: Expr -> EvalM Value
evalExpr' (NumLit n) = return n
evalExpr' (BoolLit n) = return n
evalExpr' s@(Sum n) = evalSum' s
evalExpr' p@(Product n) = evalProd' p
evalExpr' v@(Var n) = evalVar' v
evalExpr' l@(Types.LT n)  = evalLt' l
          

testexpr :: Expr
testexpr = (Sum [NumLit (NumberAtom 2), NumLit (NumberAtom 0), Product [NumLit (NumberAtom 1), NumLit (NumberAtom 2), Var "i"]])

evalSum' :: Expr -> EvalM Value
evalSum' (Sum [])  = return $ NIL
evalSum' (Sum [a]) = evalExpr' a
evalSum' (Sum a)   = do 
        let a' = map (\n -> evalNum' n) a
        foldl (\x y -> do
                x' <- x
                y' <- y
                return $ x' + y') (return $ NumberAtom 0) a'
       
evalSub' :: Expr -> EvalM Value
evalSub' (Subtr [])  = return $ NIL
evalSub' (Subtr [a]) = evalExpr' a
evalSub' (Subtr a)   = do 
        let a' = map (\n -> evalNum' n) a
        foldl (\x y -> do
                x' <- x
                y' <- y
                return $ x' - y') (return $ NumberAtom 0) a'


evalProd' :: Expr -> EvalM Value
evalProd' (Product [])  = return $ NIL
evalProd' (Product [a]) = evalExpr' a
evalProd' (Product a)   = do 
        let a' = map (\n -> evalNum' n) a
        foldl (\x y -> do
                x' <- x
                y' <- y
                return $ x' * y') (return $ NumberAtom 1) a'

evalEq' :: Expr -> EvalM Value
evalEq' (Types.EQ [])      = return $ NIL
evalEq' (Types.EQ [a])     = evalExpr' a
evalEq' (Types.EQ [a,b])   = do 
        a' <- evalExpr' a
        b' <- evalExpr' b
        return $ BoolAtom (a' == b')
evalEq' (Types.EQ _)       = return $ NIL

evalLt' :: Expr -> EvalM Value
evalLt' (Types.LT [])      = return $ NIL
evalLt' (Types.LT [a])     = evalExpr' a
evalLt' (Types.LT [a,b])   = do 
      a' <- evalExpr' a
      b' <- evalExpr' b
      return $ BoolAtom (a' < b')
evalLt' (Types.LT _)       = return $ NIL

evalLteq' :: Expr -> EvalM Value
evalLteq' (Types.LTEQ [])      = return $ NIL
evalLteq' (Types.LTEQ [a])     = evalExpr' a
evalLteq' (Types.LTEQ [a,b])   = do 
      a' <- evalExpr' a
      b' <- evalExpr' b
      return $ BoolAtom (a' <= b')
evalLteq' (Types.LTEQ _)       = return $ NIL
--evalDiv' :: Expr -> EvalM Value
--evalDiv' (Division [])   = return $ NIL
--evalDiv' (Division [a])  = evalExpr' a
--evalDiv' (Division (a:as)) = do 
--        let as' = map (\n -> evalNum' n) as
--        foldl (\x y -> do
--                x' <- x
--                y' <- y
--                return $ x' `div` y') (evalExpr' a) as'
        
--evalDefineVar :: Expr -> EvalM Value
--evalDefineVar e = do
--        env <- get
--        case e of 
--          a@(Assign name value) -> do 
--                  value' <- evalExpr' value
--                  let var = (name, value')
--                  case lookup name env of
--                     Just a -> do
--                             put $ var : (deleteBy (\(x,y) (x',y') -> x == x' ) var env)
--                             return NIL
--                     Nothing -> do 
--                             put $ var : env
--                             return NIL
--
--          _                     -> throwError "Not an assignment"


evalStatement :: Statement -> EvalM ()
evalStatement (Expression e) = do 
        evalExpr' e
        return ()
evalStatement (Seq s s') =  evalStatement s >> evalStatement s'
evalStatement (If e s s') = do
        e' <- evalBool' e -- BoolAtom False
        let pred = unpackBoolAtom e'
        if pred then evalStatement s else evalStatement s'
evalStatement (Assign name value) =  do
        env <- get
        value' <- evalExpr' value
        let var = (name, value')
        case lookup name env of
           Just a -> put $ var : (deleteBy (\(x,y) (x',y') -> x == x' ) var env)
           Nothing -> put $ var : env

evalStatement (Print s) = do
        case s of 
          (Expression e) -> do
                  val <- evalExpr' e
                  liftIO $ print val
          _              -> throwError $  "Can't print statement: " ++ show s

        
--  (set i 2)
--  (set j (+ 2 i))
--  (print 2)
--  (print j)

testStatement :: [Statement]
testStatement = 
        [
         Assign "i" (NumLit (NumberAtom 2)),
         Assign "j" (Sum [NumLit (NumberAtom 2), Var "i"]),
         Print (Expression (NumLit (NumberAtom 2))),
         Print (Expression (Var "j"))
        ]

testProgram1 :: [Statement]
testProgram1 = 
        [
         Assign "i" (NumLit (NumberAtom 2)),
         Assign "j" (Sum [NumLit (NumberAtom 2), Var "i"]),
         Print (If (Types.LT [(Var "i"), (NumLit (NumberAtom 1))]) (Print (Expression (BoolLit (BoolAtom True)))) (Print (Expression (BoolLit (BoolAtom False))))),
         Print (Expression (Var "j"))
        ]

evalProgram :: [Statement] -> EvalM ()
evalProgram prog = mapM_ evalStatement prog
