
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
import Debug.Trace
import Types


unpackBoolAtom :: Value -> Bool
unpackBoolAtom (BoolAtom b) = b

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

evalFunc :: Value -> EvalM Value
evalFunc (Function n vars args body argsnum ) = do
        if gotArgs /= argsnum 
           then
              throwError errmsg
        else do env <- get
                exprList <- mapM (evalExpr') args
                let vars' = zip vars exprList
                put (vars' ++ env)
                val <- execBody body
                put env
                return val
                 where
                   errmsg = "Function " ++ n ++ " expects " ++ (show argsnum) ++ " arguments, but got: " ++ show gotArgs
                   gotArgs = length args

evalCall :: Expr -> EvalM Value 
evalCall (Call name args) = do 
        env <- get
        case lookup name env of
          Just (Function name vars _ body n) -> evalFunc (Function name vars args body n )
          Just e  -> throwError $ "expected function but got: " ++ show e
          Nothing -> throwError $ "Undefined function " ++ name 
evalCall e = throwError $ "expected function but got: " ++ show e
                


defineFunction :: Statement -> EvalM Value
defineFunction (DeFunc name args body) = do
        env <- get  
        let func = Function name args [] body (length args)
        put $ (name, func) : env
        return $ func

execBody :: [Statement] -> EvalM Value
execBody body = do
        env <- get
        mapM_ evalStatement (init body)
        evalStatement $ last body
        
fromValueToBool :: Value -> Bool
fromValueToBool (BoolAtom b) = b

evalExpr' :: Expr -> EvalM Value
evalExpr' (NumLit n)      = return n
evalExpr' (BoolLit n)     = return n
evalExpr' s@(Sum n)       = evalSum' s
evalExpr' s@(Subtr n)     = evalSub' s
evalExpr' s@(Types.EQ n)  = evalEq' s
evalExpr' p@(Product n)   = evalProd' p
evalExpr' v@(Var n)       = evalVar' v
evalExpr' l@(Types.LT n)  = evalLt' l
evalExpr' c@(Call n a)    = evalCall c
evalExpr' x               = throwError $ "Expected expression, but got:  " ++ show x
          

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
        foldr (\x y -> do
                x' <- x
                y' <- y
                return $ (-) x' y') (return $ NumberAtom 0) a'


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

evalStatement :: Statement -> EvalM Value
evalStatement (Expression e) = do 
        e <- evalExpr' e
        return e
evalStatement (If e s s') = do
        e' <- evalBool' e 
        let pred = unpackBoolAtom e'
        if pred then evalStatement s else evalStatement s'
evalStatement a@(Assign name value) = evalAssign a
evalStatement (Print s) = do
        case s of 
          (Expression e) -> do
                  val <- evalExpr' e
                  liftIO $ print val
                  return NIL
          anything -> do 
                  val <- evalStatement anything 
                  liftIO $ print val
                  return NIL
          _              -> throwError $  "Can't print statement: " ++ show s
evalStatement w@(While cond body) = do
        pred <- evalBool' cond
        let pred' = unpackBoolAtom pred
        if pred' then do
                body' <- mapM_ evalStatement body
                evalStatement w
                else return NIL
evalStatement l@(Let _ _) = evalLet l 
evalStatement d@(DeFunc _ _ _ ) = defineFunction d


evalAssign :: Statement -> EvalM Value
evalAssign (Assign name value) = do
        env <- get
        value' <- evalExpr' value
        let var = (name, value')
        case lookup name env of
           Just a -> do 
                   put $ var : (deleteBy (\(x,y) (x',y') -> x == x' ) var env)
                   return NIL
           Nothing -> do put $ var : env
                         return NIL
evalAssign _ = throwError "Expected assignment"


evalLet :: Statement -> EvalM Value
evalLet (Let vars rest) = do
        env <- get
        mapM_ evalAssign vars
        evalStatement rest 
        put env
        return NIL


testStatement :: [Statement]
testStatement = 
        [
         Assign "i" (NumLit (NumberAtom 2)),
         Assign "j" (Sum [NumLit (NumberAtom 2), Var "i"]),
         DeFunc ("print-i-j") ["c","k"] [(Print (Expression (Var "i"))), Assign "i" (NumLit (NumberAtom 100)), (Print (Expression (NumLit (NumberAtom 55))))],
         Print (Expression (Call "print-i-j" [NumLit (NumberAtom 2),(Var "j")])),
         Print (Expression (Var "i"))
        ]

testFunc :: Statement 
testFunc = DeFunc ("print-i-j") ["i","j"] [(Print (Expression (Var "i"))), (Print (Expression (Var "j"))) ]
testLet :: Statement 
testLet = Let [Assign "i" (NumLit (NumberAtom 66))] (Print (Expression (Var "i")))

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

