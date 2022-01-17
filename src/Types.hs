{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Text.Megaparsec
import Data.Void
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.IO.Class

type Parser = Parsec Void String

data Value = NumberAtom Int | BoolAtom Bool | NIL
        deriving (Eq, Ord)

data Expr = BoolLit Value
        | NumLit Value 
        | Var String
        | List [Expr]
        | Sum [Expr]
        | Subtr [Expr]
        | Product [Expr]
        | Division [Expr]
        | Function { 
                     name :: String,
                     args :: [Expr],
                     body :: [Expr],
                     numArgs :: Int
                   }
        | EQ [Expr]
        | LT [Expr]
        | LTEQ [Expr]
        deriving (Eq, Ord, Show)       

data Statement = Skip 
               | Expression Expr 
               | Seq Statement Statement 
               | If Expr Statement Statement
               | Assign String Expr
               | Print Statement
               deriving (Eq, Ord, Show)



instance Num Value where
        (+) (NumberAtom a) (NumberAtom b) = NumberAtom $ a + b
        (-) (NumberAtom a) (NumberAtom b) = NumberAtom $ a - b
        (*) (NumberAtom a) (NumberAtom b) = NumberAtom $ a * b
        abs (NumberAtom a)                = NumberAtom $ abs a
        signum (NumberAtom a)             = NumberAtom $ signum a
        fromInteger a                     = NumberAtom $ fromInteger a
instance Show Value where
        show (NumberAtom a) = show a
        show (BoolAtom a)   = show a
        show (NIL)          = "Nil"

type Env   = [(String, Value)]
-- (s -> m (a, s)) -> StateT s (m (Either e a) -> ExceptT e m a) a
-- 
type EvalM = StateT Env (ExceptT String IO) 

instance Num Expr where
        (+) (NumLit a) (NumLit b) = NumLit $ a + b
        (-) (NumLit a) (NumLit b) = NumLit $ a - b
        (*) (NumLit a) (NumLit b) = NumLit $ a * b
        abs (NumLit a)            = NumLit $ abs a
        signum (NumLit a)         = NumLit $ signum a
        fromInteger a             = NumLit $ fromInteger a

