
module Types where

import Text.Megaparsec
import Data.Void
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.IO.Class

type Parser = Parsec Void String

data Value = NumberAtom Int 
           | BoolAtom Bool
           | NIL
           |  Function { 
                     name :: String,
                     vars :: [String],
                     args :: [Expr],
                     body :: [Statement],
                     numArgs :: Int
                       }
        deriving (Eq, Ord)

data Expr = BoolLit Value
        | NumLit Value 
        | Var String
        | List [Expr]
        | Call String [Expr]
        | Sum [Expr]
        | Subtr [Expr]
        | Product [Expr]
        | Division [Expr]
        | EQ [Expr]
        | LT [Expr]
        | LTEQ [Expr]
        deriving (Eq, Ord, Show)       

data Statement = Skip 
               | Expression Expr 
               | If Expr Statement Statement
               | Assign String Expr
               | Print Statement
               | Let [Statement] Statement
               | DeFunc String [String] [Statement]
               | While Expr [Statement]
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
type EvalM = StateT Env (ExceptT String IO) 

instance Num Expr where
        (+) (NumLit a) (NumLit b) = NumLit $ a + b
        (-) (NumLit a) (NumLit b) = NumLit $ a - b
        (*) (NumLit a) (NumLit b) = NumLit $ a * b
        abs (NumLit a)            = NumLit $ abs a
        signum (NumLit a)         = NumLit $ signum a
        fromInteger a             = NumLit $ fromInteger a

