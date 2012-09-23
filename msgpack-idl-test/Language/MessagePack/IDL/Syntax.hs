{-# LANGUAGE DeriveDataTypeable #-}
module Language.MessagePack.IDL.Syntax where

import Data.Data
import qualified Data.Text as T

data Spec 
  = TestSet
    { testSetName :: String
    , testCases   :: [TestCase]
    }
  deriving (Eq, Show)

data Operator
  = Equal
  | GreaterEqual
  | GreaterThan
  | LowerEqual
  | LowerThan
  deriving (Eq, Show)

data Expression
  = Call
    { methodName :: String
    , args :: [Expression]
    }
  | Var String
  | ConstInt Int
  | ConstDouble Double
  | ConstString String
  | ConstList [Expression]
  deriving (Eq, Show)

data Statement
  = Assign
    { variable :: String,
      assinedValue :: Expression
    }
  | ExpressionState Expression
  | Assert
    { expected :: Expression
    , comparator :: Operator
    , actual :: Expression }
  deriving (Eq, Show)

data TestCase
  = Test
    { testName :: String
    , statements :: [Statement]
    }
  deriving (Eq, Show)

