{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Ruby (
  Config(..),
  generate,
  ) where

import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text
import System.Directory

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configModule :: String
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} TestSet {..} = do
  let body = LT.unlines (map genTest testCases)
      testClassName = (capitalize testSetName) ++ "Test"
  LT.writeFile "test.rb" $ templ configFilePath [lt|
#!/usr/bin/env ruby

require 'test/unit'
DIR = File.dirname(__FILE__)
require "#\{DIR}/test_target/client"
require "#\{DIR}/test_target/types"

HOST = "localhost"
PORT = 5000
TIMEOUT = 10

class #{testClassName} < Test::Unit::TestCase
  def setup
  end

  def teardown
  end

  #{body}
end
|]

capitalize :: String -> String
capicalize [] = []
capitalize s = (toUpper $ head s) : tail s

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***

#{content}
|]

indentLines :: Int -> [LT.Text] -> LT.Text
indentLines n lines =
  let space = replicate (n * 2) ' ' in
  LT.unlines (map (LT.append (LT.pack space)) lines)

genTest :: TestCase -> LT.Text
genTest Test {..} = 
  let ss = map genStatement statements
      body = indentLines 2 ss in
  [lt|
  def test_#{testName}
#{body}
  end
|]

genStatement :: Statement -> LT.Text
genStatement Assign {..} = [lt|#{variable} = #{genExpression assinedValue}|]
genStatement (ExpressionState e) = genExpression e 
genStatement Assert {..} = genAssert comparator expected actual

genExpression :: Expression -> LT.Text
genExpression Call {..} = 
  let args_text = LT.intercalate [lt|, |] $ map genExpression args in
  [lt|#{methodName}(#{args_text})|]
genExpression (Var v) = [lt|#{v}|]
genExpression (ConstInt n) = [lt|#{show n}|]
genExpression (ConstDouble x) = [lt|#{show x}|]
genExpression (ConstString s) = [lt|"#{s}"|]
genExpression (ConstList ls) = 
  let lists = LT.intercalate [lt|, |] $ map genExpression ls in
  [lt|[#{lists}]|]

genOperator :: Operator -> LT.Text
genOperator Equal = [lt|==|]
genOperator GreaterEqual = [lt|>=|]
genOperator GreaterThan = [lt|>|]
genOperator LowerEqual = [lt|<=|]
genOperator LowerThan = [lt|<|]

genAssert :: Operator -> Expression -> Expression -> LT.Text
genAssert op exp act = 
  let exp_text = genExpression exp
      act_text = genExpression act
      op_text  = genOperator op in
  case op of
  Equal -> [lt|assert_equal(#{exp_text}, #{act_text})|]
  _ -> [lt|assert(#{exp_text} #{op_text} #{act_text})|]