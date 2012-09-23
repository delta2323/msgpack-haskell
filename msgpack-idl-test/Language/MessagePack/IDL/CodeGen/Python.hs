{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.Python (
  Config(..),
  generate,
  ) where

import Text.Peggy
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
    { configFilePath :: FilePath }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} TestSet {..} = do
  let body = LT.unlines (map genTest testCases)
      service = map toLower testSetName
      testClassName = (capitalize testSetName) ++ "Test"
      
--  createDirectoryIfMissing True (takeBaseName configFilePath);
--  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile "__init__.py" $ templ configFilePath [lt|
|]

  LT.writeFile "test.py" $ templ configFilePath [lt|
#!/usr/bin/env python

import unittest
from test_target.client import #{service}
from test_target.types  import *

host = "localhost"
port = 5000
timeout = 10

class #{testClassName}(unittest.TestCase):
#{body}

if __name__ == '__main__':
  test_suite = unittest.TestLoader().loadTestsFromTestCase(#{testClassName})
  unittest.TextTestRunner().run(test_suite)
|]

capitalize :: String -> String
capicalize [] = []
capitalize s = (toUpper $ head s) : tail s

genArgs :: [Expression] -> LT.Text
genArgs = LT.intercalate [lt|, |] . map genExpression

genExpression :: Expression -> LT.Text
genExpression (Call fun args) = [lt|self.#{fun}(#{genArgs args})|]
genExpression (Var v) = [lt|#{v}|]
genExpression (ConstInt n) = [lt|#{show n}|]
genExpression (ConstDouble x) = [lt|#{show x}|]
genExpression (ConstString c) = [lt|"#{c}"|]
genExpression (ConstList l) = [lt|[#{genArgs l}]|]

genAssert :: Operator -> Expression -> Expression -> LT.Text
genAssert op exp act =
  let a = genArgs [exp, act] in
  case op of
  Equal -> [lt|self.assertEqual(#{a})|]

genStatement :: Statement -> LT.Text
genStatement (Assign var exp) = [lt|#{var} = #{genExpression exp}|]
genStatement (ExpressionState e) = [lt|#{genExpression e}|]
genStatement (Assert exp op act) = [lt|#{genAssert op exp act}|]

indentLines :: Int -> [LT.Text] -> LT.Text
indentLines n lines =
  let space = replicate (n * 2) ' ' in
  LT.unlines (map (LT.append (LT.pack space)) lines)

genTest :: TestCase -> LT.Text
genTest test = 
  let ss = map genStatement (statements test) in
  let body = indentLines 2 ss in
  [lt|  def test_#{testName test}(self):
#{body}|]

{--
genTests :: Spec -> LT.Text
genTests TestSet{..} tests =
  let body = LT.unlines (map genTest tests) in
  [lt|#!/usr/bin/env python

import unittest

class MyTest(unittest.TestCase):
#{body}

if __name__ == '__main__':
  test_suite = unittest.TestLoader().loadTestsFromTestCase(MyTest)
  unittest.TextTestRunner().run(test_suite)
|]
--}

{--
printTest :: Either ParseError Spec -> IO ()
printTest (Left _) = print "error"
printTest (Right test) = LT.putStrLn (genTests test)
--}

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
# This file is auto-generated from #{filepath}
# *** DO NOT EDIT ***

#{content}
|]
