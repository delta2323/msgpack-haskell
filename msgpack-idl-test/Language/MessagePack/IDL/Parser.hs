{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Language.MessagePack.IDL.Parser (
  tests
  ) where

import Data.Maybe
import qualified Data.Text as T
import Text.Peggy
import Text.Peggy.CodeGen.TH

import Language.MessagePack.IDL.Syntax

genDecs $(peggyFile "mpidl-test.peggy")
