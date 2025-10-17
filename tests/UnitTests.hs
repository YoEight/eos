{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad ((<=<))

import Math.Eos.Ast
import Math.Eos.Lexical (tokenize)
import Math.Eos.Parser (runParser)
import Math.Eos.Sym

import Test.HUnit

parseExpr :: String -> Either String Ast
parseExpr = runParser parseAst <=< tokenize

test_tokenize_sums :: Test
test_tokenize_sums = "tokenize a sum properly" ~: go
  where
    go = case tokenize "1 + 2 + 3" of
        Left s -> assertFailure s
        Right syms -> syms @=? [SymNumber 1, SymOp Add, SymNumber 2, SymOp Add, SymNumber 3]

test_parser_exp :: Test
test_parser_exp = "parser exp properly" ~: go
  where
    go = case parseExpr "1 + 2 ^ 3" of
        Left s -> assertFailure s
        Right ast -> ast @=? binary Add (number 1) (binary Exp (number 2) (number 3))

tests :: Test
tests =
    TestList
        [ TestLabel "lexical analysis" lexical_tests
        , TestLabel "parsing" parser_tests
        ]
  where
    lexical_tests = TestList [test_tokenize_sums]
    parser_tests = TestList [test_parser_exp]

main :: IO ()
main = runTestTTAndExit tests
