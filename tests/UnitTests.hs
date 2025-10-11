{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Math.Eos.Lexical (tokenize)
import Math.Eos.Sym

import Test.HUnit

test_tokenize_sums :: Test
test_tokenize_sums = "tokenize a sum properly" ~: go
  where
    go = case tokenize "1 + 2 + 3" of
        Left s -> assertFailure s
        Right syms -> syms @=? []

tests :: Test
tests = TestList [TestLabel "lexical analysis" lexical_tests]
  where
    lexical_tests = TestList [test_tokenize_sums]

main :: IO ()
main = runTestTTAndExit tests
