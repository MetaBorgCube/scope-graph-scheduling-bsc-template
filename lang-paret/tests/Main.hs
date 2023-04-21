module Main where

import Test.HUnit

import Control.Exception (catch, ErrorCall)
import Syntax (Expr(..), Type(..))
import TypeCheck (runTC, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)

runTCTest :: Expr -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: Expr -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e

runTCFail' :: Expr -> IO ()
runTCFail' e = runTCFail e >> pure ()

-- Define your test cases like the following
testApplicationPlus :: IO ()
testApplicationPlus = do
  t <- runTCTest $ App (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (Num 21)
  assertEqual "Incorrect type" NumT $ fst t

testIfValid :: IO ()
testIfValid = do
  t <- runTCTest $ Conditional Tru (Num 1) (Num 2)
  assertEqual "Incorrect type" NumT $ fst t

testIfCond :: IO ()
testIfCond = runTCFail' $ Conditional (Num 3) (Num 1) (Num 2)

testIfBranch :: IO ()
testIfBranch = runTCFail' $ Conditional Fls (Num 1) (Tru)

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "testApplicationPlus" ~: testApplicationPlus
    , "testIfValid" ~: testIfValid
    , "testIfCond" ~: testIfCond ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
