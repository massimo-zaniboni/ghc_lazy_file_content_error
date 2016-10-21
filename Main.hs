{-# LANGUAGE OverloadedStrings, BangPatterns #-}

{-
ISC License

Copyright (c) 2016, Massimo Zaniboni <massimo.zaniboni@docmelody.com>

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}

module Main (
    main
) where

import Data.List as List
import Control.Monad
import qualified Control.Exception as Exception
import qualified Data.Text as StrictText
import qualified Data.Text.IO as StrictText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Attoparsec.Text.Lazy as LazyParser
import qualified Data.Attoparsec.Combinator as Parser
import System.IO as IO
import System.Environment

type TestName = String

type Test = IO String

fileName = "bracket_test.txt"

strToCheck = "Hello world!"

main = do
  args <- getArgs
  r <- case args of
    [] -> testAll
    ["--stress-files"] -> testMaxOpenFiles
    _ -> do
      putStrLn $ "usage: bracket [--stress-files]"
      return False

  case r of
    True -> do
      putStrLn "Tests are ok"
      return 0

    False -> do
      putStrLn "Tests failed!"
      return 1

-- -------------------------------------------
-- Test hGetContents and hClose

testAll :: IO Bool
testAll = do
  writeFile fileName strToCheck
  checkAll [("bracket, lazy text, lazy eval", test_bracket_lazy_lazy)
           ,("bracket, lazy text, strict eval", test_bracket_lazy_strict)
           ,("bracket, no lazy text, lazy eval", test_bracket_lazy_lazy)
           ,("no bracket, lazy text, lazy eval, no hClose", test_nobracket_lazy_lazy_noclose)
           ,("no bracket, lazy text, lazy eval, hClose", test_nobracket_lazy_lazy_close)
           ,("no bracket, lazy text, strict eval, hClose", test_nobracket_lazy_strict_close)
           ,("withFile, lazy text, lazy eval", test_withFile_lazy_lazy)
           ,("withFile, lazy string, lazy eval", test_withFile_lazyString_lazy)
           ]

checkAll :: [(TestName, Test)] -> IO Bool
checkAll tests = do
  foldM (\r1 (testName, testFun)
         -> do r2 <- check testName testFun
               return (r1  && r2)
        ) True tests

check :: TestName -> Test -> IO Bool
check testName testFun = do
  r <- testFun
  let testPassed = r == strToCheck
  putStrLn $ "Test " ++ testName ++ ": " ++ (if testPassed then " passed" else ": very bad behaviour because an empty file was assumed!")
  return testPassed

test_bracket_lazy_lazy = do
    Exception.bracket
      (do handle <- IO.openFile fileName ReadMode
          return handle
      )

      (hClose)

      (\handle
         -> do fileContent <- LazyText.hGetContents handle
               return $ LazyText.unpack fileContent
      )

test_bracket_lazy_strict = do
    Exception.bracket
      (do handle <- IO.openFile fileName ReadMode
          return handle
      )

      (hClose)

      (\handle
         -> do fileContent <- LazyText.hGetContents handle
               return $! LazyText.unpack fileContent
      )

test_nobracket_lazy_lazy_noclose = do
  handle <- IO.openFile fileName ReadMode
  fileContent <- LazyText.hGetContents handle
  return $ LazyText.unpack fileContent

test_nobracket_lazy_lazy_close = do
  handle <- IO.openFile fileName ReadMode
  fileContent <- LazyText.hGetContents handle
  let result = LazyText.unpack fileContent
  hClose handle
  return result

test_nobracket_lazy_strict_close = do
  handle <- IO.openFile fileName ReadMode
  fileContent <- LazyText.hGetContents handle
  let !result = LazyText.unpack fileContent
  hClose handle
  return result

test_bracket_strict_lazy =
    Exception.bracket
      (do handle <- IO.openFile fileName ReadMode
          return handle
      )

      (hClose)

      (\handle
         -> do fileContent <- StrictText.hGetContents handle
               return $ StrictText.unpack fileContent
      )

test_withFile_lazy_lazy = do
    IO.withFile fileName ReadMode $ \handle -> do
      fileContent <- LazyText.hGetContents handle
      return $ LazyText.unpack fileContent

test_withFile_lazyString_lazy = do
    IO.withFile fileName ReadMode $ \handle -> do
      IO.hGetContents handle

-- -------------------------------------------
-- Stress Files

-- | A big number of files to open,
--   that should exceed the operating system resources.
maxFilesToOpen = 50000

-- | The dimensions of the opened file,
--   that should exceed the Haskell file buffering dimensions.
parsingFileKB = 15000


-- | Test if parsers with errors can leak resources.
--   Open a file in lazy mode, parse only a part of it,
--   introducing some parsing error.
testMaxOpenFiles :: IO Bool
testMaxOpenFiles = do
   IO.writeFile fileName (List.take (parsingFileKB * 1024) $ List.repeat 'X')
   parseManyTimes 0

  where

    parseManyTimes :: Int -> IO Bool
    parseManyTimes c =
      case c > maxFilesToOpen of
        True -> return True
        False -> do
          when (c `mod` 10 == 0) (putStrLn $ "parsed " ++ show c ++ " files of " ++ show maxFilesToOpen)
          f <- parseFile
          case f of
            True -> do
              putStrLn "Error during parsing of file."
              return False
            False -> parseManyTimes (c + 1)

    parseFile :: IO Bool
    parseFile = do
      t <- LazyText.readFile fileName
      case LazyParser.eitherResult $ LazyParser.parse (LazyParser.string "abc") t of
        Left _ -> return False
        Right _ -> return True
