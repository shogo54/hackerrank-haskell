{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO

-- Complete the birthdayCakeCandles function below.
birthdayCakeCandles ar = do
    length (Prelude.filter (== maximum ar) ar)
    -- or 
    --length [x | x <- ar, x == maximum ar]

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arCount <- readLn :: IO Int

    arTemp <- getLine

    let ar = Data.List.map (read :: String -> Int) . words $ arTemp

    let result = birthdayCakeCandles ar

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
