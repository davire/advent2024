module Main where

import qualified Data.Map as M
import Data.List (zipWith, sort, group)
import Data.Maybe (fromMaybe)

main:: IO ()
main = do
    txt <- readFile "day_2_input.txt"
    let reports = map (map read . words) $ lines txt

    print $ problem1 reports
    print $ problem2 reports

problem1 :: [[Int]] -> Int
problem1 reports = length $ filter isSafe reports

isSafe :: [Int] -> Bool
isSafe report = all (\d -> d `elem` [1, 2, 3]) deltas || all (\d -> d `elem` [-1, -2, -3]) deltas
    where
        deltas = zipWith (-) report (tail report)

problem2 :: [[Int]] -> Int
problem2 = length . problem2f

-- Gets all safe or dampened reports
problem2f :: [[Int]] -> [[Int]]
problem2f = filter isOk
    where 
        isOk r = isSafe r || isDampened r

-- Checks if a report is dampened
isDampened :: [Int] -> Bool
isDampened report = any isSafe $ removed report

-- Gets all sublists with one value removed
removed :: [Int] -> [[Int]]
removed (x:xs) = xs : map (x:) (removed xs)
removed [] = []
