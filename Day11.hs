{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import Control.Parallel.Strategies
--     ( parListChunk, rdeepseq, parList, using )
import qualified Data.Map.Strict as M
import Debug.Trace (trace)
import Data.List (intersperse, intercalate, sortOn, groupBy)
import Data.Maybe (fromMaybe)

main = do
    let input = parse "70949 6183 4 3825336 613971 0 15 182"
    print (problem' 25 input)
    print (problem' 75 input)

parse :: String -> [Int]
parse = map read . words

rules :: Int -> [Int]
rules 0 = [1]
rules !n =
    if even d
        then (let (!a,!b) = n `divMod` (10 ^(d `div` 2)) in [a,b])
        else let !x = n * 2024 in [x]
    where
        d = countDigits n

-- hopefully this go faster than log10 or show
countDigits :: Int -> Int
countDigits 0 = 1
countDigits !n = go n 0
  where
    go 0 !acc = acc
    go !x !acc = go (x `div` 10) (acc + 1)

-- Original algorithm. Pretty straitforward :
-- Apply the rules step by step and let it build
-- a huge list.
-- Orfortunately that doesn't scale. 

step :: [Int] -> [Int]
step = concatMap rules

problem :: Int -> [Int] -> Int
problem i l = length (go i l)
    where
        go 0 l = l
        go !n l = go (n-1) (step l)

-- ---------------------------------------------------------------------------
-- The number of stones in the list has nothing to do with their position
-- So keeping the list of stones in order makes no sense. 
-- And if we don't keep them in order, why not merge all similar stones
-- and keep track of their count ?

-- Merge similar stones, taking care of their count
compress :: [(Int,Int)] -> [(Int,Int)]
compress = map compact . groupBy cmpFst . sortOn fst
    where 
        cmpFst a b = fst a == fst b
        compact l = (fst $ head l,sum . map snd $ l) 

-- pretty much the same stepping function, but with compressed stone lists
step2 :: [(Int,Int)] -> [(Int,Int)]
step2 = concatMap crules . compress
    where 
        crules (n,m) = map (,m) (rules n)

-- loop through the steps returning the sum of all stone counts
problem' :: Int -> [Int] -> Int
problem' i l = sum (map snd (go i (map (,1) l)))
    where
        go 0 l = l
        go !n l = go (n-1) (step2 l)
