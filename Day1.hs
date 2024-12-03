module Main where

import qualified Data.Map as M
import Data.List (zipWith, sort, group)
import Data.Maybe (fromMaybe)

main:: IO ()
main = do
    txt <- readFile "day_1_input.txt"
    let (l1, l2) = unzip $ map (toTuple . map read . words) $ lines txt
    print (sum $ zipWith (\a b -> abs (a-b)) (sort l1) (sort l2))
    print (similarity l1 l2)

toTuple :: [Int] -> (Int, Int)
toTuple [a,b] = (a,b)
toTuple _ = error "Not a tuple"


similarity:: [Int] -> [Int] -> Int
similarity l1 l2 = sum $ map fn l1
    where
        m = getMap l2
        fn v = fromMaybe 0 (M.lookup v m)

getMap :: [Int] -> M.Map Int Int
getMap l = M.fromList $ map (\sl -> (head sl, head sl * length sl)) $ group $ sort l