module Main where
import System.Environment (getArgs)
import System.Exit (exitSuccess,exitFailure)
import Data.List (partition, sort, groupBy)
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Maybe ( fromMaybe )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            problems fn
            exitSuccess
        _    -> do
            putStrLn "Missing filename !"
            exitFailure
    return ()

problems :: FilePath -> IO ()
problems fn = do
    txt <- readFile fn
    let (rules, updates) = parseText txt
    print (problem1 rules updates)
    print(problem2 rules updates)
    return ()

parseText txt = (r, u)
    where
        l = lines txt
        (lr, lu') = partition (hasChar '|') l
        lu = tail lu'
        -- for each page, map the list of pages I don't want to see after this page 
        r1 = sort $ map ((\[a,b] -> [b,a]) . splitOn "|") lr
        r2 = groupBy (\[a,_ ] [b,_] -> a == b) r1
        r3 = map (\g -> (head (head g),concatMap tail g ) ) r2
        r = M.fromList r3
        u = map (splitOn ",") lu

hasChar :: Char -> String -> Bool
hasChar c s = c `elem` s

problem1 :: M.Map String [String] -> [[String]] -> Int
problem1 r u = sum $ map middlePage $ filter (correct r) u

correct :: M.Map String [String] -> [String] -> Bool
correct _ [] = True
correct r (x:xs) = not (badOrder r x xs) && correct r xs

badOrder :: M.Map String [String] -> String -> [String] -> Bool
badOrder r x xs = let np = fromMaybe [] (M.lookup x r) in any (`elem` np) xs

-- keep in mind, indices start at 0
middlePage :: [String] -> Int
middlePage u = read $ u !! (length u `div` 2)


problem2 :: M.Map String [String] -> [[String]] -> Int
problem2 r u= sum $ map (middlePage . fixOrder r) (filter (not . correct r) u)

-- fix order step by step
-- at each step, either the page is correct and we go to the next
-- or it's not and we switch position with all the pages that should come before
fixOrder :: M.Map String [String] -> [String] -> [String]
fixOrder r [] = []
fixOrder r (x:xs)
    | badOrder r x xs = fixOrder r (switch r x xs)
fixOrder r (x:xs)     = x : fixOrder r xs

-- puts pages that should be printed first before page x
switch :: M.Map String [String] -> String -> [String] -> [String]
switch r x xs = ok ++ (x:nok)
    where 
        (ok,nok) = partition isBad xs
        mustBeBefore = fromMaybe [] (M.lookup x r)
        isBad c = c `elem` mustBeBefore
