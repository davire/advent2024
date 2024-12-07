
module Main where
import System.Environment (getArgs)
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)

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

type Equation = (Int, [Int])

problems :: FilePath -> IO ()
problems fn = do
    equations <- fmap parseText (readFile fn)
    print (problem1 equations)
    print (problem2 equations)

-- getting the numbers in reverse to check from the end
parseText :: String -> [Equation]
parseText txt = [(s,n) | [s',n'] <- map (splitOn ": ") (lines txt)
                         , let s = read s'
                               n = reverse $ map read (words n')
                         ]

problem1 :: [Equation] -> Int
problem1 = sum . map check

check :: Equation -> Int
check (s, l) = if eval s l then s else 0

-- now that I've done part two, these 2 functions seem ugly
eval :: Int -> [Int] -> Bool
eval s [] | s == 0 = True
eval s []          = False
eval s (x:xs) = eval' s x xs

eval' :: Int -> Int -> [Int] -> Bool
eval' s x xs = case s `divMod` x of
    (d, m) | m == 0 -> eval d xs || s - x >= 0 && eval (s-x) xs
    _               -> s - x >= 0 && eval (s-x) xs

-- revisited the algo for the 2nd part
problem2 :: [Equation] -> Int
problem2 = sum . map check2

check2 :: Equation -> Int
check2 (s, l) = if eval2 [(s,l)] then s else 0

-- step through the equations until one of them is a solution
-- or we're out of possibilities
eval2 :: [Equation] -> Bool
eval2 [] = False
eval2 xs = isSuccess || eval2 results
    where
        results = concatMap step xs
        isSuccess = (0, []) `elem` results

-- generate all "equations" that I can come from
-- then filter those that can't possibly lead to a solution
step :: Equation -> [Equation]
step eq = filter bad $ catMaybes [stepSub eq, stepDiv eq, stepSplit eq]
    where
        bad (s, xs) = (s==0) == null xs

stepSub :: Equation -> Maybe Equation
stepSub (s, x:xs)
    | s - x >= 0 = Just (s - x, xs)
    | otherwise = Nothing

stepDiv :: Equation -> Maybe Equation
stepDiv (s, x:xs)
    | b == 0  = Just (a, xs)
    | otherwise = Nothing
    where
        (a, b) = s `divMod` x

stepSplit :: Equation -> Maybe Equation
stepSplit (s, x:xs)
    | take lrx rs == rx = Just (s', xs)
    | otherwise         = Nothing
    where
        rs  = reverse (show s)
        rx  = reverse (show x)
        lrx = length rx
        drs = drop lrx rs
        s' = if null drs then 0 else read (reverse drs)

