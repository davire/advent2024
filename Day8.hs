
module Main where
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.List (groupBy, sort, tails, foldl', nub)
-- import Data.List (unfoldr)
-- import Data.List.Split (splitOn)
-- import Control.Monad (forM_)
-- import Data.Maybe (catMaybes)

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
    d <- fmap parseText (readFile fn)
    print (problem1 d)
    print (problem2 d)


type Frequency = Char
type Position = (Int, Int)
type Bounds = (Int, Int)
type Network = (Frequency, [Position])

-- getting the numbers in reverse to check from the end
parseText :: String -> ([Network], Bounds)
parseText txt = (filter (\(a,b) -> a /= '.' && a/= '#') . groupKey $ grid, bounds)
    where
        (grid, bounds) = parseGrid txt

groupKey :: (Ord a, Eq a, Ord b, Eq b) => [(a, b)] -> [(a, [b])]
groupKey =  map (\a -> (fst (head a), map snd a) ) . groupBy (\a b -> fst a == fst b) . sort

parseGrid :: String -> ([(Char, Position)], Bounds)
parseGrid txt = (grid, bounds)
    where
        grid = [ (c, (x, y)) | (y, r) <- zip [0..] (lines txt), (x, c) <- zip [0..] r ]
        bounds = foldl' max (0,0) (map snd grid)

problem1 :: ([Network], Bounds) -> Int
problem1 (n, (mx,my)) = length . nub . sort . concatMap (filter inBounds) . allAntinodes $ n
    where
        inBounds (x,y) = x>= 0 && y>= 0 && x<= mx && y <= my

allAntinodes :: [Network] -> [[Position]]
allAntinodes = map antinodesNetwork

antinodesNetwork :: Network -> [Position]
antinodesNetwork (f,p) = concatMap antinodesPair (allPairs p)

antinodesPair :: (Position, Position) -> [Position]
antinodesPair ((x,y), (x',y')) = [ (x - (x' - x), y - (y' - y)), (x' + (x' - x), y' + (y' - y))]

allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

problem2 :: ([Network], Bounds) -> Int
problem2 (n, b) = length . nub . sort . concat . allAntinodesResonant b $ n

antinodesPairResonant :: Bounds -> (Position, Position) -> [Position]
antinodesPairResonant (mx,my) p@((x,y), (x',y')) = filter inBounds . take (2* max mx my) . merge . tail . iterate f $ p
    where
        (dx, dy) = (x' - x, y' - y )
        f ((ax,ay),(bx,by)) = ( (ax+dx, ay+dy), (bx-dx, by-dy))
        inBounds (x,y) = x>= 0 && y>= 0 && x<= mx && y <= my

merge :: [(a, a)] -> [a]
merge [] = []
merge ((a,b):xs) = a:b:merge xs

antinodesNetworkResonant :: Bounds -> Network -> [Position]
antinodesNetworkResonant b (f,p) = concatMap (antinodesPairResonant b) (allPairs p)

allAntinodesResonant :: Bounds -> [Network] -> [[Position]]
allAntinodesResonant b = map (antinodesNetworkResonant b)