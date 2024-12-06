{-# LANGUAGE BangPatterns #-}
module Main where
import System.Environment (getArgs)
import System.Exit (exitSuccess,exitFailure)
import Data.List (partition, sort, groupBy)
import qualified Data.Map as M
import qualified Data.Set as S
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
    let (pos, dir, grid) = parseText txt
    print (problem1 pos dir grid)
    print (problem2 pos dir grid)
    return ()

type Coord = (Int,Int)
type Grid = M.Map Coord Char
data Direction = North | East | South | West deriving(Show,Enum,Eq)

parseText :: String -> (Coord, Direction, Grid)
parseText txt = (p, North, m)
    where
        l = [ ((x, y), c) | (y, r) <- zip [1..] (lines txt), (x, c) <- zip [1..] r ]
        m = M.fromList l
        -- I'm assuming there is one starting position
        p = fst $ head $ filter (\(_, c) -> c == '^') l

move :: Coord -> Direction -> Coord
move (!x, !y) North = (x, y-1)
move (!x, !y) East  = (x+1, y)
move (!x, !y) South = (x, y+1)
move (!x, !y) West  = (x-1, y)

turn :: Direction -> Direction
turn North = East
turn East  = South
turn South = West
turn West  = North

problem1 :: Coord -> Direction -> Grid -> Int
problem1 !c !d g = go c d []
    where
        -- max bounds
        (!mx, !my) = fst $ M.findMax g
        -- probably faster than looking up the key ?
        inBounds (!x,!y) = x>=1 && x <= mx && y >= 1 && y <= my
        peek !c !d = fromMaybe ' ' (M.lookup (move c d) g)

        go :: Coord -> Direction -> [Coord] -> Int
        go !c !d l | not $ inBounds c = S.size $ S.fromList l   -- this is the exit case
        go !c !d l = case peek c d of
            '#' -> go c (turn d) l        -- wall -> turn and continue
            _   -> go (move c d) d (c:l)  -- not wall -> progress and keep path


problem2 :: Coord -> Direction -> Grid -> Int
problem2 !c !d g = sum $ map fromEnum loopingObstacles
    where
        -- max bounds
        (!mx, !my) = fst $ M.findMax g
        -- probably faster than looking up the key ?
        inBounds (!x,!y) = x>=1 && x <= mx && y >= 1 && y <= my
        -- peek in the direction taking into account an obstacle
        peek !c !d !o | move c d == o = '#'
        peek !c !d _                 = fromMaybe ' ' (M.lookup (move c d) g)

        -- get initial guard path. It only makes sense to put obstables
        -- in that path
        go1 :: Coord -> Direction -> [(Coord, Direction)] -> [(Coord, Direction)]
        go1 !c !d l | not $ inBounds c = l -- there's the path without obstables
        go1 !c !d l = case peek c d (0,0) of
            '#' -> go1 c (turn d) l             -- wall -> turn and continue
            _   -> go1 (move c d) d ((c, d):l)  -- not wall -> progress and keep path

        possibleObstacles :: [Coord]
        possibleObstacles = S.toList $ S.fromList (map (uncurry move) $ go1 c d [])
        loopingObstacles :: [Bool]
        loopingObstacles = map (go c d []) possibleObstacles


        go :: Coord -> Direction -> [(Coord, Direction)] -> Coord -> Bool
        go !c !d l !o | not $ inBounds c = False   -- no loop here
        go !c !d l !o | (c, d) `elem` l  = True    -- we looped
        go !c !d l !o = case peek c d o of
            '#' -> go c (turn d) l o             -- wall -> turn and continue
            _   -> go (move c d) d ((c, d):l) o  -- not wall -> progress and keep path
