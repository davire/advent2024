-- {-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where
-- imports for main
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
-- imports for parsing
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Either (fromRight)
-- other imports
import Data.List (sort, group, groupBy, sortOn, foldl')
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            problem1 fn
            problem2 fn
            exitSuccess
        _    -> do
            putStrLn "Missing filename !"
            exitFailure
    return ()

problem1 :: FilePath -> IO ()
problem1 fn = do
    robots <- parseInput `fmap` readFile fn
    let sz = (101,103)
        sec = 100
        points = map (getPos sz sec) robots
        score  = safety sz points
    print score


problem2 :: FilePath -> IO ()
problem2 fn = do
    robots <- parseInput `fmap` readFile fn
    let sz = (101,103)
        candidates = findTree robots sz
    print (fst $ head candidates)

type Parser = P.Parsec Void String

-- A parser for an individual coordinate (e.g., "X+94, Y+34")
pair :: Parser (Int,Int)
pair = do
    x <- L.signed (return ()) L.decimal
    PC.string ","
    y <- L.signed (return ()) L.decimal
    return (x, y)

-- A parser for an entire block
block :: Parser Robot
block = do
    pos <- PC.string "p=" *> pair
    vel <- PC.string " v=" *> pair <* PC.newline
    return (pos, vel)

-- -- A parser for the entire input
inputParser :: Parser [Robot]
inputParser = P.many block

-- -- Parsing function
tryParse :: String -> Either (P.ParseErrorBundle String Void) [Robot]
tryParse = P.parse inputParser ""

parseInput :: String -> [Robot]
parseInput = fromRight [] . tryParse


type Point = (Int, Int)
type Velocity = (Int, Int)
type Size = (Int, Int)
type Seconds = Int
type Quadrant = Int
type Robot = (Point, Velocity)

getPos :: Size -> Seconds -> Robot -> Point
getPos (sx, sy) s ((px, py), (vx, vy)) = ((px + s * vx) `mod` sx, (py + s * vy) `mod` sy)

quadrant :: Size -> Point -> Quadrant
quadrant (sx, sy) (px,py) = case (px < mx,px > mx,py < my,py > my) of
    (True, False, True, False) -> 1
    (False, True, True, False) -> 2
    (True, False, False, True) -> 3
    (False, True, False, True) -> 4
    _                          -> 0
    where
        mx = sx `div` 2
        my = sy `div` 2

safety :: Size -> [Point] -> Int
safety s = product . map length . group . sort. filter (>0) . map (quadrant s)

-- problem 2
-- let's try to find frames where the points are clustered

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Check if two points are neighbors based on a threshold distance
isNeighbor :: Int -> Point -> Point -> Bool
isNeighbor threshold p1 p2 = dist p1 p2 <= threshold

-- Score a single point based on neighbors
scorePoint :: Int -> [Point] -> Point -> Int
scorePoint threshold points point = length (filter (isNeighbor threshold point) points) - 1

-- Score all points
scorePoints :: Int -> [Point] -> [(Point, Int)]
scorePoints threshold points = [(p, scorePoint threshold points p) | p <- points]

showGrid :: Size -> [Point] -> String
showGrid (sx, sy) p = unlines $ chunksOf sx chars
    where
        chars = [c | y <- [0.. sy - 1], x <- [0 .. sx - 1], let c = if (x,y) `elem` p then '#' else ' ']

-- finds all candidate trees
findTree :: [Robot] -> (Int, Int) -> [(Int, [Point])]
findTree robots sz = candidates
    where
        -- a candidate is a list of points where there is at least 4 lines of 20 consecutive points
        candidates = filter (\(sec,points) -> hasHVLine 20 points >= 4) frames
        frames = map getFrame [1..]
        getFrame sec = (sec, map (getPos sz sec) robots)

-- Group points by rows or columns
groupByCoords :: (Point -> Int) -> [Point] -> [[Point]]
groupByCoords fn = groupBy (\p1 p2 -> fn p1 == fn p2) . sortOn fn

-- Check if there's a "line" of at least n "pixels" 
hasLine :: Int -> [Int] -> Bool
hasLine n =  any ((>n) . length) . spans . sort

spans :: [Int] -> [[Int]]
spans = foldl' fn []

fn :: [[Int]] -> Int -> [[Int]]
fn [] i = [[i]]
fn (h@(x:xs):ys) i
    | i == x + 1 = (i:h):ys
    | otherwise  = [i]:(h:ys)

-- Detect horizontal or vertical lines of length at least n
hasHVLine :: Int -> [Point] -> Int
hasHVLine n points = rowCount + colCount
    where
        cols = groupByCoords fst points
        rows = groupByCoords snd points
        check fn = fromEnum . hasLine n . map fn
        colCount = sum $ map (check snd) cols
        rowCount = sum $ map (check fst) rows
