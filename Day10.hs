-- {-# LANGUAGE BangPatterns #-}
module Main where
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import qualified Data.Vector.Unboxed as U
import Data.Char (digitToInt, chr)
import Data.List.Split (chunksOf)
import qualified Data.Set as S
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (guard)

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


-- problems :: FilePath -> IO ()
problems fn = do
    m <- makeArray `fmap` readFile fn
    print $ problem1 m
    print $ problem2 m


problem1 :: Matrix -> Int
problem1 m = sum $ map (score1 m) (getTrailheads m)

type Point = (Int,Int)
type Matrix = (U.Vector Int, Point)

-- we use an pair of unboxed vector and grid size as data structure
makeArray :: String -> Matrix
makeArray txt = (v, s)
    where
        l = lines txt
        s = (length (head l), length l)
        v = U.fromList $ map digitToInt $ concat l

-- get the elevation from coordinates, if it's in bounds
getXY :: Matrix -> Point -> Maybe Int
getXY (v,s@(mx,my)) p@(x,y)
    | inBounds s p = Just (v U.! (y*mx+x))
    | otherwise    = Nothing

-- get cardinal neighbors
north :: Point -> Point
north (x,y) = (x,y-1)
south :: Point -> Point
south (x,y) = (x,y+1)
west :: Point -> Point
west (x,y)  = (x-1,y)
east :: Point -> Point
east (x,y)  = (x+1,y)

-- for debug purposes, converts the matrix into a character grid
showM :: Matrix -> String
showM (v,b) = unlines $ chunksOf (fst b) (U.toList $ U.map (chr . (+48)) v)

-- checks if a point is inside the matrix
inBounds :: Point -> Point -> Bool
inBounds (sx,sy) (x,y) = x >= 0 && y >= 0 && x < sx && y < sy

-- given a list of points, gets all the points for the next elevation level
step1 :: Matrix -> Int -> [Point] -> [Point]
step1 m v = concatMap check'
    where
        -- get a list of checking functions for all neighbor
        -- apply them to the a point, keep only the suitable neighbors 
        check' p = mapMaybe (($ p) . check m next)  [north, south, west, east]
        next = v + 1

-- checks if a point neighbor is suitable for the path
check :: Matrix -> Int -> (Point -> Point) -> Point -> Maybe Point
check m next f p = do
    let p' = f p        -- get the neighbor
    v' <- getXY m p'    -- stop here if out of bounds
    guard (v' == next)  -- stop here if not the expected elevation
    pure p'

-- computes the list of ending points from a trail head, 
-- the score is the number of points.
score1 :: Matrix -> Point -> Int
score1 m p = length (go [p] 0)
    where
        -- gets all ending positions for a starting point
        -- removes duplicates at each step
        go :: [Point] -> Int -> [Point]
        go ps 9 = ps
        go ps v = go (S.toList . S.fromList $ step1 m v ps) (v+1)

-- scans the grid for places with elevation 0
getTrailheads :: Matrix -> [Point]
getTrailheads m = [(x,y) | x <- [0..sx-1], y <- [0..sy-1], let v = fromJust (getXY m (x,y)), v == 0]
    where
        (sx, sy) = snd m

-- for each trail heads, compute how many trails are possible. take the sum of that
problem2 :: Matrix -> Int
problem2 m = sum $ map (length . getAllTrails m) (getTrailheads m)

type Trail = [Point]

-- make a first incomplete trail from a point, and expand it step by step
-- until we have all complete trails that start from the initial point
getAllTrails :: Matrix -> Point -> [Trail]
getAllTrails m p = go 0 [[p]]
    where
        go :: Int -> [Trail] -> [Trail]
        go 9 trails = trails
        go v trails = go (v+1) $ concatMap (advanceTrail m v) trails

-- given an incomplete trail, return all the ways we can expand it by 1 elevation step
advanceTrail :: Matrix -> Int -> Trail -> [Trail]
advanceTrail m v t = map (: t) (step2 m v (head t))

-- given a point, return all points in cardinal directions that are 1 step uphill
step2 :: Matrix -> Int -> Point -> [Point]
step2 m v p = mapMaybe check' [north, south, west, east]
    where
        check' = flip (check m next) p
        next = v + 1
