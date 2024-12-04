module Main where
import Text.Regex.Posix (getAllTextMatches, (=~))
import Data.List (foldl')
import qualified Data.Array as A

main:: IO ()
main = do
    txt <- readFile "day_4_input.txt"

    print $ problem1 txt
    print $ problem2 txt

problem1 :: String -> Int
problem1  = countXmas . makeArray

type Point = (Int,Int)
type Ray = [Point]
type Bounds = (Point,Point)
type Matrix = A.Array Point Char 

makeArray :: String -> Matrix
makeArray txt = A.array ((1,1),(bx,by)) arrayData
    where 
        l = lines txt
        bx = length $ head l 
        by = length l
        arrayData = [((x,y), c) | (y,r) <- zip [1..] l, (x,c) <- zip [1..] r ] 

countXmas :: Matrix -> Int
countXmas a = length $ filter (== "XMAS") $ map (map (a A.!)) positions 
    where
        positions = concatMap (rays $ A.bounds a) (A.indices a)


rays :: Bounds -> Point -> [Ray]
rays b pt = filter (all $ inBounds b) $ streak pt 
    where
        streak pt = [take 4 $ iterate (add d) pt | d <- [(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]]
        add (dx,dy) (x,y) = (x+dx,y+dy)
 
inBounds ((sx,sy),(mx,my)) (x,y) = x >= sx && y >= sy && x <= mx && y <= my

problem2 :: String -> Int
problem2  = countCrosses . makeArray

crosses :: Point -> [Ray]
crosses (x,y) = [[(x-1,y-1),(x,y),(x+1,y+1)],[(x+1,y-1),(x,y),(x-1,y+1)]]      

countCrosses :: Matrix -> Int 
countCrosses a = length $ filter validCross inBoundsCrosses 
    where
        allCrosses = map crosses $ A.indices a
        inBoundsRay = all (inBounds $ A.bounds a)
        inBoundsCrosses = filter (all inBoundsRay) allCrosses
        toTxt = map (a A.!) 
        validCross [r1,r2] = let (t1,t2) = (toTxt r1,toTxt r2) in (t1 == "MAS" || t1 == "SAM") && (t2 == "MAS" || t2 == "SAM") 
