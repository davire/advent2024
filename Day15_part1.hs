-- {-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where
-- imports for main
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
-- imports for parsing
import Text.Megaparsec ( (<?>) )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Either (fromRight)
-- other imports
import Data.List (sort, group, groupBy, sortOn, foldl')
import Data.List.Split (chunksOf)
import Data.Vector.Unboxed ((//))
import qualified Data.Vector.Unboxed as U
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Debug.Trace (trace)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            problem1 fn
            exitSuccess
        _    -> do
            putStrLn "Missing filename !"
            exitFailure
    return ()

problem1 :: FilePath -> IO ()
problem1 fn = do
    (g,m) <- readFile fn >>= parseInput
    let r = foldl' step g m
        s = gpsSum r
    putStrLn $ "Problem 1 : " ++ show s


type Parser = P.Parsec Void String

-- A parser for an individual coordinate (e.g., "X+94, Y+34")
parseGrid :: Parser (Matrix, [Mvt])
parseGrid = do
    let gridChar = P.oneOf "#.O@" <?> "grid char (#,.,O,@)"
        mvtChar = fromChar <$> ( P.oneOf "<>v^" <?> "movement (^,>,v,<)" )
        gridNewline = PC.newline <?> "grid newline"
        mvtNewline = PC.newline <?> "movement newline"
    lines <- P.some (P.some gridChar <* gridNewline)
    let
        bounds@(bx,by) = (length (head lines), length lines)
        vec    = U.fromList $ concat lines
        spos   = fromMaybe (-1) ('@' `U.elemIndex` vec)
        mat = Matrix vec bounds (spos `divMod` bx)

    PC.newline <?> "newLine between grid and movements"
    mov <- concat <$> P.some (P.some mvtChar <* mvtNewline)
    if spos <0
        then fail "Robot position not found"
        else return (mat, mov)

parseInput :: String -> IO (Matrix, [Mvt])
parseInput txt = do
    case P.parse parseGrid "" txt of
        Left err -> do
            putStrLn (P.errorBundlePretty err)
            exitFailure
        Right (s,t) -> do
            return (s,t)

type Point = (Int,Int)
type Size = (Int,Int)
data Matrix = Matrix (U.Vector Char) Size Point
instance Show Matrix
    where
        show (Matrix v b p) = unlines (
                ("size : " ++ show b ++ "  robot : " ++ show p)
                : chunksOf (fst b) (U.toList v)
            )
getPos :: Matrix -> Point
getPos (Matrix _ _ p) = p
getSize :: Matrix -> Size
getSize (Matrix _ s _) = s
getGrid :: Matrix -> U.Vector Char
getGrid (Matrix g _ _) = g

data Mvt = North | East |South | West
instance Show Mvt
    where
        show North = "^"
        show East  = ">"
        show South = "v"
        show West  = "<"

fromChar :: Char -> Mvt
fromChar '^' = North
fromChar '>' = East
fromChar 'v' = South
fromChar '<' = West

mvt :: Mvt -> Point -> Point
mvt North (x,y) = (x,y-1)
mvt South (x,y) = (x,y+1)
mvt West (x,y)  = (x-1,y)
mvt East (x,y)  = (x+1,y)


-- get the grid element at the given coordinates
getXY :: Matrix -> Point -> Maybe Char
getXY (Matrix v s@(mx,my) _) p@(x,y)
    | inBounds s p = Just (v U.! (y*mx+x))
    | otherwise    = Nothing

-- checks if a point is inside the matrix
inBounds :: Size -> Point -> Bool
inBounds (sx,sy) (x,y) = x >= 0 && y >= 0 && x < sx && y < sy

step :: Matrix -> Mvt -> Matrix
step m@(Matrix _ _ p) d = fromMaybe m (move m d p)

move :: Matrix -> Mvt -> Point -> Maybe Matrix
move m@(Matrix v b r) d p = do
    let np = mvt d p
        bx = fst b
        i (x,y) = y * bx + x
    c  <- getXY m np
    guard (c /= '#')
    if c == '.'
        then do
            oc <- getXY m p
            let v' = -- trace ("move c="++show c++" d="++ show d++" p="++show p++" np="++show np) $ 
                     v // [(i p, '.'),(i np, oc)]
            return (Matrix v' b np)
        else do
            m' <- move m d np
            oc <- getXY m p
            let v' = getGrid m' // [(i p, '.'),(i np, oc)]
            return (Matrix v' b np)

gpsSum :: Matrix -> Int 
gpsSum m = 
    sum 
    $ map (gps . coords . fst) 
    $ filter lantern 
    $ zip [0..] (U.toList (getGrid m))
    where 
        lantern e = snd e == 'O'
        (bx,_) = getSize m
        coords i = let (y,x) = i `divMod` bx in (x,y)
        gps (x,y) = y * 100 + x