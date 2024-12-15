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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            problem2 fn
            exitSuccess
        _    -> do
            putStrLn "Missing filename !"
            exitFailure
    return ()

problem2 :: FilePath -> IO ()
problem2 fn = do
    (g,m) <- readFile fn >>= parseInput
    let r = foldl' step g m
        s = gpsSum r
    putStrLn $ "Problem 2 : " ++ show s


type Parser = P.Parsec Void String

-- A parser for an individual coordinate (e.g., "X+94, Y+34")
parseGrid :: Parser (Matrix, [Mvt])
parseGrid = do
    let gridChar = widen <$> P.oneOf "#.O@" <?> "grid char (#,.,O,@)"
        mvtChar = fromChar <$> ( P.oneOf "<>v^" <?> "movement (^,>,v,<)" )
        gridNewline = PC.newline <?> "grid newline"
        mvtNewline = PC.newline <?> "movement newline"
    lines <- P.some (concat <$> P.some gridChar <* gridNewline)
    let
        bounds@(bx,by) = (length (head lines), length lines)
        vec    = U.fromList $ concat lines
        spos   = fromMaybe (-1) ('@' `U.elemIndex` vec)
        coords p = let (y,x) = p `divMod` bx in (x,y)
        mat = Matrix vec bounds (coords spos)

    PC.newline <?> "newLine between grid and movements"
    mov <- concat <$> P.some (P.some mvtChar <* mvtNewline)
    if spos <0
        then fail "Robot position not found"
        else return (mat, mov)

widen :: Char -> String
widen '.' = ".."
widen '#' = "##"
widen 'O' = "[]"
widen '@' = "@."

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
data Matrix = Matrix !(U.Vector Char) !Size !Point
instance Show Matrix
    where
        show (Matrix v b p) = unlines (
                ("size : " ++ show b ++ "  robot : " ++ show p)
                : showV (fst b) v
            )
showV w v = chunksOf w (U.toList v)

getPos :: Matrix -> Point
getPos (Matrix _ _ p) = p
getSize :: Matrix -> Size
getSize (Matrix _ s _) = s
getGrid :: Matrix -> U.Vector Char
getGrid (Matrix g _ _) = g

data Mvt = North | East |South | West deriving (Eq)
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
move m@(Matrix v b r) !d !p =
    do
    let np = mvt d p -- the point where we want to move
        bx = fst b   -- the grid's with 
        i (x,y) = y * bx + x
        -- updates the grid so that a space if put at position p and 
        -- the given character at position np
        doMove v p c np =
            v // [(i p, '.'), (i np, c)]
            where
                v' = v // [(i p, '.'), (i np, c)]
                fn s t = s ++ "  -->  " ++ t
                showMove = unlines $ zipWith fn (showV bx v) (showV bx v')

    c  <- getXY m np
    guard (c /= '#')
    if c == '.'
        then do
            oc <- getXY m p
            let v' = doMove v p oc np
                m' = Matrix v' b np
            return m' 
        else
            if d == West || d == East
                then do
                    m' <- move m d np
                    oc <- getXY m p
                    let v' = doMove (getGrid m') p oc np
                        m'' = Matrix v' b np
                    return m''
                else do
                    let d' = if c == '[' then East else West -- direction to find the other part of the lantern
                        c' = if c == '[' then ']' else '['
                        -- we want to move
                        -- p in the d direction -> np
                        -- but there is a lantern in np
                        -- so we have to move the lantern in the d direction too
                        -- that means moving np in the d direction
                        -- and the other side of the lantern, in the d direction too
                        -- the other side of the lantern is the point adjacent to np in the d' direction
                        np' = mvt d' np
                    -- if we can move the point in the direction 
                    !m'  <- move m  d np
                    -- and the other part of the lantern
                    !m'' <- move m' d np'
                    !oc  <- getXY m p
                    let !v'  = doMove (getGrid m'')  p  oc  np
                        !m''' = Matrix v' b np
                    return m''' 

gpsSum :: Matrix -> Int
gpsSum m =
    sum
    $ map (gps . coords . fst)
    $ filter lantern
    $ zip [0..] (U.toList (getGrid m))
    where
        lantern e = snd e == '['
        (bx,_) = getSize m
        coords i = let (y,x) = i `divMod` bx in (x,y)
        gps (x,y) = y * 100 + x