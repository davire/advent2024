-- {-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)


type Button = (Int, Int)
type ClawMachine = (Button, Button, (Int, Int))
type Parser = P.Parsec Void String

-- A parser for an individual coordinate (e.g., "X+94, Y+34")
pair :: Parser Button
pair = do
    x <- (PC.string "X+" <|> PC.string "X=") >> L.decimal
    PC.string ", "
    y <- (PC.string "Y+" <|> PC.string "Y=") >> L.decimal
    return (x, y)

-- A parser for an entire block
block :: Parser ClawMachine
block = do
    buttonA <- PC.string "Button A: " >> pair
    buttonB <- PC.newline >> PC.space >> PC.string "Button B: " >> pair
    prize <- PC.newline >> PC.space >> PC.string "Prize: " >> pair
    PC.newline
    return (buttonA, buttonB, prize)

-- A parser for the entire input
inputParser :: Parser [ClawMachine]
inputParser = P.many (block <* P.optional PC.newline)

-- Parsing function
parseInput :: String -> Either (P.ParseErrorBundle String Void) [ClawMachine]
parseInput = P.parse inputParser ""

parseMachines :: String -> [ClawMachine]
parseMachines = fromRight [] . parseInput

pushButton :: Button -> Int -> (Int, Int)
pushButton (bx,by) n = (n * bx, n * by)

tokens :: (Int,Int) -> Int
tokens (a,b) = 3 * a + b

solutions :: ClawMachine -> [(Int, Int)]
solutions (a, b, (tx, ty)) =
    [(n, n') |
        n <- [0 .. 100],
        let (ax, ay) = pushButton a n,
        n' <- [0 .. 100],
        let (bx, by) = pushButton b n',
        (ax + bx, ay + by) == (tx, ty)]


-- Part 2 : no way the brute force works
-- we have to find a solution to the equation n⋅a + m⋅b = t 
-- using https://en.wikipedia.org/wiki/Cramer%27s_rule
-- i.e.
--   n * ax + m * bx == tx
--   n * by + m * by == ty

solveLinear :: ClawMachine -> (Int, Int)
solveLinear ((ax, ay), (bx, by), (tx, ty)) = if det /= 0 && check then (n,m) else (0,0)
    where
        det = ax * by - ay * bx
        n = (tx * by - ty * bx) `div` det -- won't be evaluated if det == 0
        m = (ax * ty - ay * tx) `div` det
        check = n*ax + m*bx == tx && n*ay + m*by == ty

add10000000000000 :: ClawMachine -> ClawMachine
add10000000000000 (a, b, (tx,ty)) = (a, b, (tx + 10000000000000, ty + 10000000000000))

sumTokens :: [ClawMachine] -> Int
sumTokens = sum . map (tokens . solveLinear)

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
    problem1 fn
    problem2 fn

problem1 :: FilePath -> IO ()
problem1 fn = do
    t <- parseMachines `fmap` readFile fn
    print $ sum $ map tokens $ concatMap solutions t


problem2 :: FilePath -> IO ()
problem2 fn = do
    t <- (map add10000000000000 . parseMachines) `fmap` readFile fn
    print $ sumTokens t