module Main where
import Text.Regex.Posix (getAllTextMatches, (=~))
import Data.List (foldl')

main:: IO ()
main = do
    txt <- readFile "day_3_input.txt"

    print $ problem1 txt
    print $ problem2 txt

problem1 :: String -> Int
problem1 txt = sum (map product muls)
    where
        unscrambled = getAllTextMatches $ txt =~ "mul\\([0-9]+,[0-9]+\\)" :: [String]
        muls        = map (map read . getOperands ) unscrambled

-- Get all sequences of digits in a string
getOperands :: String -> [String]
getOperands s = getAllTextMatches $ s =~ "[0-9]+"


problem2 :: String -> Int 
problem2 txt = snd $ foldl' eval (True, 0) insts
    where 
        unscrambled = getAllTextMatches $ txt =~ "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" :: [String]
        insts = map mkInst unscrambled


-- Convert a string to an instruction
mkInst :: String -> Inst
mkInst s | take 3 s == "mul" = let [x,y] = map read . getOperands $ s in Mul x y 
mkInst s | take 3 s == "don" = Don't 
mkInst _                     = Do 


-- Available instructions
data Inst 
    = Mul Int Int 
    | Do 
    | Don't

-- Given some state, process an instruction ans return the new state
eval:: (Bool, Int) -> Inst -> (Bool, Int)
eval (_, s) Do            = (True, s) 
eval (_, s) Don't         = (False, s)
eval (True, s) (Mul x y)  = (True, s + x * y) 
eval (False, s) (Mul _ _) = (False, s)  



