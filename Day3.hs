module Main where
import Text.Regex.Posix (getAllTextMatches, (=~))

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

getOperands :: String -> [String]
getOperands s = getAllTextMatches $ s =~ "[0-9]+"

problem2 :: String -> Int 
problem2 txt = eval True 0 insts
    where 
        unscrambled = getAllTextMatches $ txt =~ "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" :: [String]
        insts = map mkInst unscrambled


mkInst :: String -> Inst
mkInst s | take 3 s == "mul" = let [x,y] = map read . getOperands $ s in Mul x y 
mkInst s | take 3 s == "don" = Don't 
mkInst _                     = Do 


data Inst 
    = Mul Int Int 
    | Do 
    | Don't

eval:: Bool -> Int -> [Inst] -> Int
eval _ s [] = s 
eval _ s (Do:xs) = eval True s xs 
eval _ s (Don't:xs) = eval False s xs
eval True s (Mul x y:xs) = eval True (s+x*y) xs 
eval False s (Mul _ _:xs) = eval False s xs  

