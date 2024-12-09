
module Main where
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import Data.Char (digitToInt)

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
    d <- fmap parseText (readFile fn)
    -- return d
    print (problem1 d)
    print (problem2 d)


data Item
    = File Int Int
    | Free Int
    deriving(Eq)

instance Show Item
    where
        show (File i s) = replicate s (head $ show i)
        show (Free s) = replicate s '.'

parseText:: String -> [Item]
parseText txt = go 0 True (head $ lines txt)
    where
        go _ _ []         = []
        go i True (x:xs)  = File i (digitToInt x) : go (i+1) False xs
        go i False (x:xs) = Free (digitToInt x)   : go i True xs

getFiles :: [Item] -> [Item]
getFiles items = [ File a b | File a b <- items ]

sample :: String
sample = "2333133121414131402"
t :: [Item]
t = parseText sample
test :: ([Item], [Item], [a])
test = (t, reverse (getFiles t), [])

compress :: ([Item], [Item], [Item]) -> [Item]
compress ([],_,r) = reverse r
compress a = compress (step a)

checksum :: [Item] -> Int 
checksum items = sum (zipWith (*) [0..] (concatMap getId items))

getId :: Item -> [Int]
getId (File i s) = replicate s i 
getId (Free s)   = replicate s 0

step :: ([Item], [Item], [Item]) -> ([Item], [Item], [Item])
step (File i1 s1:xs, f@(File i2 s2:ys), r)
    | i2 == i1 = ([], ys, File i2 s2:r)
    | i2 < i1  = ([],f,r)
    | i2 > i1  = (xs, f, File i1 s1:r)
step (Free s1:xs, f@(File i2 s2:ys), r)
    | s2 == s1 = (xs, ys, File i2 s2:r)
    | s2 < s1  = (Free (s1-s2):xs, ys, File i2 s2:r)
    | s2 > s1  = (xs, File i2 (s2 - s1):ys, File i2 s1:r)
step _ = error "should now happen"


problem1 :: [Item] -> Int
problem1 items = checksum $ compress (items, reverse (getFiles items), [])

-- Applies step until all files are placed of left in place
compress2 :: ([Item],[Item]) -> [Item]
compress2 (i,[]) = i 
compress2 items = compress2 (step2 items) 

-- In a step we try to fit an item just once
step2 :: ([Item],[Item]) -> ([Item],[Item])
step2 (items, f:ys) = (fitItem f items, ys)

-- Tries to fit an item before we see it in the list
fitItem :: Item -> [Item] -> [Item]
fitItem item [] = []
-- If we're seeing a file, 
-- if it's our item then stop trying to place it
-- otherwise keep the file and try to place in the remainder
fitItem item r@(File i s:xs)
    | item == File i s = r                           -- stop trying to fit, we saw it in the list
    | otherwise        = File i s : fitItem item xs  -- keep if it's a file
-- If we're seeing free space, 
-- if it fits exactly then place the item and remove it from the rest of the list
-- if it fits then place the item, keep the free space remainder and remove the item from the rest of the list
-- otherwise keep the free space try to place the item in the remainder
fitItem item@(File i s) r@(Free s':xs) 
    | s == s'   = item : replace item xs                
    | s < s'    = item : Free (s'-s) : replace item xs  
    | otherwise = Free s':fitItem item xs  

replace f@(File i s) (x:xs) | x == f = Free s:xs 
replace f (x:xs)                     = x: replace f xs 

test2 :: ([Item], [Item])
test2 = (t, reverse (getFiles t))

display :: ([Item],[Item]) -> [Char]
display (i,f) = concatMap show i ++ "     " ++ show f

tst :: ([Item], [Item]) -> Int -> IO ()
tst item n = do 
    let l = take n $ iterate step2 item    
    putStrLn $ unlines (map display l)

problem2 :: [Item] -> Int
problem2 items = checksum $ compress2 (items, reverse (getFiles items))
