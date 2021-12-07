import System.IO
import Data.Function
import Data.List
import Text.Printf

get2D :: [[Int]] -> Int -> Int -> Int
get2D t d c = (t !! d) !! c

fishes :: Int -> [[Int]]
fishes 0 = [[1,1,1,1,1,1,1,1,1]]
fishes d = 
    let l = d - 1
        f = fishes l
        tf = get2D f l
        r = [(tf 6) + (tf 8), (tf 0), (tf 1), (tf 2), (tf 3), (tf 4), (tf 5), (tf 6), (tf 7)]
    in f ++ [r]
        

main = do
    cont <- readFile "input.txt"
    let nums = map read 
             . filter (not . any (==',')) 
             . groupBy ((==) `on` (==',')) 
             $ cont :: [Int]
        fish = fishes 256
        p x = sum . map (get2D fish x) $ nums
    mapM_ (\x -> printf "(%d) %d\n" x (p x)) [0..256]
