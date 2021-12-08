import System.IO
import Data.Function
import Data.List
import Text.Printf

median (x:xs) n =
    let front = [a | a <- xs, a <= x]
        back = [a | a <- xs, a > x]
        q = length front
    in  if n == q
            then x
            else if q > n
                then median front n
                else median back (n - q - 1)


sumTo n = (n + 1) * n `div` 2

part2 :: [Int] -> [Int] -> Int
part2 range nums =
    let grid = map (\n -> map (sumTo . abs . subtract n) nums) range
        options = map sum grid
    in minimum options


main = do
    cont <- readFile "input.txt"
    let nums = map read 
             . filter (not . any (==',')) 
             . groupBy ((==) `on` (==',')) 
             $ cont :: [Int]
        med = median nums (div (length nums) 2)
        p1 = sum . map (abs . flip subtract med) $ nums
    print p1

    let l = minimum nums
        h = maximum nums
        r = [l..h]
    print $ part2 r nums
