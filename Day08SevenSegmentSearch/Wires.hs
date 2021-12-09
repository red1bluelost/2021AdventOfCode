import System.IO
import Data.Function
import Data.List
import Text.Printf
import Data.Maybe

fourToNum [w,x,y,z] = w * 1000 + x * 100 + y * 10 + z

decode [a,b,c,d,e,f,g] i
    | match [a,b,c,e,f,g]   = 0
    | match [c,f]           = 1
    | match [a,c,d,e,g]     = 2
    | match [a,c,d,f,g]     = 3
    | match [b,c,d,f]       = 4
    | match [a,b,d,f,g]     = 5
    | match [a,b,d,e,f,g]   = 6
    | match [a,c,f]         = 7
    | match [a,b,c,d,e,f,g] = 8
    | match [a,b,c,d,f,g]   = 9
    | otherwise = error i
    where l = length i 
          match x = length x == l
                 && length x == (length $ filter (\y -> any (`elem`y) i) x)

unique sss = case length sss of
                2 -> True
                3 -> True
                4 -> True
                7 -> True
                _ -> False

reduce [a,b,c,d,e,f,g] i
    | l == 2 = [     a,      b, filt c,      d,      e, filt f,      g]
    | l == 3 = [filt a,      b, filt c,      d,      e, filt f,      g]
    | l == 4 = [     a, filt b, filt c, filt d,      e, filt f,      g]
    | l == 7 = [     a,      b,      c,      d,      e,      f,      g]
    | l == 6 = [filt a, filt b,      c,      d,      e, filt f, filt g]
    | l == 5 = [filt a,      b,      c, filt d,      e,      f, filt g]
    | otherwise = error "fuck"
    where l = length i
          filt = filter (`elem`i)

red :: [[Char]] -> [[Char]]
red d = do
    let s = filter ((1==) . length) d
    if length s == 7
    then d
    else red (foldl (\acc [i] -> map (\x -> if length x == 1 
                                            then x 
                                            else filter (/=i) x) 
                                      acc) 
                                d s)

display (i,o) =
    let d = replicate 7 "abcdefg"
        u = red $ foldl reduce d i
    in fourToNum $ map (decode u) o
        

main = do
    cont <- readFile "input.txt"
    let ioPairs = map ((\(x,y) -> (words x, drop 1 $ words y)) 
                     . (\l -> splitAt (fromJust $ elemIndex '|' l) l))
                . lines
                $ cont
        p1 = length . filter unique . concatMap snd $ ioPairs
    print p1

    print . sum . map display $ ioPairs
        

