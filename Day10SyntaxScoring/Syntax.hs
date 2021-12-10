import System.IO
import Data.Function
import Data.List
import Text.Printf

data LineState = Good | Incomplete String | Corrupted Char Char
                 deriving (Eq, Show)

getLineState :: String -> LineState
getLineState [] = Good
getLineState (x:xs) = getImpl xs [x]
    where getImpl []      []      = Good
          getImpl []      r       = Incomplete r
          getImpl (')':i) ('(':o) = getImpl i o
          getImpl (']':i) ('[':o) = getImpl i o
          getImpl ('}':i) ('{':o) = getImpl i o
          getImpl ('>':i) ('<':o) = getImpl i o
          getImpl ('(':i) o       = getImpl i ('(':o)
          getImpl ('[':i) o       = getImpl i ('[':o)
          getImpl ('{':i) o       = getImpl i ('{':o)
          getImpl ('<':i) o       = getImpl i ('<':o)
          getImpl (x:_)   (y:_)   = Corrupted y x
          getImpl x y = error ("*" ++ x ++ "   " ++ y ++ "*")

score (Corrupted _ x) = case x of
                            ')' -> 3
                            ']' -> 57
                            '}' -> 1197
                            '>' -> 25137
score _ = 0

isIncomplete (Incomplete _) = True
isIncomplete _ = False

scoreI (Incomplete s) = foldl (\acc c -> acc * 5 + (scoreC c)) 0 s
    where scoreC '(' = 1
          scoreC '[' = 2
          scoreC '{' = 3
          scoreC '<' = 4
scoreI _ = error "Nope"

main = do
    cont <- readFile "input.txt"
    let ls = lines cont
    print $ sum . map (score . getLineState) $ ls

    let is = sort 
           . map (scoreI . getLineState) 
           . filter (isIncomplete . getLineState) 
           $ ls
        m = is !! (div (length is) 2)
    print m

