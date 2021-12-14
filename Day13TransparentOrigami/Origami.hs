import System.IO
import System.Environment
import Data.Function
import Data.List
import Text.Printf
import Data.Maybe

data Fold = X Int | Y Int
            deriving (Show)

toFold ('x':'=':num) = X (read num)
toFold ('y':'=':num) = Y (read num)

bottomRight :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
bottomRight [] (i,j) = (i,j)
bottomRight ((x,y):ps) (i,j) = bottomRight ps ((max x i),(max y j))

makeGrid :: [(Int,Int)] -> (Int,Int) -> [[Bool]]
makeGrid coords (i,j) = [[(x,y) `elem` coords | x <- [0..i]] | y <- [0..j]]

doFold :: Fold -> [[Bool]] -> [[Bool]]
doFold (Y n) grid =
    let (top,bot) = splitAt n grid
    in map (\(x,y) -> map (\(i,j) -> i||j) $ zip x y) $ zip top (reverse bot)
doFold (X n) grid = transpose . doFold (Y n) . transpose $ grid

countDots :: [[Bool]] -> Int
countDots = sum . map (length . filter id)

showGrid :: [[Bool]] -> String
showGrid = concatMap ((++"\n") . map (\x -> if x then '#' else '.'))

main = do
    args <- getArgs
    cont <- readFile (args !! 0)
    let (locs, foldS) = (\l -> splitAt (fromJust $ findIndex null l) l)
                      . lines 
                      $ cont
        folds = map (toFold . last . words) 
              . drop 1 
              $ foldS
        coords = map ((\[x,y] -> (read x, read y))
                    . filter (not . any (==',')) 
                    . groupBy ((==) `on` (==',')))
                 locs :: [(Int,Int)]
        grid = makeGrid coords (bottomRight coords (0,0))
    print $ countDots $ doFold (head folds) grid
    putStrLn $ showGrid $ foldl (flip doFold) grid folds

