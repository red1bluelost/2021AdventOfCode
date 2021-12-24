import System.IO
import System.Environment
import Data.Function
import Data.List
import Text.Printf
import Data.Maybe

type RegFile = (Int, Int, Int, Int)
type Inst = [String]
type Input = [Int]

getReg :: RegFile -> String -> Int
getReg (w,x,y,z) src = case src of "w" -> w
                                   "x" -> x
                                   "y" -> y
                                   "z" -> z
                                   n   -> read n

setReg :: RegFile -> String -> Int -> RegFile
setReg (w,x,y,z) dst n = case dst of "w" -> (n,x,y,z)
                                     "x" -> (w,n,y,z)
                                     "y" -> (w,x,n,z)
                                     "z" -> (w,x,y,n)

runInst :: (RegFile, Input) -> Inst -> (RegFile, Input)
runInst (rf, i) ["inp", dst]      = (setReg rf dst (head i)                           , tail i)
runInst (rf, i) ["add", dst, src] = (setReg rf dst . ((+) `on` (getReg rf)) dst $ src , i)
runInst (rf, i) ["mul", dst, src] = (setReg rf dst . ((*) `on` (getReg rf)) dst $ src , i)
runInst (rf, i) ["div", dst, src] = (setReg rf dst . (quot `on` (getReg rf)) dst $ src, i)
runInst (rf, i) ["mod", dst, src] = (setReg rf dst . (rem  `on` (getReg rf)) dst $ src, i)
runInst (rf, i) ["eql", dst, src] = (setReg rf dst . (\b -> if b then 1 else 0) . ((==) `on` (getReg rf)) dst $ src, i)

simulate :: [Inst] -> (RegFile, Input) -> (RegFile, Input)
simulate = flip (foldl runInst)

nextLargestNumber :: Input -> Input
nextLargestNumber = init . foldr (\n (l:ns) -> if l == 0 then n - 1:9:ns else n:l:ns) [0]

largestModelNumber :: [Inst] -> Input -> Input
largestModelNumber insts input = 
    let ((_,_,_,z),_) = simulate insts ((0,0,0,0),input)
    in if z == 0 
           then input
           else largestModelNumber insts (nextLargestNumber input)

main = do
    args <- getArgs
    cont <- readFile (args !! 0)
    let insts = map words . lines $ cont
    print $ largestModelNumber insts [9,9,9,9,9,9,9,9,9,9,9,9,9,9]

