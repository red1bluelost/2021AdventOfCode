import System.IO
import System.Environment
import Data.Function
import Data.List
import Text.Printf
import Data.Maybe
import qualified Data.Map as M

type RegFile = (Int, Int, Int, Int)
type Inst = [String]
type Block = [Inst]
type Input = Int
type Inputs = [Int]
type Cache = M.Map (RegFile, Input) RegFile
type Caches = M.Map Int Cache

erf :: RegFile
erf = (0,0,0,0)

maxInput :: Inputs
maxInput = [9,9,9,9,9,9,9,9,9,9,9,9,9,9]

minInput :: Inputs
minInput = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]

replace :: [a] -> Int -> a -> [a]
replace l i e = let (x,_:ys) = splitAt i l
                in x ++ (e:ys)

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

runInst :: (RegFile, Inputs) -> Inst -> (RegFile, Inputs)
runInst (rf, i) ["inp", dst]      = (setReg rf dst (head i)                           , tail i)
runInst (rf, i) ["add", dst, src] = (setReg rf dst . ((+) `on` (getReg rf)) dst $ src , i)
runInst (rf, i) ["mul", dst, src] = (setReg rf dst . ((*) `on` (getReg rf)) dst $ src , i)
runInst (rf, i) ["div", dst, src] = (setReg rf dst . (quot `on` (getReg rf)) dst $ src, i)
runInst (rf, i) ["mod", dst, src] = (setReg rf dst . (rem  `on` (getReg rf)) dst $ src, i)
runInst (rf, i) ["eql", dst, src] = (setReg rf dst . (\b -> if b then 1 else 0) . ((==) `on` (getReg rf)) dst $ src, i)

simulate :: (RegFile, Inputs) -> [Inst] -> (RegFile, Inputs)
simulate = foldl runInst

splitBlocks :: [Inst] -> [Block]
splitBlocks [] = []
splitBlocks (i:is) = let (f,e) = span (/=["inp","w"]) is
                     in (i:f):splitBlocks e

runBlock :: Block -> (RegFile, Cache) -> Input -> (RegFile, Cache)
runBlock b (r,c) i = case M.lookup (r,i) c of 
                         Just or -> (or, c)
                         Nothing -> let (nr,_) = simulate (r,[i]) b
                                    in (nr, M.insert (r,i) nr c)

simulateM :: (RegFile, Inputs, Caches) -> (Block, Int) -> (RegFile, Inputs, Caches)
simulateM (rf, x:xs, cs) (b, i) = let (nr, ncs) = runBlock b (rf, M.findWithDefault M.empty i cs) x
                                  in (nr, xs, M.insert i ncs cs)

simulateMem :: (RegFile, Inputs, Caches) -> [Block] -> (RegFile, Inputs, Caches)
simulateMem a b = foldl simulateM a (zip b [0..])

nextLargestNumber :: Inputs -> Inputs
nextLargestNumber = init . foldr (\n (l:ns) -> if l == 0 then n - 1:9:ns else n:l:ns) [0]

largestModelNumber :: [Block] -> Caches -> Inputs -> Inputs
largestModelNumber blocks caches input =
    let ((_,_,_,z),_,ncs) = simulateMem (erf, input, caches) blocks
    in if z == 0
        then input
        else largestModelNumber blocks ncs (nextLargestNumber input)

main = do
    args <- getArgs
    cont <- readFile (args !! 0)
    let insts = map words . lines $ cont
        blocks = splitBlocks insts
    print $ largestModelNumber blocks M.empty maxInput
