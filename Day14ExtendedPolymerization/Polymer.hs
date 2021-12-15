import System.IO
import System.Environment
import Data.Function
import Data.List
import Text.Printf
import Data.Maybe
import qualified Data.Map as M

type PairMap = M.Map (Char,Char) Char
type CCounter = M.Map Char Integer
type PCounter = M.Map (Char,Char) Integer

firstCount :: String -> (CCounter, PCounter)
firstCount s = let cc = foldl (\acc c -> M.insertWith (+) c 1 acc) M.empty s
                   pc = foldl (\acc p -> M.insertWith (+) p 1 acc) M.empty (zip s (tail s))
               in (cc, pc)

stepPoly :: PairMap -> (CCounter, PCounter) -> (CCounter, PCounter)
stepPoly m (cc,pc) = 
    let (ccc,pcc) = M.foldrWithKey f (cc,M.empty) pc
    in (ccc,pcc)
    where f (a,b) n (c,p) =
            let nc = fromJust $ M.lookup (a,b) m
                newC = M.insertWith (+) nc n c
                newP = M.insertWith (+) (a,nc) n
                     . M.insertWith (+) (nc,b) n
                     $ p
            in (newC,newP)


stepPolyTimes map cnrs n = foldl (\acc _ -> stepPoly map acc) cnrs [1..n]

score cc =
    let ls = map snd . M.toList $ cc
    in (maximum ls) - (minimum ls)


main = do
    args <- getArgs
    cont <- readFile (args !! 0)
    let lns = lines cont
        tplt = head lns
        pairs = M.fromList . map (\s@(a:b:_) -> ((a,b), last s)) . drop 2 $ lns
        (cc,pc) = firstCount tplt
        (cc1,_) = stepPolyTimes pairs (cc,pc) 10
        (cc2,_) = stepPolyTimes pairs (cc,pc) 40
    print $ score cc1
    print $ score cc2

