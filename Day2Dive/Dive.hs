import System.IO


posDepScan :: [(String, Int)] -> (Int, Int)
posDepScan = foldl posDep (0, 0)
    where posDep (p, d) ("forward", x) = (p + x, d)
          posDep (p, d) ("down", x) = (p, d + x)
          posDep (p, d) ("up", x) = (p, d - x)

posDepAimScan :: [(String, Int)] -> (Int, Int, Int)
posDepAimScan = foldl posDepAim (0, 0, 0)
    where posDepAim (p, d, a) ("forward", x) = (p + x, d + a * x, a)
          posDepAim (p, d, a) ("down", x) = (p, d, a + x)
          posDepAim (p, d, a) ("up", x) = (p, d, a - x)

main = do
    cmds <- map words . lines <$> readFile "input.txt"
    let cs = map (\(x:y:[]) -> (x, read y :: Int)) cmds
        (p1, d1) = posDepScan cs
        (p2, d2, a2) = posDepAimScan cs
    putStrLn "Part 1 = "
    print $ p1 * d1
    putStrLn "Part 2 = "
    print $ p2 * d2
    
