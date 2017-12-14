import Data.Char (digitToInt, isSpace)

collapse :: Integral a => (a, a) -> a
collapse (x, y)
  | x == y = x
  | otherwise = 0

pairWithStep :: Integral a => Int -> [a] -> [(a,a)]
pairWithStep n xs = zip xs companion
  where
    companion = drop n xs ++ take n xs

solveA :: Integral a => [a] -> a
solveA = sum . fmap collapse . pairWithStep 1

solveB :: Integral a => [a] -> a
solveB xs = (sum . fmap collapse . pairWithStep l) xs
  where
    l = length xs `div` 2

main :: IO ()
main = do
  txt <- readFile "inputs/01.txt"
  let noLineBreak = filter (not . isSpace) txt
  let numbers = fmap digitToInt noLineBreak

  putStrLn "Solution A:"
  print $ solveA numbers
  putStrLn ""

  putStrLn "Solution B:"
  print $ solveB numbers
