import Data.List (foldl')

makeSpreadsheet :: (Read a, Integral a) => String -> [[a]]
makeSpreadsheet = (fmap . fmap) read . fmap words . lines

minmax :: Integral a => [a] -> (a, a)
minmax xs = foldl' (\(mi, ma) x -> (min mi x, max ma x)) (head xs, head xs) xs

checksum :: Integral a => [(a, a)] -> a
checksum = foldl' (\acc (mi, ma) -> acc + (ma - mi)) 0

solveA :: Integral a => [[a]] -> a
solveA = checksum . fmap minmax

main :: IO ()
main = do
  txt <- readFile "inputs/02.txt"
  let spreadsheet = makeSpreadsheet txt

  putStrLn "Solution A:"
  print $ solveA spreadsheet
