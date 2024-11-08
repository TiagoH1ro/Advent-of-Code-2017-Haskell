import System.IO (hFlush, stdout, readFile)

{- ADVANCE OF CODE: 2017-}
{-DAY 2/ PART 1-}

--Lê a string que contém as informações do arquivo e transforma o a entrada em uma lista de listas,
--onde os elementos de uma linha representam uma lista.
readSpreadSheet :: String -> [[Int]]
readSpreadSheet input = map (map read . words) $ lines input

solve :: [[Int]] -> Int
solve xxs = sum [maximum xs - minimum xs | xs <- xxs]

main :: IO ()
main = do
  
  input <- readFile "input.txt"  
  let spreadSheet = readSpreadSheet input
  
  print $ solve spreadSheet
  hFlush stdout

