import System.IO (hFlush, stdout, readFile)

{- ADVANCE OF CODE: 2017-}
{-DAY 2/ PART 1-}

-- Lê a string que contém as informações do arquivo e transforma o a entrada em uma lista de listas,
-- onde os elementos de uma linha representam uma lista.
readSpreadsheet :: String -> [[Int]]
readSpreadsheet input = map (map read . words) $ lines input

solve :: [[Int]] -> Int
solve xxs = sum [maximum xs - minimum xs | xs <- xxs]

-- Percorre cada lista (que representa uma linha da planilha) e acha quais numeros são divisíveis e soma-se o resultadado
solve2 :: [[Int]] -> Int
solve2 spreadsheet = sum [ div x y | row <- spreadsheet, x <- row, y <- row, x /= y, mod x y == 0]
 
main :: IO ()
main = do
  
  input <- readFile "input.txt"  
  let spreadsheet = readSpreadsheet input
  
  print $ solve spreadsheet
  print $ solve2 spreadsheet
  hFlush stdout
  

