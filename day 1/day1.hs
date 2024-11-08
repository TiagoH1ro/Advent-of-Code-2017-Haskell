import System.IO (hFlush, stdout, readFile)

{- ADVANCE OF CODE: 2017-}
{-DAY 1/ PART 1-}

-- Percorre a String salvando as tuplas do caracter atual com o próximo
matchWithNextDigit :: String -> [(Char, Char)]
-- Para isto é criado uma lista cíclica que começa a partir do segundo caracter da string
matchWithNextDigit input = zip input $ drop 1 $ cycle input

-- Percorre a lista de tuplas e caso seja igual, coloca o valor em uma lista para serem somados no final
compareDigits :: [(Char, Char)] -> [Int]
compareDigits xs = [ read [x] | (x, y) <- xs , x == y]

solve :: String -> Int
solve input = sum $ compareDigits $ matchWithNextDigit input

{-DAY 1/ PART 2-}

-- Percorre a String fazendo uma tupla com o caracter que está input.length/2 de distância
matchWithHalfwayAroundDigit :: String -> [(Char, Char)]
-- Para isso é criado uma lista cíclica a partir da metade da string original
matchWithHalfwayAroundDigit input = zip input $ drop halfLenght $ cycle input
  where halfLenght =  length input `div` 2

solve2 :: String -> Int
solve2 input = sum $ compareDigits $ matchWithHalfwayAroundDigit input

main :: IO ()
main = do

  input <- readFile "input.txt"
  
  putStrLn $ "Solution part one: " <> show (solve input)
  putStrLn $ "Solution part two: " <> show (solve2 input)
  hFlush stdout
  

