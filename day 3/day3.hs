layerSide :: Int -> Int
layerSide n
    | even result = result + 1
    | otherwise = result
    where result = (ceiling . sqrt . fromIntegral) n

layerMax :: Int -> Int
layerMax n = layerSide n ^2

layerMin :: Int -> Int
layerMin n = (layerSide n - 2) ^2 + 1

layerIndex :: Int -> Int
layerIndex n = layerSide n `div` 2

layerPositions :: Int -> [(Int, Int)]
layerPositions n = -- n is the number we want to find the position
    let k = layerIndex n
        right = [(k, y) | y <- [k-1, k-2 .. (-k)]]
        top = [(x, -k) | x <- [(k-1), k-2 .. (-k)]]
        left = [(-k, y) | y <- [-k+1 .. k]]
        bottom = [(x, k) | x <- [-k+1 .. k]]
        
    in right ++ top ++ left ++ bottom

spiralPosition :: Int -> (Int, Int)
spiralPosition n =
    let distanceFromMin = n - layerMin n
        positions = layerPositions n
    in positions !! distanceFromMin

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x, y) = abs x + abs y
main = do
    input <- getLine
    let n = read input

    print $ (manhattanDistance.spiralPosition) n

