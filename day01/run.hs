module Main where

fuel1 :: Int -> Int
fuel1 = subtract 2 . (`div` 3)

fuel2 :: Int -> Int
fuel2 n
  | m <= 0 = 0
  | otherwise = m + fuel2 m
  where m = fuel1 n

solve :: (Int -> Int) -> [Int] -> Int
solve f = sum . map f

main :: IO ()
main = do
  contents <- map read . lines <$> readFile "data.txt"
  print $ map ($ contents)  [solve fuel1, solve fuel2]