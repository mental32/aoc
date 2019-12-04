import qualified Data.List as L

import qualified Text.Regex as Re (mkRegex, matchRegex)

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

hasDoubles :: (Ord a) => [a] -> [a] -> Bool -> Bool
hasDoubles [] st rs     = rs
hasDoubles (x:xs) st rs
    | x `L.elem` st = hasDoubles xs (L.delete x st) True
    | otherwise   = hasDoubles xs (x : st) rs


digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

part :: Integer -> Int
part n = fromEnum valid
  where
    ns      = digs n
    ordered = isSorted ns
    doubles = hasDoubles ns [] False
    valid   = ordered && doubles

main :: IO ()
main = print $ sum $ map part [353096..843212]
