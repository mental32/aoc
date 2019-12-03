import System.IO (withFile, hGetContents, IOMode ( ReadMode ))
import Data.List.Split as Sp (splitOn)
import qualified Data.Set as S (intersection, map, findMin, fromList, toList)

type Point = (Integer, Integer)

fooX :: Char -> Integer
fooX 'L' = -1
fooX 'R' = 1
fooX 'U' = 0
fooX 'D' = 0

fooY :: Char -> Integer
fooY 'L' = 0
fooY 'R' = 0
fooY 'U' = 1
fooY 'D' = -1

points :: Point -> [String] -> [Point]
points p []          = []
points p ((y:ys):xs) = zs ++ (points z xs)
    where (z:zs) = points' y (read ys) p

points' :: Char -> Integer -> Point -> [Point]
points' _ 0 p = [p]
points' d n (x, y) = (points' d (n - 1) (x', y')) ++ [(x', y')]
    where
        x' = x + (fooX d)
        y' = y + (fooY d)

calc :: String -> String -> Integer
calc these those = S.findMin . (S.map (uncurry (+))) $ (points'' these) `S.intersection` (points'' those)
    where points'' = S.fromList . (points (0, 0)) . (Sp.splitOn ",")

main :: IO ()
main = withFile
    "input_data" ReadMode (\handle -> do
        contents <- hGetContents handle
        let (x:y:_) = take 2 $ lines contents in print $ calc x y)
