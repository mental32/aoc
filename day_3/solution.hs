import System.IO (withFile, hGetContents, IOMode ( ReadMode ))
import qualified Data.List.Split as Sp (splitOn)
import qualified Data.Set as S (intersection, map, findMin, fromList, toList)
import qualified Data.Map as M (fromList)

type Point = (Integer, Integer)

fooX = M.fromList $ zip "LRUD" (-1, 1, 0, 0)
fooY = M.fromList $ zip "LRUD" (0, 0, 1, -1)

points :: Point -> [String] -> [Point]
points p []          = []
points p ((y:ys):xs) = zs ++ (points z xs)
    where (z:zs) = points' y (read ys) p

points' :: Char -> Integer -> Point -> [Point]
points' _ 0 p = [p]
points' d n (x, y) = (points' d (n - 1) (x', y')) ++ [(x', y')]
    where
        x' = x + ((M.!) fooX d)
        y' = y + ((M.!) fooY d)

calc :: String -> String -> Integer
calc these those = S.findMin . (S.map (uncurry (+))) $ (points'' these) `S.intersection` (points'' those)
    where points'' = S.fromList . (points (0, 0)) . (Sp.splitOn ",")

main :: IO ()
main = withFile
    "input_data" ReadMode (\handle -> do
        contents <- hGetContents handle
        let (this:that:_) = take 2 $ lines contents in print $ calc this that)
