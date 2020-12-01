import qualified Data.List as L
import qualified Data.Char as C (digitToInt)

part :: (Int -> Bool) -> String -> Int
part fn n = fromEnum $ (&&) (digits' == (L.sort digits')) $ any (fn . length) $ L.group n
  where
    digits' = map C.digitToInt n

main :: IO ()
main = do
    putStr "Part 1: "
    print $ sum $ (map (part (>=2) . show)) input
    putStr "Part 2: "
    print $ sum $ (map (part (==2) . show)) input
    where
        input = [353096..843212]
