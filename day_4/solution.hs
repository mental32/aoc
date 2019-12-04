import qualified Data.List as L
import qualified Data.Char as C (digitToInt)

partOne :: String -> Int
partOne n = fromEnum valid
  where
    ordered = let digits' = ((map C.digitToInt)) n in (==digits') (L.sort digits')
    doubles = ((any ((>=2) . length)) . L.group) n
    valid   = ordered && doubles

main :: IO ()
main = (print . sum . (map (partOne . show))) [353096..843212]
