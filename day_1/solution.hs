import qualified Data.Set as S (fromList)
import System.IO (withFile, IOMode( ReadMode ), hGetContents)

calculate :: (Integral a) => a -> a
calculate x
    | weight <= 0 = 0
    | otherwise   = weight + (calculate weight)
  where
      weight = (x `quot` 3) - 2

main = withFile
    "input_data" ReadMode (\handle -> do
        contents <- ((map calculate) . (map read) . lines) <$> hGetContents handle
        print $ (sum . (S.fromList)) contents)
