module Solution ( compute, withInputData ) where

import qualified Data.Text.Read as R (decimal)
import qualified Data.Text as T (Text, pack, splitOn, empty, strip)
import qualified Data.Either as E (fromRight)
import System.IO (withFile, IOMode( ReadMode ), hGetContents)
import Data.List

compute :: (Integral a) => a -> [a] -> [a]
compute _ [] = []
compute i xs
    | opp == 99 = xs
    | otherwise = compute (i + 4) $ (left ++ [lhs `fn` rhs] ++ tail right)
    where
        opp = genericIndex xs i
        fn = if opp == 1 then (+) else (*)
        lhs = genericIndex xs $ genericIndex xs (i + 1)
        rhs = genericIndex xs $ genericIndex xs (i + 2)
        dst = genericIndex xs (i + 3)
        (left, right) = genericSplitAt dst xs

parse :: (Integral a) => T.Text -> [a]
parse = map (fst . E.fromRight (99, T.empty) . R.decimal) . T.splitOn (T.pack ",") . T.strip

withInputData :: (Integral a) => ([a] -> IO ()) -> IO ()
withInputData fn = withFile
    "input_data" ReadMode (\handle -> do
        contents <- hGetContents handle
        fn $ parse $ T.pack contents)

-- This solution is only for part 1
main :: IO ()
main = withInputData (print . head . (compute 0))
