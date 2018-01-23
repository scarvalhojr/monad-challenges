
import Numeric (readHex)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

hexDecode :: String -> String
hexDecode = map (toEnum . fst . head . readHex) . (chunksOf 2)
