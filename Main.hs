import qualified Data.ByteString as B
import Data.Word

word8ToHex :: Word8 -> [Char]
word8ToHex w
        | w < 10 = show w
        | w < 16 = (['A'..'F'] !! ((fromIntegral w) - 10)) : []
        | otherwise = word8ToHex (w `div` 16) ++ word8ToHex (w `mod` 16) 

make2Digit :: [Char] -> [Char]
make2Digit (x:xs)
    | xs == [] = '0' : x : []
    | otherwise = x : xs

convertToHex source dest = do 
    rawImage <- B.readFile source
    let hexImage = map make2Digit . map word8ToHex . B.unpack $ rawImage
    -- print . map make2Digit . map word8ToHex . B.unpack $ content
    writeFile dest $ unwords hexImage

