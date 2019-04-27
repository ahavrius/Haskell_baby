import Text.Read
import Data.Maybe
import Control.Monad

wordsOrNums::[String]->[Either String Integer]
wordsOrNums = map (\x -> maybe (Left x) (Right) (readMaybe x))

strToInts :: String -> [Integer]
strToInts = map (either (\x -> 0) (id)) . wordsOrNums . words

main = do
    inputStr <- getLine
    let res = strToInts inputStr
    mapM_ print res

