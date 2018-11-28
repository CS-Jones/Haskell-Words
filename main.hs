import Data.Char
import Data.Ord
import Data.List
import Data.Monoid

dropNonLetters xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\'") ]

toWordList :: String -> [String]
toWordList = words . map toLower . dropNonLetters

countCommonWords ::  [String] -> Int
countCommonWords xs = length (intersect xs com)
    where com = ["the", "be", "to", "of", "and", "a", "in", "that", "have", "I", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at"]

dropCommonWords :: [String] -> [String]
dropCommonWords xs = filter (\x -> x `notElem` common) xs
    where common = ["the", "be", "to", "of", "and", "a", "in", "that", "have", "I", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at"]

countWords :: [String] -> [(String,Int)]
countWords xs = map (\x -> (head x, length x)) $ group $ sort xs

sortWords :: [(String,Int)] -> [(String,Int)]
sortWords xs = (reverse.sortBy (comparing snd)) xs

toAskBar x = show (replicate (snd x) '*') ++ "  ->  " ++ (fst x) ++ "\n"
makeHistogram xs = concat $ take 20 (map toAskBar xs)
main = do
  putStrLn "Introduce a filename:"
  fname <- getLine
  textdata <- readFile fname
  let wordlist = toWordList textdata
  putStrLn "Report:"
  putStrLn ("\t" ++ (show $ length wordlist) ++ " words")
  putStrLn ("\t" ++ (show $ countCommonWords wordlist) ++ " common words")
  putStrLn "\nHistogram of the most frequent words (excluding common words):"
  putStr $ makeHistogram $ sortWords $ countWords $ dropCommonWords $ wordlist
