
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Sets
import Text.Read

{- Check if a set of Strings is comma-free.
   If the set of strings is comma-free, return Nothing.
   Otherwise return an example s which violates the comma-free property.
-}
commaFreeViolation :: Sets.Set String -> Maybe (String, String)
commaFreeViolation wordSet =

    -- Find the substrings of the concatenation of every pair of words. If any of those substrings
    -- belongs to the set of codewords then we have a conflict.
    let words = Sets.toList wordSet
        pairsOfWords = allConcatenatedPairs words
        allSubstrs = Sets.fromList . concat $ (map (\w -> allSubstrings w) pairsOfWords)
        conflicts = Sets.toList $ Sets.intersection allSubstrs wordSet

     -- If there was a conflict, return the conflicting pair of words.
     in if null conflicts then Nothing else Just (unconcat (head conflicts))

{- From a concatenation of two words from a block-code, extract the two words. -}
unconcat :: String -> (String, String)
unconcat s = let len = (length s) `quot` 2
              in ((sublist 0 len s), (sublist len (len*2) s))

{- Get the set formed by concatenating every pair of code-words. -}
allConcatenatedPairs :: [String] -> [String]
allConcatenatedPairs words =
    let allPairs = [(u,v) | u <- words, v <- words]
     in map (\(x,y) -> x ++ y) allPairs

{- sublist a b l returns the sublist of l starting from index a inclusive and ending
   at index b exclusive. -}
sublist :: Int -> Int -> [a] -> [a]
sublist start end = drop start . take end

{- Given a concatenating u ++ v of two codewords, return all substrings. u and v will
   not be in the list returned. -}
allSubstrings :: String -> [String]
allSubstrings word =
    -- Since word is concatenation of two equal-sized code-words the length is always
    -- even and so integer division plays nice.
    let len = (length word) `quot` 2
        indices = [1..(len-1)]

    -- Map each index to the substring starting at that index.
    in map (\i -> sublist i (i+len) word) indices

{- Ask the user for an Int; if they don't supply an Int they're asked again. -}
getLineInt :: IO Int
getLineInt = do
    line <- getLine
    case readMaybe line of
        Just x -> return x
        Nothing -> putStrLn "Must enter a number. " >> getLineInt

{- Ask the user for a string of length n. Keep asking until they provide a string of length n. -}
getWordOfLength :: Int -> IO String
getWordOfLength n = do
    line <- getLine
    if (length line) /= n
        then do
            putStrLn $ "Must enter a word of length " ++ (show n)
            getWordOfLength n
        else
            return line

{- Ask the user for codewords, checking if each new one violates the comma-free property. -}
checkCodewords :: Int -> Sets.Set String -> IO ()
checkCodewords wordLen words = forever $ do
    w <- getWordOfLength wordLen
    let words2 = (Sets.insert w words) in
        case commaFreeViolation words2 of
            Nothing -> checkCodewords wordLen words2
            Just (s1,s2) -> do
                putStrLn $ "Conflict between " ++ s2 ++ " and " ++ s1
                checkCodewords wordLen words

main :: IO ()
main = do
    putStrLn "Enter the length of code-words."
    n <- getLineInt
    putStrLn $ "Enter some words of length " ++ (show n) ++ "."
    checkCodewords n Sets.empty
