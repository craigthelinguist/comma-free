
import Control.Monad
import Data.List
import Text.Read

{- Check if a set of Strings is comma-free.
   If the set of strings is comma-free, return Nothing.
   Otherwise return an example s which violates the comma-free property.
-}
commaFreeViolation :: [String] -> Maybe (String, String, String)
commaFreeViolation words =
    -- Figure out every concatenated pair of words.
    let concatenatedPairs = allConcatenatedPairs words
    -- Map each concatenated pair to (the pair of words, the substrings in concatenation)
        allSubstrs = map (\c -> (unconcat c, allSubstrings c)) concatenatedPairs
    -- Map each (pair,substrings) to (pair, substrings in the code)
        conflicts = map (\(pair,substrs) -> (pair, intersect substrs words)) allSubstrs
    -- Get the first conflict, if there is one.
    in getConflict conflicts

{- From a list of possible conflicts, extract the first.
   The input is a list of ((String,String), [String])
      (String,String) is a pair of words from the code.
      [String] is the list of substrings in the concatenated words, which are also in the code. -}
getConflict :: [  ((String,String), [String])  ] -> Maybe (String, String, String)
getConflict [] = Nothing
getConflict ( ( _   , []       ) : rest) = getConflict rest
getConflict ( ((a,b), conflicts) : _   ) = Just (a,b,(head conflicts))

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
checkCodewords :: Int -> [String] -> IO ()
checkCodewords wordLen words = forever $ do
    w <- getWordOfLength wordLen
    if elem w words then checkCodewords wordLen words
    else let words2 = (w:words) in
            case commaFreeViolation words2 of
                Nothing -> checkCodewords wordLen words2
                Just (s1, s2, s3) -> do
                    putStrLn $ "The pair " ++ s1 ++ "|" ++ s2 ++ " conflicts with " ++ s3
                    checkCodewords wordLen words

main :: IO ()
main = do
    putStrLn "Enter the length of code-words."
    n <- getLineInt
    putStrLn $ "Enter some words of length " ++ (show n) ++ "."
    checkCodewords n []
