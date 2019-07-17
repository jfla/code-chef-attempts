import Data.Maybe
import Data.List
import System.IO

-- TO RUN: cat metam_input.txt | runghc metam

main = do
    input <- getInput
    let shortestPairs = map (\pair -> (pair, shortestBetween (dict input) pair)) (pairs input)
    
    outputResults shortestPairs

--unsafe
getInput :: IO Input
getInput = (fromJust . parseInput) `fmap` hGetContents stdin --readFile "metam_input.txt"

outputResults :: [((String,String),Int)] -> IO ()
outputResults rs = mapM_ outputResult rs
    where
        outputResult ((s1,s2),l) = putStrLn $ s1 ++ " " ++ s2 ++ " " ++ show l

data Input = Input { dict :: [String], pairs :: [(String, String)] }
    deriving Show

parseInput :: String -> Maybe Input
parseInput contents =
    let (dict_, _:pairslines) = span (/= "#") (lines contents) -- _ to get rid of # in pairslines
        pairsm = mapM strToPair pairslines
        
        -- unsafe
        strToPair str =
            case words str of
              a:b:[] -> Just (a,b)
              _      -> Nothing
    in  case pairsm of
            Just pairs_ -> Just $ Input dict_ pairs_
            Nothing     -> Nothing

-- must take away 1 because start point not included
shortestBetween :: [String] -> (String, String) -> Int
shortestBetween dict (start,end) = (minimum $ map length $ routes dict start end []) - 1

connected :: String -> String -> Bool
connected a b = (length a == length b) && (noChanges a b == 1)
    where
        noChanges a b = noTrues $ zipWith (/=) a b
        noTrues :: [Bool] -> Int
        noTrues = length . filter id

routes :: [String] -> String -> String -> [String] -> [[String]]
routes dict start end sofar
    | start == end  = [end:sofar]
    | otherwise     = do
        next <- filter (\word -> connected start word && not (word `elem` sofar)) dict
        routes dict next end (start:sofar)

dict_ = ["dip","lip","mad","map","maple","may","pad","pip","pod","pop","sap","sip","slice","slick","spice","stick","stock"]
start = "pod"
end = "pip"
sofar = []
