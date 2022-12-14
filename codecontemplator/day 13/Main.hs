module Main where

import Text.Parsec
import Debug.Trace
import Data.List.Split (splitOn)

data NestedInts = I Int | L [NestedInts] deriving Show

example = "[1,[2,[3,[4,[5,6,7]]]],8,19]"

pint:: Parsec String st Int
pint = read <$> many1 digit

plist :: Parsec String st NestedInts
plist =
    (I <$> pint) 
    <|> 
    (L <$> between (char '[') (char ']') (sepBy plist (char ',')))
    
compareL :: NestedInts -> NestedInts -> Ordering 
compareL (I i1) (I i2) = -- trace (show i1 ++ " ~ " ++ show i2) $ 
    compare i1 i2
compareL (I i) l = compareL (L [I i]) l
compareL l (I i) = compareL l (L [I i])
compareL (L []) (L []) = EQ
compareL (L []) (L (y:_)) = LT
compareL (L (x:_)) (L []) = GT
compareL (L (x:xs)) (L (y:ys)) = 
    case compareL x y of
        EQ -> compareL (L xs) (L ys)
        v -> v

process :: [String] -> Bool
process [leftStr,rightStr] = 
    let 
        p x = case parse plist "" x of
                Left msg -> error (show msg)
                Right r -> r    
        left = p leftStr
        right = p rightStr                
    in  
        compareL left right == LT        
process _ = error "wrong group size"

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let groups = splitOn [""] contents
    let result = map process groups
    let final = sum $ map snd $ filter fst $ zip result [1..]
    print final
