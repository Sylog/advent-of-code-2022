module Main where

import Text.Parsec
import Data.List.Split (splitOn)
import Data.Ord (Ord)
import Data.Either (fromRight)
import Data.List (sort)

data NestedInts = I Int | L [NestedInts] deriving Show

pint:: Parsec String st Int
pint = read <$> many1 digit

plist :: Parsec String st NestedInts
plist =
    (I <$> pint) 
    <|> 
    (L <$> between (char '[') (char ']') (sepBy plist (char ',')))

instance Eq NestedInts where
    (==) a b = compare a b == EQ

instance Ord NestedInts where
    compare (I i1) (I i2) = compare i1 i2
    compare (I i) l = compare (L [I i]) l
    compare l (I i) = compare l (L [I i])
    compare (L []) (L []) = EQ
    compare (L []) (L (y:_)) = LT
    compare (L (x:_)) (L []) = GT
    compare (L (x:xs)) (L (y:ys)) = 
        case compare x y of
            EQ -> compare (L xs) (L ys)
            v -> v

main :: IO ()
main = do
    contents <- filter (/="") . lines <$> readFile "input.txt"
    let parseHelper x = case parse plist "" x of
            Left msg -> error (show msg)
            Right r -> r 
    let markers = map parseHelper ["[[2]]", "[[6]]"]
    let elems = map parseHelper (contents)
    let elems' = sort (elems ++ markers)
    mapM_ print elems'
    let elemsIndex = foldr (*) 1 $ map snd $ filter (flip elem markers . fst ) (zip elems' [1..])
    print elemsIndex
