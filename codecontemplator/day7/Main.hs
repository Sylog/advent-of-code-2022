import Data.List (break, isPrefixOf, concat, intercalate, sortBy)
import Debug.Trace (trace)
import Data.Maybe (fromJust)
import Control.Monad.State.Lazy (State,modify,execState)
import Data.Map (Map)
import qualified Data.Map as Map
  
-- Ref: http://learnyouahaskell.com/zippers

type Name = String
type Size = Integer
data FSItem = File Name Size | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsEmpty :: FSZipper
fsEmpty = (Folder "/" [], [])

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs) : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsNew :: FSItem -> FSZipper -> FSZipper
fsNew item@(File _ _) (Folder folderName items, bs) = (Folder folderName (item : items), bs)
fsNew item@(Folder _ _) (Folder folderName items, bs) = (item, FSCrumb folderName items [] : bs)
 
topMost :: FSZipper -> FSZipper
topMost (t, []) = (t, [])
topMost z = topMost (fsUp z)

fsGet :: FSZipper -> FSItem
fsGet = fst . topMost

process :: FSZipper -> String -> FSZipper
process focus s
  | s == "$ cd /" = trace "topmost" $ topMost focus
  | s == "$ cd .." = fsUp focus
  | "$ cd " `isPrefixOf` s =
    let dirName = drop (length "$ cd ") s
    in fsNew (Folder dirName []) focus
  | s == "$ ls" = focus
  | "dir " `isPrefixOf` s = focus
  | otherwise =
    let [size, name] = words s
     in fsNew (File name (read size)) focus

size :: String -> FSItem -> State (Map String Integer) Integer
size path (File name size) = return size
size path (Folder name items) = do
  let path' = path ++ "/" ++ name
  result <- sum <$> mapM (size path') items
  modify (\d -> Map.insert (path') result d)
  return result

getSize :: FSItem -> [(String,Integer)]
getSize item = 
  let m = execState (size "" item) (Map.empty) 
  in Map.toList m

main = do
  text <- lines <$> readFile "input.txt"
  let fsTree = fsGet $ foldl process fsEmpty text
  let result = sum $ filter (<=100000) $ map snd $ getSize fsTree
  print result  -- 1845346