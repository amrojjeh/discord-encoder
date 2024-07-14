import Data.Map (Map, empty, insertWith, toList)
import Data.List (sortBy)
import Control.Monad.RWS.Lazy (MonadState(put))

type Encoded = String

type FrequencyTable = Map Char Int

data Tree
  = TreeFreq Char Int
  | TreeNode Int Tree Tree

-- encode :: String -> Encoded
-- encode msg = marshal msg (assembleTree (freq msg))

-- marshal :: String -> Tree -> Encoded


assembleTree :: FrequencyTable -> Tree
assembleTree table =
  let sortedList = sortBy (\(_,a) (_,b) -> compare a b) (toList table)
      acc (char,nextFreq) tree = TreeNode (freq+nextFreq) left right
        where freq = case tree of
                      TreeFreq _ freq->freq
                      TreeNode freq _ _->freq
              newNode=TreeFreq char nextFreq
              (left,right) = if freq<nextFreq
                              then (newNode, tree)
                              else (tree, newNode)
      first = head sortedList
  in foldr acc (uncurry TreeFreq first) (tail sortedList)


printTree :: Tree -> String
printTree (TreeFreq char freq)="("++[char]++" "++show freq++")"
printTree (TreeNode freq left right)="("++show freq++","++printTree left++","++printTree right++")"

freq :: String -> FrequencyTable
freq =
  let acc x = insertWith (+) x 1
   in foldr acc empty
