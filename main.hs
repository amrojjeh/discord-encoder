import Control.Monad
import Control.Monad.RWS.Lazy (MonadState (put))
import Data.List (foldl', sortBy)
import Data.Map (Map, empty, insert, insertWith, lookup, toList, union)
import System.Environment
import System.IO
import Prelude hiding (lookup)

data Encoded = Encode EncodingTable String

type FrequencyTable = Map Char Int

type EncodingTable = Map Char String

data Tree
  = TreeFreq Char Int
  | TreeNode Int Tree Tree

main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- readFile file
      putStr $ show $ encode x

encode :: String -> Encoded
encode msg = marshal msg (encodeTable $ assembleTree $ freq msg)

instance Show Encoded where
  show (Encode table str) = show table ++ "\n----\n" ++ str

marshal :: String -> EncodingTable -> Encoded
marshal msg table =
  let must (Just a) = a
      must Nothing = error "unexpected"
      encoded = msg >>= (\char -> must $ lookup char table)
   in Encode table encoded

encodeTable :: Tree -> EncodingTable
encodeTable tree =
  let helper (TreeFreq letter _) bits map = insert letter bits map
      helper (TreeNode _ left right) bits map =
        union
          (helper left (bits ++ "0") map)
          (helper right (bits ++ "1") map)
   in helper tree "" empty

assembleTree :: FrequencyTable -> Tree
assembleTree table =
  let sortedList = sortBy (\(_, a) (_, b) -> compare a b) (toList table)
      acc tree (char, nextFreq) = TreeNode (freq + nextFreq) left right
        where
          freq = case tree of
            TreeFreq _ freq -> freq
            TreeNode freq _ _ -> freq
          newNode = TreeFreq char nextFreq
          (left, right) =
            if freq < nextFreq
              then (newNode, tree)
              else (tree, newNode)
      first = head sortedList
   in foldl' acc (uncurry TreeFreq first) (tail sortedList)

printTree :: Tree -> String
printTree (TreeFreq char freq) = "(" ++ [char] ++ " " ++ show freq ++ ")"
printTree (TreeNode freq left right) = "(" ++ show freq ++ "," ++ printTree left ++ "," ++ printTree right ++ ")"

freq :: String -> FrequencyTable
freq =
  let acc x = insertWith (+) x 1
   in foldr acc empty
