import Data.Map (Map, empty, insertWith)

type Encoded = String

type FrequencyTable = Map Char Int

data Tree
  = TreeFreq Int Char
  | TreeNode Int Tree Tree

-- encode :: String -> Encoded
-- encode msg = marshal msg (assembleTree (freq msg))

-- marshal :: String -> Tree -> Encoded
-- assembleTree :: FrequencyTable -> Tree

freq :: String -> FrequencyTable
freq =
  let acc x = insertWith (+) x 1
   in foldr acc empty
