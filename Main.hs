import qualified Data.Set as Set
import Data.Ord
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map

main :: IO ()
main = do set <- corpus
          putStrLn $ max_subwords_in_corpus set
  
corpus :: IO (Set.Set String)
corpus = fmap (Set.fromList . lines) (readFile "short.txt")

max_subwords_in_corpus :: (Set.Set String) -> String
max_subwords_in_corpus corpus = maximumKeyOfValue $ map_of_subword_lengths
  where 
    num_subwords_in_corpus :: String -> Int
    num_subwords_in_corpus str = Set.size $ Set.intersection (Set.fromList $ List.subsequences str) corpus
    map_of_subword_lengths :: Map.Map String Int
    map_of_subword_lengths = Map.fromSet num_subwords_in_corpus corpus
    
maximumKeyOfValue :: Map.Map String Int -> String
maximumKeyOfValue m = Map.foldlWithKey' compare_things first_key m
  where 
    first_key = unsafeFirstKey m
    compare_things :: String -> String -> Int -> String
    compare_things old_key new_key new_value = if (new_value) >= (unsafeLookup old_key m) then new_key else old_key
    
unsafeFirstKey :: Map.Map k v -> k
unsafeFirstKey m = 
  case Map.keys m of
    [] -> error "No keys"
    (k:_) -> k

unsafeLookup :: Ord k => k -> Map.Map k v -> v
unsafeLookup key m = 
  case Map.lookup key m of
    Nothing -> error "Nothing there to lookup"
    Just v -> v
  
-- foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a