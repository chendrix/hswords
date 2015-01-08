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
max_subwords_in_corpus corpus = fst $ List.maximumBy (comparing snd) $ Map.assocs $ map_of_subword_lengths
  where 
    num_subwords_in_corpus :: String -> Int
    num_subwords_in_corpus str = Set.size $ Set.intersection (Set.fromList $ List.subsequences str) corpus
    map_of_subword_lengths :: Map.Map String Int
    map_of_subword_lengths = Map.fromSet num_subwords_in_corpus corpus