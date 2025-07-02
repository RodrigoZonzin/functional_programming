module Dados where 
    import Util
    import Data.List (sort)

    class Dado t where 
        toString :: t -> String

    data Set a = Set {valores :: [a]} 
        deriving (Eq, Show)

    vazioSet :: Set a -> Bool
    vazioSet (Set vs) =  if (length vs) <= 0 then True else False


    -- funcao jaEsta movida para Util
    -- estou mantendo ordenado, como na matematica
    inserirSet :: (Eq a, Ord a) => a -> Set a -> Set a
    inserirSet x (Set vals) 
        | jaEsta x vals = Set vals
        | otherwise = Set (sort (x : vals))

    removerSet :: (Eq a, Ord a) => a -> Set a -> Set a
    removerSet _ (Set [])   = Set []
    --removerSet x (Set vals) = 

