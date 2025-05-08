import Data.List 
-- Ex 7
--[1, 2, 3, 4, 5, 6] 2 |-> [[1, 2], [3, 4], [5, 6]] 
divLista :: [a] -> Int -> [[a]]
divLista [] _ = []
divLista lt n = take n lt : divLista (drop n lt) n

inverte :: [a] -> [a]
inverte [] = []
inverte (a:b) = inverte b ++ [a]

-- Ex 8 
ehPalin :: Eq a => [a] -> Bool
ehPalin [] = True
ehPalin a = if a == b then True else False 
  where b = inverte a
