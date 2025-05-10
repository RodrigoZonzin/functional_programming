import Data.List 

-- Crie os tipos: Nome, equivalente a String; Valor, equivalente a Float;
-- Quantidade, equivalente a Int; Produto, com nome e valor; Item, 
-- com produto e quantidade. Crieuma função chamada produtos que retorna
-- uma lista com 10 produtos a sua escolha.

type Nome = String
type Valor = Float
type Quantidade = Int 
type Produto = (Nome, Valor)
type Item = (Produto, Quantidade)


produtos :: [Produto]
produtos = [(("Punto 2018", 52000.7)), 
            (("Lapis", 0.98)), 
            (("Beretta 9mm", 9000.00)), 
            (("RTX 5090", 25000.00)), 
            (("Miojo", 1.20)),
            (("Lutas e Metamorfoses de uma Mulher - Edouard Louis", 45.00)), 
            (("Estadio do Athletic", (10^6))), 
            (("Almoco no RU", 2.75)), 
            (("Battlefield 2042", 249.00)), 
            (("Passagem Ida - Paris", 5478.25))]

-- Crie as seguintes funções auxiliares: repete, que recebe um elemento de qualquer tipo e um inteiro e retorna uma lista 
-- com o elemento repetido o número de vezes informado; index, que recebe um elemento de um subtipo de Eq e uma lista do 
-- mesmo tipo e retorna o índice do elemento na lista se ele existir ou Nothing, caso o elemento não exista na lista;
-- e elemento que recebe uma lista de qualquer tipo e um índice (Int) e retorna o elemento no índice informado ou Nothing, 
-- para um índice inválido. Para essa questão pesquise o tipo Maybe do Haskell.
repete :: a -> Int -> [a]
--repete [] _ = []
repete _ 0 = []
repete a n = a : repete a (n-1)


index :: Eq a => a -> [a] -> Maybe Int
index x = indexAux 0
  where
    indexAux _ [] = Nothing
    indexAux i (y:ys)
      | x == y = Just i
      | otherwise = indexAux (i + 1) ys