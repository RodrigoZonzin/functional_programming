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

-- Criei esse operador para comparar elementos do tipo Produto 
-- como Produto = (Nome, Valor), p1 e p2 in Produto <==> p1.nome == p2.nome e p1,valor == p2.valor
(==.) :: Produto -> Produto -> Bool
(==.) (n1, v1) (n2, v2) = if (n1 == n2) && (v2 == v2) then True else False

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

--elemento :: [a] -> Int -> a
--elemento [] _ = Nothing
--elemento (h:t) idx 
--  | idx < 0 = Nothing
--  | idx == 0 = Just h 
--  | otherwise = elemento t (idx-1)

-- Exemplo de funcionamento: 
-- elemento [1, 20, 41, 34, 56] 2
-- idx < 0 False 
-- idx == 0 False 
--    elemento [20, 41, 34, 56] 1
--    idx < 0 False
--    idx == 0 False
--       elemento [41, 34, 56] 0
--        idx < 0 False
--        idx == 0 True = 41


--Crie a função addProduto que recebe uma lista de Produto e um Produto e
-- retorna a lista com o produto adicionado no final. Crie a função remProduto que recebe uma lista
-- de Produto e um Nome e retorna a lista sem o produto com nome passado. Crie a função
-- buscaProduto que recebe uma lista de Produto e um Nome e retorna um Maybe Produto, que será
-- o produto da lista possui aquele nome ou Nothing (ver tipo Maybe).

addProduto :: [Produto] -> Produto -> [Produto]
addProduto [] p = p:[]
addProduto (h:t) p = h : addProduto t p

-- addProduto [Carro, Placa, Lapis, Arroz] Mesa |-> 
-- Carro [Placa, Lapis, Arroz] Mesa
-- t == [] False addProduto t Mesa 
  -- Placa [Lapis, Arroz] Mesa 
  -- t == [] False addProduto t Mesa 
    -- Lapis [Arroz] Mesa
    -- t == [] False addProduto t Mesa
      --Arroz [] Mesa
      -- t == [] True Mesa:[]

      -- e daí volta concatenando com as anteriores 


remProduto :: [Produto] -> Produto -> [Produto]
remProduto [] _ = []
remProduto (h:t) prod 
  | h ==. prod = remProduto t prod
  | otherwise = h: remProduto t prod

-- remProduto [Carro, Placa, Lapis, Arroz] Placa |-> 
-- h : remProduto [Placa, Lapis, Arroz]
-- if h == prod remProduto t prod

buscaProduto :: [Produto] -> Produto -> Maybe Produto
buscaProduto [] _ = Nothing
buscaProduto (h:t) prod
  | h ==. prod = Just prod
  | otherwise  = buscaProduto t prod


--
alinhaEsq :: String -> Char -> Int -> String
alinhaEsq [] _ _ = []
alinhaEsq _ _ 0  = []

alinhaEsq str c n = str ++ alinhaEsq_aux c n 
  where 
    alinhaEsq_aux :: Char -> Int -> String
    alinhaEsq_aux _ 0 = []
    alinhaEsq_aux c n = c : alinhaEsq_aux c (n-1) 

--alinhaEsq "Rodrigo" '.' 4
--"Rodrigo" ++ alinhaEsq_aux '.' 4
--"Rodrigo" ++ . ++ alinhaEsq_aux '.' 3
--"Rodrigo" ++ . ++ . ++ alinhaEsq_aux '.' 2
--"Rodrigo" ++ . ++ . ++ . ++ alinhaEsq_aux '.' 1
--"Rodrigo" ++ . ++ . ++ . ++ . ++ alinhaEsq_aux '.' 0
--"Rodrigo" ++ . ++ . ++ . ++ . ++ ''
--"Rodrigo" ++ . ++ . ++ . ++ .
--"Rodrigo" ++ . ++ . ++ ..
--"Rodrigo" ++ . ++ ...
--"Rodrigo" ++ ....
--"Rodrigo"....

alinhaDir :: String -> Char -> Int -> String
alinhaDir [] _ _  = []
alinhaDir _ _ 0   = []
alinhaDir str ch n =  (alinhaDIrAux ch n) ++ str 
  where 
    alinhaDIrAux :: Char -> Int -> String
    alinhaDIrAux _ 0 = []
    alinhaDIrAux ch n = ch : alinhaDIrAux ch (n-1)


--"####...##" ++ "Rodrigo"

infix 5 $$
($$) :: Valor -> Int -> String
val $$ n =
  let (intPart, fracPart) = properFraction val         -- separa parte inteira e decimal
      fracStr = take n $ (drop 2 . show) (val - fromIntegral intPart + 1)  -- pega parte decimal como string
      paddedFrac = take n (fracStr ++ repeat '0')      -- completa com zeros se necessário
  in show intPart ++ if n > 0 then "." ++ paddedFrac else ""

dinheiro :: Valor -> String
dinheiro val = '$' : (val $$ 2)



formataItem :: Item -> String
formataItem ((n, v), q) =
  alinhaEsq n '.' (45 - length n)
  ++ dinheiro v ++ " x "
  ++ show q ++ " = "
  ++ dinheiro (v * fromIntegral q)
