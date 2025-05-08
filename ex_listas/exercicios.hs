import Data.Char
-- Exercicios 1
-- media = (x1+x2+...+xn)/n
media a = sum a / fromIntegral (length a)


-- Exercicios 2
-- [1 2 3 4] ==> [4 3 2 1]
-- [1 2 3 4] ==> [4]: reverter([1 2 3])
-- [1 2 3 4] ==> [4]: [3] : reverter([1 2])
-- [1 2 3 4] ==> [4]: [3] : [2] : reverter([1])
-- [1 2 3 4] ==> [4]: [3] : [2] : [1] : reverter([])
-- [1 2 3 4] ==> [4]: [3] : [2] : [1] : []
-- [1 2 3 4] ==> [4 3 2 1]

reverter [] = []
reverter (a:b) = reverter b ++ [a]

-- Ex 3
-- get_pos [11 12 13 14 15] 2 ==> 13
-- get_pos [11] [12 13 14 15]

get_pos _ [] = -1
get_pos n (a:b) = if n == a then 0 else 1 + get_pos n b

-- Ex 4
-- "RoDrigo" ==> [R, o, D, r, i, g, o]
-- isUpper(R) + isUpper(o) + ... + isUpper(o) 
maiusculas [] = [] 
maiusculas (a:b) = if isUpper a then a : maiusculas b else maiusculas b   

-- Ex 5
-- "oie como estas" ==> ['oie', 'como', 'estas']
-- ==> ['o', 'c', 'e']

separar [] = [] 
separar vec = words vec

iniciais [] = []
iniciais (a:b) = separar a ++ iniciais (separar b)


-- Ex 6 
-- "Rod1a Rask0ln1kov" == > ["Rod1a", "Rask0ln1kov"]
-- "Rod1a" = {x/ for all x, isChar(x)}
-- = isChar() return a else return b

apenas_letras [] = []
apenas_letras (a:b) = if isChar a then a: apenas_letras(b) else apenas_letras b  

-- Ex 7
--[1, 2, 3, 4, 5, 6] 2 |-> [[1, 2], [3, 4], [5, 6]] 
divLista :: [a] -> Int -> [[b]]
divLista [] _ = []
divLista t n = take t n : divLista t (drop t n)




