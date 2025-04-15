

-- sum i j = i(i+1)

somatorio :: Int -> Int -> Int
somatorio 0 n = div (n*(n+1)) 2

somatorio_intervalo n m = (somatorio 0 m) - (somatorio 0 n)
