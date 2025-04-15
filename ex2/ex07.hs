
somatorio :: Int -> Int -> Int
somatorio 0 n = div (n*(n-1)) 2 

media_inter :: Int -> Int -> Double
media_inter n m =  (somatorio 0 m - somatorio 0 n)/(m-n)
