

pot :: Int -> Int -> Int
pot _ 0 = 1
pot k m = k*pot k m-1 
