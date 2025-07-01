

data Data = Dt {dia :: Int, mes :: Int, ano :: Int}

dataStr :: Data -> String 
dataStr (Dt d m a) = (show d)++"/"++(show m)++"/"++(show a)