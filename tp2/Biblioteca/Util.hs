module Util where 
  data Data = Dt {dia :: Int, mes :: Int, ano :: Int}

  dataStr :: Data -> String 
  dataStr (Dt d m a) = (show d)++"/"++(show m)++"/"++(show a)


  formata :: String -> String -> String
  formata chave valor = chave ++ repete '.' (30 - length chave-1) ++ ":" ++ repete ' ' (50 - length valor) ++ valor
    where
      repete :: Char -> Int -> String
      repete ch 0 = ""
      repete ch n = ch : repete ch (n - 1)

  -- func auxiliar para a funcao inserirSet
  jaEsta :: Eq a => a -> [a] -> Bool
  jaEsta _ []      = False
  jaEsta el (h:t)  = if el == h then True else jaEsta el t