--- Ex1
a = (("Hello", (4, True)), 15.5)
result1 = fst (snd (fst a))

--- Ex2
type Trapezio = (Float, Float, Float)

areaTrapezio :: Trapezio -> Float 
areaTrapezio (t, p, h) = ((t+p)*h)/2

--- Ex 3
type Ponto = (Float, Float)

func :: [Ponto]  -> Ponto
func [] = (0, 0)
func ((x, y):t) = (x + fst sp, y + snd sp) 
  where sp = func t

--- Ex 4 
type Tabuleiro = (Char, Int)
type Jogador = String
type Jogada = (Jogador, Tabuleiro)

fn :: [Jogadas] -> Jogador -> Int

