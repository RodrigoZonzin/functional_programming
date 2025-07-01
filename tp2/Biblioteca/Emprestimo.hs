import Aluno, Data, Livro

data Emprestimo = Ep {numero :: Int, al: Aluno, dtEm :: Data, dtDev :: Data, livros :: [Livro]}