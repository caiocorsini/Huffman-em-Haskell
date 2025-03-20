-- Conta quantas vezes um caracter aparece em uma string
contar :: [Char] -> Char -> Int
contar [] c = 0
contar (a:x) c = if c == a then 1 + contar x c else contar x c

-- Monta a lista de contagem de caracteres de uma string
frequencia :: [Char] -> [(Char,Int)]
frequencia [] = []
frequencia (a:x) = (a,contar (a:x) a) : frequencia x

-- Remove tuplas duplicatas. Somente o da esquerda
-- elem verifica se "a" se encontra repetido em alguma lista
-- map aplica uma operacao (neste caso elem) em todos os elementos de uma lista
-- fst cria uma lista so com caracteres. Se baseia nela para remover repetidos.
removerDup :: [(Char,Int)] -> [(Char,Int)]
removerDup [] = []
removerDup (a:x) = if fst a `elem` map fst x then removerDup x else a : removerDup x
    
-- RemoverDup so remove o da esquerda, o qual eh o que deve ser mantido, logo eh
-- necessario inverter a lista antes de usar o removerDup
inverterLista :: [(Char,Int)] -> [(Char,Int)]
inverterLista [] = []
inverterLista (a:x) = (inverterLista x) ++ [a]

-- INICIO FUNÇÕES ALAN

{-
Funções prontas uttilizadas:
fst e snd: retornam o primeiro e segundo elementos de uma tupla

particaoMenores lista num = [ (simbolo, codigo) | (simbolo, codigo) <- lista, codigo <= num ]
particaoMaiores lista num = [ (simbolo, codigo) | (simbolo, codigo) <- lista, codigo > num ]
quicksort [] = []
quicksort (p:lista)=(quicksort (particao1 p lista)) ++ [p] ++ (quicksort (particao2 p lista))


-}

-- ordena as tuplas (char, frequencia) em ordem
-- decrescente de frequencia. O critério de
-- desempate fica por sua conta
ordenar :: [(Char, Int)] -> [(Char, Int)]
ordenar [] = []
ordenar (a:lista) = (ordenar (particaoMaiores lista a)) ++ [a] ++ (ordenar (particaoMenores lista a))
  where
    particaoMenores lista (simbolo, pivo) = [ (simbolo, codigo) | (simbolo, codigo) <- lista, codigo < pivo ]
    particaoMaiores lista (simbolo, pivo) = [ (simbolo, codigo) | (simbolo, codigo) <- lista, codigo >= pivo ]

-- dada uma tupla (char, frequencia) devolver uma
-- tupla (char, codigo_binario)
-- gerarCodigos :: [(Char, Int)] -> [(Char, String)] 



-- FIM FUNÇÕES ALAN


main :: IO()
main = do
    {-
    putStrLn "Digite uma palavra qualquer: "
    palavra <- getLine
    print (removerDup (inverterLista (frequencia palavra)))
    -}

    print(ordenar [('a',5),('b',2),('r',2),('c',1),('d',1)])
    
    
    