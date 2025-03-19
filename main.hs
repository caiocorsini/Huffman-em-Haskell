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

main :: IO()
main = do
    putStrLn "Digite uma palavra qualquer: "
    palavra <- getLine
    print (removerDup (inverterLista (frequencia palavra)))
    
    
    
    