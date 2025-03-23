----------------------------
-- Huffman em Haskell
-- Alan Gleizer 10416804
-- Caio Corsini 10342005
-- Gilberto de melo 10419275
----------------------------

-- Conta quantas vezes um caracter aparece em uma string
contar :: [Char] -> Char -> Int
contar [] c = 0
contar (a:x) c = if c == a then 1 + contar x c else contar x c

frequenciaJaVistos :: [Char] -> [Char] -> [(Char, Int)]
frequenciaJaVistos [] _ = []
frequenciaJaVistos (x:xs) jaVistos 
  | x `elem` jaVistos = frequenciaJaVistos xs jaVistos
  | otherwise = (x, contar (x:xs) x): frequenciaJaVistos xs (x : jaVistos)

-- Monta a lista de contagem de caracteres de uma string
frequencia :: [Char] -> [(Char,Int)]
frequencia palavra = frequenciaJaVistos palavra [] 

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

-- Ordena as tuplas (char, frequencia) em ordem decrescente de frequencia.
-- O critério de desempate fica por sua conta
-- uso da implementação de quicksort vista em aula
ordenar :: [(Char, Int)] -> [(Char, Int)]
ordenar [] = []
ordenar (a:lista) = (ordenar (particaoMaiores lista a)) ++ [a] ++ (ordenar (particaoMenores lista a))
  where
    particaoMenores lista (simbolo, pivo) = [ (simbolo, freq) | (simbolo, freq) <- lista, freq < pivo ]
    particaoMaiores lista (simbolo, pivo) = [ (simbolo, freq) | (simbolo, freq) <- lista, freq >= pivo ]

-- dada uma tupla (char, frequencia) devolver uma tupla (char, codigo_binario)
gerarCodigos :: [(Char, Int)] -> [(Char, String)]
gerarCodigos [] = []
gerarCodigos lista = combinar lista tabelaCodigos
  where 
    combinar [] _ = []  -- casos bas: se qualquer lista estiver vazia
    combinar _ [] = []
    combinar ((simbolo, freq):restoLista) (codigo:restoCodigos) = (simbolo, codigo) : combinar restoLista restoCodigos
    --        ^ notação head:resto                                 ^ combinar elems   ^ chamada recursiva    
    tabelaCodigos = [ "0", "10", "110", "111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111", "10000", "10001", "10010", "10011", "10100", "10101", "10110", "10111", "11000", "11001", "11010", "11011", "11100", "11101", "11110", "11111", "100000", "100001", "100010", "100011", "100100", "100101", "100110", "100111", "101000", "101001", "101010", "101011", "101100", "101101", "101110", "101111", "110000", "110001", "110010", "110011", "110100", "110101", "110110", "110111", "111000", "111001", "111010", "111011", "111100", "111101", "111110", "111111", "1000000", "1000001", "1000010", "1000011", "1000100", "1000101", "1000110", "1000111", "1001000", "1001001", "1001010", "1001011", "1001100", "1001101", "1001110", "1001111", "1010000", "1010001", "1010010", "1010011", "1010100", "1010101", "1010110", "1010111", "1011000", "1011001", "1011010", "1011011", "1011100", "1011101", "1011110", "1011111", "1100000", "1100001", "1100010", "1100011", "1100100", "1100101", "1100110", "1100111"]

-- codificar
codificar :: String -> [(Char, String)] -> String
codificar [] _ = ""
codificar (x:xs) tabela = buscarCodigo x tabela ++ codificar xs tabela
  where
    buscarCodigo _ [] = "" -- se nao houver o caractere na tabela de codigos
    buscarCodigo c ((ch, codigo):resto)
      | c == ch = codigo
      | otherwise = buscarCodigo c resto

-- huffman
-- Executa o codigo de huffman, com o auxilio de diversas funcoes
huffman :: String -> String
huffman entrada = codificar entrada tabelaCodigos
  where
    listaFrequencia = removerDup(inverterLista(frequencia entrada))
    listaOrdenada = ordenar listaFrequencia
    tabelaCodigos = gerarCodigos listaOrdenada
    
-- Input do usuario
main :: IO()
main = do
    putStrLn "Digitar palavra: "
    palavra <- getLine
    print (huffman palavra)

-- Funcoes implementadas
-- Caio: frequencia removerDup inverterLista contar
-- Alan: ordenar gerarCodigos
-- Gilberto: frequenciaJaVistos huffman codificar