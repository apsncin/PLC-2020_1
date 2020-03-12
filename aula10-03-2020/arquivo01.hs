vendas 0 = 4
vendas 1 = 8
vendas 2 = 10
vendas 3 = 12
vendas 4 = 8

totalVendas :: Int -> Int
totalVendas n
 | n == 0 = vendas 0
 | otherwise = (vendas n) + (totalVendas (n-1))

totalVendasCasPadrao :: Int -> Int
totalVendasCasPadrao 0 = vendas 0
totalVendasCasPadrao n = vendas n + totalVendasCasPadrao (n-1)

-- Definições locais: let <definicções> in <expressão>
-- <definições> where <definições>

--where é como uma função local

somaQuadrado :: Int -> Int -> Int
somaQuadrado x y = quadX + quadY
    where
        quadX = x * x
        quadY = y * y

somaQuadrado2 :: Int -> Int -> Int
somaQuadrado2 x y = quadP x + quadP y
    where quadP n = n * n

--Usando let in no terminal -> let in é uma expressão
--Exemplo:
--let x = 3 in x + 5
--(let x = 3 in x + 5) + 12

somaQuadLetIn :: Int -> Int -> Int
somaQuadLetIn x y = let sqX = x * x
                        sqY = y * y
                    in  sqX + sqY


vendasS :: Int -> Int -> Int
vendasS n 0
 | vendas 0 == n = 1
 | otherwise = 0
vendasS n s
 | vendas s == n = 1 + vendasS n (s-1)
 | otherwise = vendasS n (s-1)

offset = fromEnum 'a' - fromEnum 'A'

maiuscula :: Char -> Char
maiuscula ch = toEnum (fromEnum ch - offset)

ehDigito :: Char -> Bool
ehDigito ch = ('0' <= ch) && (ch <= '9')



--show -> Transforma algo em uma String
--Exemplo:
-- show 3

--read -> Transforma uma String em algo
--Exemplo:
-- read "3" :: Int
-- (read "3" :: Int) + 4

--funções disponíveis para pontos flutuantes:
-- floor
-- celling
-- round

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita 0 s = s
paraDireita n s = addEspacos n ++ s

mediaVendas :: Int -> Int
mediaVendas n = div (totalVendas n) (n+1)

imprimeSemanas :: Int -> Int -> String
imprimeSemanas n m
 | n == m = show n ++ addEspacos 10 ++ show (vendas n) ++ "\n"
 | otherwise = show m ++ addEspacos 10 ++ show (vendas m) ++ "\n" ++ imprimeSemanas n (m + 1)


tabelaVendas :: Int -> String
tabelaVendas n = "Semana" ++ addEspacos 3 ++ "Vendas" ++ "\n"
                 ++ imprimeSemanas n 0
                 ++ "Total " ++ addEspacos 5 ++ show (totalVendas n) ++ "\n"
                 ++ "Media " ++ addEspacos 5 ++ show (mediaVendas n) ++ "\n"

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr (tabelaVendas n)

            