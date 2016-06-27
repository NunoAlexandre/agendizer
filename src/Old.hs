-- 41943 - Nuno Miguel Bento Alexandre | TP13

import System.Environment
import Data.List
import System.IO


main = do
   -- taga e le os ficheiros indicados em argumento pela linha de comandos...
	(fin1:fin2:fout:_) <- getArgs
	entradasA1 <- readFile fin1
	entradasA2 <- readFile fin2
   -- formata as entradas...
	let agenda1 = paraAgenda entradasA1
	let nomesA1 = map fst agenda1
	let agenda2  = paraAgenda entradasA2
	let nomesA2 = map fst agenda2
	-- funde as agendas atribuindo os numeros aos contatos...
	let agendaNaoRepetidos = atribuiONumero (naoRepetidos nomesA1  nomesA2) agenda1 agenda2
	let agendaRepetidos = defineUmNumero (repetidos nomesA1 nomesA2) agenda1 agenda2
	let agendaFinal = sort (agendaRepetidos ++ agendaNaoRepetidos)
	-- Por fim escreve a agenda final no ficheiro fout
	writeFile fout (mostraAgenda agendaFinal)


mostraAgenda :: Show a => [([Char], a)] -> String
mostraAgenda agenda = unlines $ map (\(nome, numero) -> nome ++ " " ++ (show numero)) agenda

escolhe :: Integral a => a -> a -> a
escolhe t1 t2
		| t1 `div` 100000000 == 2 	= t1
		| t2 `div` 100000000 == 2 	= t2
		| otherwise					   = t1


atribuiONumero :: Eq a => [a] -> [(a, t)] -> [(a, t)] -> [(a, t)]
atribuiONumero ws xs ys = map (obtemONumero xs ys) ws

obtemONumero :: Eq a => [(a, t)] -> [(a, t)] -> a -> (a, t)
obtemONumero xs ys nome
						| elem nome (map fst xs) = (nome, numeroDe nome xs)
						| otherwise = (nome, numeroDe nome ys)


defineUmNumero :: (Eq a, Integral t) => [a] -> [(a, t)] -> [(a, t)] -> [(a, t)]
defineUmNumero ws xs ys = map (defineNumero xs ys) ws

defineNumero :: (Eq a, Integral t) => [(a, t)] -> [(a, t)] -> a -> (a, t)
defineNumero xs ys nome = (nome, escolhe (numeroDe nome xs) (numeroDe nome ys))

repetidos :: Eq a => [a] -> [a] -> [a]
repetidos xs ys = nub $ filter (\nome -> nome `elem` ys) xs

naoRepetidos :: Eq a => [a] -> [a] -> [a]
naoRepetidos xs ys = nub $ (filter (\nome -> not (elem nome ys)) xs) ++ (filter (\nome -> not (elem nome xs))ys)

numeroDe :: Eq a => a -> [(a, b)] -> b
numeroDe name xs = snd $ head (filter (\(nome, numb) -> nome == name) xs)

paraAgenda :: String -> [(String, Int)]
paraAgenda entradas = map (\linha -> ( obterNome linha, obterNumero linha)) $ lines entradas

obterNome :: String -> String
obterNome linha = unwords $ init $ words linha

obterNumero :: String -> Int
obterNumero linha = read $ last $ words linha :: Int -- MELHOR