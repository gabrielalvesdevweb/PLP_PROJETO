import Data.Time
import System.Exit

main :: IO()
main = do
  menu
    
menu :: IO()
menu = do
  putStrLn("Menu:\n\n" ++ "1. Selecionar Dificuldade\n" ++ "2. Sair\n")
  dados <- getLine
  let opcao = read(dados)
  if opcao == 1 then do
    putStrLn("\nSelecione a Dificuldade:\n\n1. Fácil\n" ++ "2. Médio\n" ++ "3. Difícil\n")
    escolha <- getLine
    let dificuldade = read(escolha)
    if dificuldade == 1 then facil
    else if dificuldade == 2 then medio
    else if dificuldade == 3 then dificil
    else menu
  else if opcao == 2 then exitSuccess
  else menu

facil :: IO()
facil = do
  putStrLn "Dificuldade: Fácil\n"
  dados <- readFile "facil.txt"
  putStrLn dados
  let texto = separaPalavras (== ' ') dados
  x <- getLine
  let textoArray = separaPalavras (== ' ') x
  if(ehigual texto textoArray) then do
    putStrLn "Parabéns, você passou para o próximo nível!\n"
    medio
  else do
    putStrLn "Erro de digitação. Tente novamente!\n"
    facil

medio :: IO()
medio = do
  putStrLn "Dificuldade: Média\n"
  dados <- readFile "medio.txt"
  putStrLn dados
  let texto = separaPalavras (== ' ') dados
  x <- getLine
  let textoArray = separaPalavras (== ' ') x
  if(ehigual texto textoArray) then do
    putStrLn "Parabéns, você passou para o próximo nível!\n"
    dificil
  else do
    putStrLn "Erro de digitação. Tente novamente!\n"
    medio

dificil :: IO()
dificil = do
  putStrLn "Dificuldade: Difícil\n"
  dados <- readFile "dificil.txt"
  putStrLn dados
  let texto = separaPalavras (== ' ') dados
  x <- getLine
  let textoArray = separaPalavras (== ' ') x
  if(ehigual texto textoArray) then do
    putStrLn "Parabéns, você concluiu com êxito o teste de datilografia!\n"
    testeNovamente
  else do
    putStrLn "Erro de digitação. Tente novamente!\n"
    dificil

testeNovamente :: IO()
testeNovamente = do
  putStrLn "Quer realizar o teste novamente no nível difícil?\n\n[S]im\n[N]ão\n"
  resposta <- getLine
  if(resposta == "S") then dificil
  else if(resposta == "N") then menu
  else testeNovamente


separaPalavras :: (Char -> Bool) -> String -> [String]
separaPalavras p s = case dropWhile p s of
  "" -> []
  s' -> w : separaPalavras p s''
    where
      (w, s'') = break p s'

ehigual :: [String] -> [String] -> Bool
ehigual [] [] = True
ehigual [] _ = False
ehigual _ [] = False
ehigual (a : as) (b : bs)
  | a == b = ehigual as bs
  | otherwise = False