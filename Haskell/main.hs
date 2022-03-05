{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative ()
import Control.Exception ()
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char ()
import Data.List ()
import Data.Time.Clock ()
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import System.IO
  ( IO,
    IOMode (ReadMode, ReadWriteMode, WriteMode),
    getLine,
    hClose,
    hFlush,
    hGetContents,
    hGetLine,
    hPutStr,
    openFile,
    putStrLn,
  )
import System.IO.Error ()
import Prelude hiding (catch)
import Distribution.PackageDescription (CondTree(condTreeComponents))

data Animal = Animal
  { nomeAnimal :: String,
    emailCliente :: String,
    especie :: String,
    peso :: String,
    altura :: String,
    idade :: String
  }
  deriving (Read, Show)

data Cliente = Cliente
  { nomeCliente :: String,
    email :: String,
    senha :: String,
    telefone :: String
  }
  deriving (Read, Show)

data Agendamento = Agendamento
  { date :: String,
    servicos :: [String],
    concluido :: Bool,
    animal :: String
  }
  deriving (Read, Show)

obterCliente :: Cliente -> String -> String
obterCliente Cliente {nomeCliente=n, email=e, senha=s, telefone=t} prop
    | prop == "nomeCliente" = n
    | prop == "email" = e
    | prop == "senha" = s
    | prop == "telefone" = t

editCliente :: Cliente -> Animal -> Cliente
editCliente Cliente {nomeCliente = n, email = e, senha = s, telefone = t} a = Cliente {nomeCliente = n, email = e, senha = s, telefone = t}

obterAnimal :: Animal -> String -> String
obterAnimal Animal {nomeAnimal=n, emailCliente=ec, especie=e, peso=p, altura=a, idade=i} prop
    | prop == "nomeAnimal" = n
    | prop == "emailCliente" = ec
    | prop == "especie" = e
    | prop == "peso" = p
    | prop == "altura" = a
    | prop == "idade" = i

main :: IO ()
main = do
  putStrLn "Boas vindas!"
  putStrLn "Selecione uma das opções abaixo:\n"
  showMenu

showMenu :: IO ()
showMenu = do
  putStrLn "1 - Sou Administrador"
  putStrLn "2 - Sou Cliente"

  opcao <- getLine
  menus opcao

menus :: String -> IO ()
menus x
  | x == "1" = menuAdm
  | x == "2" = menuCliente
  | otherwise = invalidOption showMenu

invalidOption :: IO () -> IO ()
invalidOption f = do
  putStrLn "Selecione uma alternativa válida"
  f

menuAdm :: IO ()
menuAdm = do
  putStrLn "\nSelecione uma das opções abaixo:"
  putStrLn "1 - Ver usuários cadastrados no sistema"
  putStrLn "2 - Remover usuários"

  opcao <- getLine
  opcaoAdm opcao

opcaoAdm :: String -> IO ()
opcaoAdm x
  | x == "1" = verClientesCadastrados
  | x == "2" = removerCliente
  | otherwise = invalidOption menuAdm

menuCliente :: IO ()
menuCliente = do
  putStrLn "\nSelecione uma das opções abaixo:"
  putStrLn "1 - Se cadastrar como cliente"
  putStrLn "2 - Logar no sistema como cliente"

  opcao <- getLine
  opcaoCliente opcao

opcaoCliente :: String -> IO ()
opcaoCliente x
  | x == "1" = cadastrarComoCliente
  | x == "2" = logarComoCliente
  | otherwise = invalidOption menuCliente

segundoMenuCliente :: String -> IO ()
segundoMenuCliente email = do
  putStrLn "\nSelecione o que deseja como cliente"
  putStrLn "1 - Cadastrar um novo animal"
  putStrLn "2 - Listar animais cadastrados"
  putStrLn "x - Retornar para o menu\n"

  opcao <- getLine
  segundaTelaCliente opcao email

segundaTelaCliente :: String -> String -> IO ()
segundaTelaCliente x email
  | x == "1" = cadastraAnimal email
  | x == "2" = listarAnimais email
  | otherwise = invalidOption menuCliente

indexCliente :: [Cliente] -> String -> Int -> Int
indexCliente (c : cs) email i
  | obterCliente c "email" == email = i
  | obterCliente c "email" /= email = next
  where
    next = indexCliente cs email (i + 1)

converterEmAnimal a = read a :: Animal

listarAnimais :: String -> IO()
listarAnimais emailCliente = do
    file <- openFile "animais.txt" ReadMode
    contents <- hGetContents file

    let animaisStr = lines contents
    let animais = map converterEmAnimal animaisStr

    mostrarAnimaisDoCliente emailCliente animais
    showMenu

mostrarAnimaisDoCliente :: String -> [Animal] -> IO()
mostrarAnimaisDoCliente emailCliente [] = do
    putStrLn ""
mostrarAnimaisDoCliente emailCliente (a:as) = do
    if obterAnimal a "emailCliente" /= emailCliente
        then do
            mostrarAnimaisDoCliente emailCliente as
    else do
        putStrLn ("Nome: " ++ obterAnimal a "nomeAnimal")
        putStrLn ("Especie: " ++ obterAnimal a "especie")
        putStrLn ("Peso: " ++ obterAnimal a "peso")
        putStrLn ("Altura: " ++ obterAnimal a "altura")
        putStrLn ("Idade: " ++ obterAnimal a "idade" ++ "\n")
        mostrarAnimaisDoCliente emailCliente as


toStringListCliente :: [Cliente] -> String
toStringListCliente (x : xs) = show x ++ "\n" ++ toStringListCliente xs
toStringListCliente [] = ""

toCliente c = read c :: Cliente

toObjListCliente :: [String] -> [Cliente]
toObjListCliente = map toCliente

cadastraAnimal :: String -> IO ()
cadastraAnimal email = do
  putStrLn "\nInsira o nome do animal: "
  nome <- getLine
  putStrLn "\nInsira a especie do animal: "
  especie <- getLine
  putStrLn "\nInsira a altura do animal: "
  altura <- getLine
  putStrLn "\nInsira o peso do animal: "
  peso <- getLine
  putStrLn "\nInsira o idade do animal: "
  idade <- getLine
  putStrLn ""

  let animal = Animal {nomeAnimal = nome, emailCliente = email, peso = peso, altura = altura, especie = especie, idade = idade}

  appendFile "animais.txt" (show animal ++ "\n")

  putStrLn "\nAnimal Cadastrado com sucessos!\n"
  showMenu

verClientesCadastrados :: IO ()
verClientesCadastrados = do
  file <- openFile "clientesCadastrados.txt" ReadMode
  contents <- hGetContents file
  print (show contents)

removerCliente :: IO ()
removerCliente = do
  putStrLn "\nInsira o email do cliente a ser removido:"
  email <- getLine
  fileExists <- doesFileExist (email ++ ".txt")
  if not fileExists
    then do
      putStrLn ("\nCliente com email: '" ++ email ++ "' não existe!")
    else do
      removeFile (email ++ ".txt")
      putStrLn ("\nCliente com email: '" ++ email ++ "' removido com sucesso!")
  showMenu


encontraCliente' :: [Cliente] -> String -> String -> Bool
encontraCliente' [] email senha = False
-- Procura Cliente somente verificando o email
encontraCliente' (c : cs) email ""
  | obterCliente c "email" == email = True
  | obterCliente c "email" /= email = encontrar
  where
    encontrar = encontraCliente' cs email ""
-- Procura Cliente verificando o email e a senha
encontraCliente' (c : cs) email senha
  | obterCliente c "email" == email && obterCliente c "senha" == senha = True
  | obterCliente c "email" /= email || obterCliente c "senha" /= senha = encontrar
  where
    encontrar = encontraCliente' cs email senha

cadastrarComoCliente :: IO ()
cadastrarComoCliente = do
  putStrLn "\nInsira seu nome:"
  nome <- getLine

  putStrLn "Insira seu email:"
  email <- getLine

  putStrLn "Insira sua senha:"
  senha <- getLine

  putStrLn "Insira seu telefone:"
  telefone <- getLine

  putStrLn ""

  fileExists <- doesFileExist "clientes.txt"
  if fileExists
    then do
      file <- openFile "clientes.txt" ReadMode
      contents <- hGetContents file
      let clientes = lines contents
      let hasThisClient = encontraCliente' ([read x :: Cliente | x <- clientes]) email ""

      if hasThisClient
        then do
          putStrLn "Usuario ja existente"
          showMenu
        else do
          criarCliente nome email senha telefone
    else do
      criarCliente nome email senha telefone

criarCliente :: String -> String -> String -> String -> IO ()
criarCliente nome email senha telefone = do
  let cliente = Cliente {nomeCliente = nome, email = email, senha = senha, telefone = telefone}
  file <- appendFile "clientes.txt" (show cliente ++ "\n")
  putStrLn "\nCliente cadastrado com sucesso!"
  putStrLn ""
  showMenu

logarComoCliente :: IO ()
logarComoCliente = do
  putStrLn "Insira seu email"
  email <- getLine
  fileExists <- doesFileExist "clientes.txt"

  if fileExists
    then do
      putStrLn "Insira sua senha"
      senha <- getLine
      file <- openFile "clientes.txt" ReadMode
      contents <- hGetContents file
      let clientes = lines contents
      let hasCliente = encontraCliente' [read x :: Cliente | x <- clientes] email senha

      if hasCliente
        then do
          putStrLn "Login realizado com sucesso"
          segundoMenuCliente email
        else do
          putStrLn "Nome ou senha incorretos"
          menuCliente
      hClose file
    else do
      putStrLn "Nenhum cliente não cadastrado. Por favor, cadastre-se"
      cadastrarComoCliente