{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Exit (exitSuccess)
import System.Directory ( doesFileExist, removeFile )
import System.IO
    ( IO,
      getLine,
      putStrLn,
      hClose,
      hFlush,
      openFile,
      hGetContents,
      hPutStr,
      IOMode(ReadMode, WriteMode), hGetLine )
import Control.Exception ()
import System.IO.Error ()
import Prelude hiding (catch)
import Data.List ()
import Control.Applicative ()
import Data.Time.Clock ()
import Data.Char ()
import Control.Monad
import qualified Data.ByteString.Char8 as B

data Animal = Animal {
    nomeAnimal :: String,
    especie :: String,
    peso :: String,
    altura :: String,
    idade :: Int,
    agendamentos :: [Agendamento]
} deriving (Read, Show)

data Cliente = Cliente {
    nomeCliente :: String,
    email :: String,
    senha :: String,
    telefone :: String,
    animais :: [Animal]
} deriving (Read, Show)

data Agendamento = Agendamento {
    date :: String,
    servicos :: [String],
    concluido :: Bool
} deriving (Read, Show)

main :: IO()
main = do
    putStrLn "Boas vindas!"
    putStrLn "Selecione uma das opções abaixo:\n"
    showMenu

showMenu:: IO()
showMenu = do
    putStrLn "1 - Sou Administrador"
    putStrLn "2 - Sou Cliente"

    opcao <- getLine
    menus opcao

menus :: String -> IO()
menus x
    | x == "1" = menuAdm
    | x == "2" = menuCliente
    | otherwise = invalidOption showMenu

invalidOption :: IO() -> IO()
invalidOption f = do
        putStrLn "Selecione uma alternativa válida"
        f

menuAdm :: IO()
menuAdm = do
    putStrLn "\nSelecione uma das opções abaixo:"
    putStrLn "1 - Ver usuários cadastrados no sistema"
    putStrLn "2 - Remover usuários"

    opcao <- getLine
    opcaoAdm opcao

opcaoAdm :: String -> IO()
opcaoAdm x
    | x == "1" = verClientesCadastrados
    | x == "2" = removerCliente
    | otherwise = invalidOption menuAdm


menuCliente :: IO()
menuCliente = do
    putStrLn "\nSelecione uma das opções abaixo:"
    putStrLn "1 - Se cadastrar como cliente"
    putStrLn "2 - Logar no sistema como cliente"

    opcao <- getLine
    opcaoCliente opcao

opcaoCliente:: String -> IO()
opcaoCliente x
    | x == "1" = cadastrarComoCliente2
    | x == "2" = logarComoCliente
    | otherwise = invalidOption menuCliente

segundoMenuCliente :: IO()
segundoMenuCliente = do
    putStrLn "\nSelecione o que deseja como cliente"
    putStrLn "1 - Cadastrar um novo animal"
    putStrLn "x - Retornar para o menu\n"

    opcao <- getLine
    segundaTelaCliente opcao


segundaTelaCliente :: String -> IO()
segundaTelaCliente x
    | x == "1" = cadastraAnimal
    | otherwise = invalidOption menuCliente

indexCliente :: [Cliente] -> String -> Int -> Int
indexCliente (c:cs) email i
    | obterEmail c == email = i
    | obterEmail c /= email = next
    where next = indexCliente cs email (i + 1)

toStringList :: [Cliente] -> String
toStringList (x:xs) = show x ++ "\n" ++ toStringList xs
toStringList [] = ""


cadastraAnimal :: IO()
cadastraAnimal = do
    putStrLn "\nInsira seu email: "
    email <- getLine

    file <- openFile "clientes.txt" ReadMode
    contents <- hGetContents file
    hClose file

    let clientes = lines contents
    let clientesObj = [read x :: Cliente | x <- clientes]
    let index = indexCliente clientesObj email 0

    let cliente = clientesObj !! index

    putStrLn "\nInsira o nome do animal: "
    nome <- getLine
    putStrLn "\nInsira a especie do animal: "
    especie <- getLine
    putStrLn "\nInsira a altura do animal: "
    altura <- getLine
    putStrLn "\nInsira o peso do animal: "
    peso <- getLine
    putStrLn ""

    salvarAnimal (toStringList (cliente:clientesObj))


salvarAnimal :: String -> IO ()
salvarAnimal dados = do
    print(dados)
    -- filex <- openFile "clientes.txt" WriteMode 
    -- hPutStr filex dados
    -- hFlush filex
    -- hClose filex
    showMenu

editCliente :: Cliente -> Animal -> Cliente
editCliente (Cliente n e s t as) a = Cliente {nomeCliente=n,email=e,senha=s,telefone=t, animais=a:as}

verClientesCadastrados :: IO()
verClientesCadastrados = do
    file <- openFile "clientesCadastrados.txt" ReadMode
    contents <- hGetContents file
    print (show contents)

removerCliente:: IO()
removerCliente = do
    putStrLn "\nInsira o email do cliente a ser removido:"
    email <- getLine
    fileExists <- doesFileExist (email ++ ".txt")
    if not fileExists then do
        putStrLn ("\nCliente com email: '" ++ email ++ "' não existe!")
    else do
        removeFile (email ++ ".txt")
        putStrLn ("\nCliente com email: '" ++ email ++ "' removido com sucesso!")
    showMenu


cadastrarComoCliente :: IO()
cadastrarComoCliente = do
    putStrLn "\nInsira seu email:"
    email <- getLine
    fileExists <- doesFileExist (email ++ ".txt")
    if fileExists
        then do
            putStrLn "Usuario ja existente"
            showMenu
        else do
            file <- openFile (email ++ ".txt") WriteMode
            fileClientesCadastrados <- appendFile "clientesCadastrados.txt" email
            fileClientesCadastrados <- appendFile "clientesCadastrados.txt" " "

            putStrLn "\nInsira sua senha:"
            senha <- getLine
            hPutStr file senha
            putStrLn "\nCliente cadastrado com sucesso!"
            hFlush file
            hClose file
            putStrLn ""
            showMenu

obterEmail :: Cliente -> String
obterEmail Cliente {nomeCliente=c, email=e, senha=s, telefone=t, animais=[]} = e

obterSenha :: Cliente -> String
obterSenha (Cliente _ _ senha _ _) = senha


encontraCliente' :: [Cliente] -> String -> String -> Bool
encontraCliente' (c:cs) email ""
    | obterEmail c == email = True
    | obterEmail c /= email = encontrar
    where encontrar = encontraCliente' cs email ""

encontraCliente' [] email senha = False

encontraCliente' (c:cs) email senha
    | obterEmail c == email && obterSenha c == senha = True
    | obterEmail c /= email || obterSenha c /= senha = encontrar
    where encontrar = encontraCliente' cs email senha

cadastrarComoCliente2 :: IO()
cadastrarComoCliente2 = do
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

criarCliente :: String -> String -> String -> String -> IO()
criarCliente nome email senha telefone = do
    let cliente = Cliente {nomeCliente=nome, email=email, senha=senha, telefone=telefone, animais=[]}
    file <- appendFile "clientes.txt" (show cliente ++ "\n")
    putStrLn "\nCliente cadastrado com sucesso!"
    putStrLn ""
    showMenu

logarComoCliente :: IO()
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

            if hasCliente then do
                putStrLn "Login realizado com sucesso"
                segundoMenuCliente

            else do
                putStrLn "Nome ou senha incorretos"
                menuCliente
            hClose file
    else do
        putStrLn "Nenhum cliente não cadastrado. Por favor, cadastre-se"
        cadastrarComoCliente2