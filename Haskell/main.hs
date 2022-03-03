import System.Exit (exitSuccess)
import System.Directory ( doesFileExist )
import System.IO
    ( IO,
      getLine,
      putStrLn,
      hClose,
      hFlush,
      openFile,
      hGetContents,
      hPutStr,
      IOMode(ReadMode, WriteMode) )
import Control.Exception ()
import System.IO.Error ()
import Prelude hiding (catch)
import Data.List ()
import Control.Applicative ()
import Data.Time.Clock ()
import Data.Char ()


main :: IO()
main = do
    putStrLn "Boas vindas!"
    putStrLn "Selecione uma das opções abaixo:\n"
    showMenu

showMenu:: IO()
showMenu = do
    putStrLn "1 - Sou Administrador"
    putStrLn "2 - Sou Cliente"
    putStrLn "3 - Sair"

    opcao <- getLine
    menus opcao

menus :: String -> IO()
menus x
    | x == "2" = menuCliente
    | otherwise = invalidOption showMenu

invalidOption :: IO() -> IO()
invalidOption f = do
        putStrLn "Selecione uma alternativa válida"
        f

menuCliente :: IO()
menuCliente = do
    putStrLn "\nSelecione uma das opções abaixo:"
    putStrLn "1 - Se cadastrar como cliente"
    putStrLn "2 - Logar no sistema como cliente"

    opcao <- getLine
    opcaoCliente opcao

opcaoCliente:: String -> IO()
opcaoCliente x
    | x == "1" = cadastrarComoCliente
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

cadastraAnimal :: IO()
cadastraAnimal = do
    animalCadastrado <- doesFileExist "animais.txt"
    putStrLn "\nInsira o nome do animal: "
    nome <- getLine
    putStrLn "\nInsira a especie do animal: "
    especie <- getLine
    putStrLn "\nInsira a altura do animal: "
    altura <- getLine
    putStrLn "\nInsira o peso do animal: "
    peso <- getLine
    file <- openFile "animais.txt" WriteMode
    hPutStr file nome
    hPutStr file especie
    hPutStr file altura
    hPutStr file peso
    putStrLn "\nAnimal cadastrado com sucesso"
    hFlush file
    hClose file
    putStrLn ""
    showMenu

cadastrarComoCliente:: IO()
cadastrarComoCliente = do
    clienteCadastrado <- doesFileExist "./clientes.txt"

    if not clienteCadastrado then do
        putStrLn "\nInsira seu nome:"
        nome <- getLine
        putStrLn "\nInsira seu email:"
        email <- getLine
        putStrLn "\nInsira sua senha:"
        senha <- getLine

        file <- openFile "./clientes.txt" WriteMode
        hPutStr file email
        hPutStr file " "
        hPutStr file senha
        putStrLn "\nCliente cadastrado com sucesso!"
        hFlush file
        hClose file
        putStrLn ""
        showMenu
    
    else do
        putStrLn "Cliente já cadastrado"
        logarComoCliente

logarComoCliente :: IO()
logarComoCliente = do
    clienteCadastrado <- doesFileExist "clientes.txt"

    if clienteCadastrado then do
        putStrLn "Insira seu email"
        email <- getLine
        putStrLn "Insira sua senha"
        senha <- getLine
        file <- openFile "clientes.txt" ReadMode
        senhaCadastrado <- hGetContents file

        putStrLn senhaCadastrado

        if senha == senhaCadastrado then do
            putStrLn "Login realizado com sucesso"
            segundoMenuCliente
        
        else do
            putStrLn "Nome ou senha incorretos"
            menuCliente
        hClose file
    
    else do
        putStrLn "Cliente não cadastrado. Por favor, cadastre-se"
        cadastrarComoCliente





    