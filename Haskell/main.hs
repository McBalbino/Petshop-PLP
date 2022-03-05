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
    showMenu

showMenu:: IO()
showMenu = do
    putStrLn "\nSelecione uma das opções abaixo:\n"

    putStrLn "1 - Sou Administrador"
    putStrLn "2 - Sou Cliente"
    putStrLn "3 - Sair"

    opcao <- getLine
    menus opcao

menus :: String -> IO()
menus x
    | x == "1" = menuAdm
    | x == "2" = menuCliente
    | x == "3" = encerrarSessao
    | otherwise = invalidOption showMenu

encerrarSessao:: IO()
encerrarSessao = putStrLn "Saindo... Até a próxima!" 

invalidOption :: IO() -> IO()
invalidOption f = do
        putStrLn "Selecione uma alternativa válida"
        f

menuAdm :: IO()
menuAdm = do
    putStrLn "\nSelecione uma das opções abaixo:"
    putStrLn "1 - Ver usuários cadastrados no sistema"
    putStrLn "2 - Remover usuários"
    putStrLn "3 - Alterar disponibilidade hotelzinho"
    putStrLn "4 - listar resumo de atendimentos"
    putStrLn "5 - Atualizar contato Adm"
    putStrLn "6 - Voltar"

    opcao <- getLine
    opcaoAdm opcao

opcaoAdm :: String -> IO()
opcaoAdm x
    | x == "1" = verClientesCadastrados
    | x == "2" = removerCliente
    | x == "3" = alterarDisponibilidadeHotelzinho
    | x == "4" = listarResumoDeAtendimentos
    | x == "5" = atualizarContatoAdm
    | x == "6" = showMenu
    | otherwise = invalidOption menuAdm

listarResumoDeAtendimentos :: IO()
listarResumoDeAtendimentos = do
    file <- openFile "agendamentos.txt" ReadMode
    contents <- hGetContents file
    print (show contents)

alterarDisponibilidadeHotelzinho :: IO ()
alterarDisponibilidadeHotelzinho = do
    putStrLn "\nSelecione qual a disponibilidade do hotelzinho neste momento:"
    putStrLn "1 - Hotelzinho está disponível"
    putStrLn "2 - Hotelzinho NÃO está disponível"

    opcao <- getLine
    opcaoHotelzinho opcao

opcaoHotelzinho:: String -> IO()
opcaoHotelzinho x
    | x == "1" = ativaHotelzinho
    | x == "2" = desativaHotelzinho
    | otherwise = invalidOption alterarDisponibilidadeHotelzinho

ativaHotelzinho:: IO()
ativaHotelzinho = do
    file <- openFile "hotelzinho.txt" WriteMode
    hPutStr file "disponível"
    hClose file
    putStrLn "Hotelzinho foi configurado como disponível"
    menuAdm

desativaHotelzinho:: IO()
desativaHotelzinho = do
    file <- openFile "hotelzinho.txt" WriteMode
    hPutStr file "indisponível"
    hClose file
    putStrLn "Hotelzinho foi configurado como indisponível"
    menuAdm


atualizarContatoAdm:: IO()
atualizarContatoAdm = do
    putStrLn "\nTem certeza que deseja atualizar o contato do Administrador?"
    putStrLn "\n--Aperte 1 para continuar--"
    opcao <- getLine
    opcaoContato opcao

opcaoContato:: String -> IO()
opcaoContato x
    | x == "1" = mudaContato
    | otherwise = invalidOption menuAdm

mudaContato :: IO()
mudaContato = do
    putStrLn "\nInsira o novo número para contato abaixo"

    numero <- getLine
    file <- openFile "contato.txt" WriteMode
    hPutStr file numero
    hClose file
    putStrLn "\nContato atualizado com sucesso!"
    menuAdm

menuCliente :: IO()
menuCliente = do
    putStrLn "\nSelecione uma das opções abaixo:"
    putStrLn "1 - Se cadastrar como cliente"
    putStrLn "2 - Logar no sistema como cliente"
    putStrLn "3 - Ver contato do administrador"
    putStrLn "4 - Voltar ao menu principal"

    opcao <- getLine
    opcaoCliente opcao

opcaoCliente:: String -> IO()
opcaoCliente x
    | x == "1" = cadastrarComoCliente2
    | x == "2" = logarComoCliente
    | x == "3" = verContatoDoAdministrador
    | x == "4" = showMenu
    | otherwise = invalidOption menuCliente

segundoMenuCliente :: IO()
segundoMenuCliente = do
    putStrLn "\nSelecione o que deseja como cliente"
    putStrLn "1 - Cadastrar um novo animal"
    putStrLn "2 - Acessar Hotelzinho Pet"
    putStrLn "x - Retornar para o menu\n"

    opcao <- getLine
    segundaTelaCliente opcao

segundaTelaCliente :: String -> IO()
segundaTelaCliente x
    | x == "1" = cadastraAnimal
    | x == "2" = menuHotelzinhoPet
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

imprimeClientesCadastrados :: [Cliente] -> Int -> IO()
imprimeClientesCadastrados [] 0 = putStrLn "Nenhum cliente cadastrado"
imprimeClientesCadastrados [] _ = putStrLn "Clientes listados com sucesso" 
imprimeClientesCadastrados (x : xs) n = do
    putStrLn ((show n) ++ " - " ++ obterNomes x)
    imprimeClientesCadastrados xs (n + 1)

verClientesCadastrados :: IO()
verClientesCadastrados = do
    file <- openFile "clientes.txt" ReadMode
    contents <- hGetContents file
    let clientes = lines contents
    imprimeClientesCadastrados [read x :: Cliente | x <- clientes] 0



removerCliente:: IO()
removerCliente = do
    putStrLn "\nInsira o email do cliente a ser removido:"
    email <- getLine
    fileExists <- doesFileExist ("./clientes/" ++ email ++ ".txt")
    if not fileExists then do
        putStrLn ("\nCliente com email: '" ++ email ++ "' não existe!")
    else do
        removeFile ("./clientes/" ++ email ++ ".txt")
        putStrLn ("\nCliente com email: '" ++ email ++ "' removido com sucesso!")
    showMenu


cadastrarComoCliente :: IO()
cadastrarComoCliente = do
    putStrLn "\nInsira seu email:"
    email <- getLine
    fileExists <- doesFileExist ("./clientes/" ++ email ++ ".txt")
    if fileExists
        then do
            putStrLn "Usuario ja existente"
            showMenu
        else do
            file <- openFile ("./clientes/" ++ email ++ ".txt") WriteMode
            clientesCadastrados <- doesFileExist "clientesCadastrados.txt"
            if not clientesCadastrados then do
                fileClientesCadastrados <- openFile "clientesCadastrados.txt" WriteMode;
                hPutStr fileClientesCadastrados email
                hFlush fileClientesCadastrados
                hClose fileClientesCadastrados
            else appendFile "clientesCadastrados.txt" ("\n" ++ email)

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

obterNomes :: Cliente -> String
obterNomes (Cliente nomeCliente _ _ _ _) = nomeCliente


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




menuHotelzinhoPet :: IO()
menuHotelzinhoPet = do
    putStrLn "O Hotelzinho Pet é o serviço de hospedagem de animaizinhos!"
    putStrLn "Você deseja hospedar algum animalzinho no nosso serviço?"
    putStrLn "1 - Agendar animal"
    putStrLn "Caso não tenha interesse, prima qualquer outra tecla"

    opcao <- getLine
    segundaOpcaoHotelzinho opcao

segundaOpcaoHotelzinho :: String -> IO()
segundaOpcaoHotelzinho x
    | x == "1" = agendaHotelzinho
    | otherwise = invalidOption menuAdm

agendaHotelzinho :: IO()
agendaHotelzinho = do
    
    file <- openFile "agendamentos.txt" ReadMode
    disponibilidade <- hGetContents file
    
    if  disponibilidade == "disponível"
        then do
            file <- openFile "hotelzinho.txt" WriteMode
            putStrLn "\nInsira a especie do animalzinho a ser hospedado: "
            especie <- getLine
            putStrLn "\nInsira o nome do animalzinho "
            nome <- getLine
            putStrLn "\nQual o período de tempo que o animalzinho vai ficar hospedado?: "
            tempo <- getLine
            file <- appendFile "animais.txt" "especie: "
            file <- appendFile "animais.txt" especie
            file <- appendFile "animais.txt" "; "
            file <- appendFile "animais.txt" "nome: "
            file <- appendFile "animais.txt" nome
            file <- appendFile "animais.txt" "; "
            file <- appendFile "animais.txt" "período de tempo: "
            file <- appendFile "animais.txt" tempo
            file <- appendFile "animais.txt" "\n"
            putStrLn "\nAnimal agendado com sucesso"
            putStrLn ""
            showMenu


    else do
        putStrLn "Infelizmente o serviço de hotelzinho não está disponível para receber animaizinhos no momento."

    
        putStrLn "Cliente não cadastrado."
        putStrLn "Deseja fazer o cadastro agora? (s/n):"
        op <- getLine;
        if op == "s" then do 
            cadastrarComoCliente
        else menuCliente

verContatoDoAdministrador:: IO()
verContatoDoAdministrador = do
    file <- openFile "contato.txt" ReadMode
    contato <- hGetContents file
    putStrLn contato

    showMenu