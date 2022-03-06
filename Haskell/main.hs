{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative ()
import Control.Exception ()
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List ()
import Data.Time.Clock ()
import Distribution.PackageDescription (CondTree (condTreeComponents))
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
    hPutStrLn,
    openFile,
    putStrLn,
  )
import System.IO.Error ()
import Prelude hiding (catch)

data Animal = Animal
  { nomeAnimal :: String,
    emailCliente :: String,
    especie :: String,
    peso :: String,
    altura :: String,
    idade :: String
  }
  deriving (Read, Show)

data Admin = Admin
  { nomeAdmin :: String,
    senhaAdmin :: String,
    telefoneAdmin :: String
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
    servicos :: String,
    concluido :: Bool,
    animal :: String,
    emailDoDono :: String
  }
  deriving (Read, Show)

printLine :: IO()
printLine = putStrLn "\n------------------------------------------"

obterCliente :: Cliente -> String -> String
obterCliente Cliente {nomeCliente = n, email = e, senha = s, telefone = t} prop
  | prop == "nomeCliente" = n
  | prop == "email" = e
  | prop == "senha" = s
  | prop == "telefone" = t

editCliente :: Cliente -> Animal -> Cliente
editCliente Cliente {nomeCliente = n, email = e, senha = s, telefone = t} a = Cliente {nomeCliente = n, email = e, senha = s, telefone = t}

obterAnimal :: Animal -> String -> String
obterAnimal Animal {nomeAnimal = n, emailCliente = ec, especie = e, peso = p, altura = a, idade = i} prop
  | prop == "nomeAnimal" = n
  | prop == "emailCliente" = ec
  | prop == "especie" = e
  | prop == "peso" = p
  | prop == "altura" = a
  | prop == "idade" = i

main :: IO ()
main = do
  printLine
  putStrLn "Boas vindas!"
  showMenu

showMenu :: IO ()
showMenu = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"

  putStrLn "1 - Sou Administrador"
  putStrLn "2 - Sou Cliente"
  putStrLn "x - Sair"
  printLine

  putStr "Opção: "
  opcao <- getLine
  menus opcao

menus :: String -> IO ()
menus x
  | x == "1" = acessoAdm
  | x == "2" = menuCliente
  | x == "x" = encerrarSessao
  | otherwise = invalidOption showMenu

encerrarSessao :: IO ()
encerrarSessao = do 
  printLine
  putStrLn "Saindo... Até a próxima!"
  printLine

invalidOption :: IO () -> IO ()
invalidOption function = do
  putStrLn "Selecione uma alternativa válida"
  function

acessoAdm :: IO ()
acessoAdm = do
  printLine
  putStrLn "\nFaça acesso como administrador"
  putStr "Senha administrador: "
  senha <- getLine

  adminDados <- readFile "admin.txt"
  let admin = read adminDados :: Admin

  if obterAdmin admin "senha" == senha then do
    menuAdm
  else do
    printLine
    putStrLn "Senha inválida!"
    putStr "Deseja tentar fazer login como administrador novamente! (s/n): "
    opcao <- getChar

    if toLower opcao == 's' then do
      acessoAdm
    else
      showMenu
 
obterAdmin :: Admin -> String -> String
obterAdmin Admin {nomeAdmin = n, senhaAdmin = s, telefoneAdmin = t} prop
  | prop == "nome" = n
  | prop == "senha" = s
  | prop == "telefone" = t


menuAdm :: IO ()
menuAdm = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:"
  putStrLn "1 - Ver usuários cadastrados no sistema"
  putStrLn "2 - Remover usuários"
  putStrLn "3 - Alterar disponibilidade hotelzinho"
  putStrLn "4 - listar resumo de atendimentos"
  putStrLn "5 - Atualizar contato Adm"
  putStrLn "6 - EditarDadosDeUmAnimal"
  putStrLn "7 - Voltar"
  opcao <- getLine
  opcaoAdm opcao

opcaoAdm :: String -> IO ()
opcaoAdm x
  | x == "1" = verClientesCadastrados
  | x == "2" = removerCliente
  | x == "3" = alterarDisponibilidadeHotelzinho
  | x == "4" = listarResumoDeAtendimentos
  | x == "5" = atualizarContatoAdm
  | x == "6" = editarAnimal
  | x == "7" = showMenu
  | otherwise = invalidOption menuAdm

listarResumoDeAtendimentos :: IO ()
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

opcaoHotelzinho :: String -> IO ()
opcaoHotelzinho x
  | x == "1" = ativaHotelzinho
  | x == "2" = desativaHotelzinho
  | otherwise = invalidOption alterarDisponibilidadeHotelzinho

ativaHotelzinho :: IO ()
ativaHotelzinho = do
  file <- openFile "hotelzinho.txt" WriteMode
  hPutStr file "disponível"
  hClose file
  putStrLn "Hotelzinho foi configurado como disponível"
  menuAdm

desativaHotelzinho :: IO ()
desativaHotelzinho = do
  file <- openFile "hotelzinho.txt" WriteMode
  hPutStr file "indisponível"
  hClose file
  putStrLn "Hotelzinho foi configurado como indisponível"
  menuAdm

atualizarContatoAdm :: IO ()
atualizarContatoAdm = do
  putStrLn "\nTem certeza que deseja atualizar o contato do Administrador?"
  putStrLn "\n--Aperte 1 para continuar--"
  opcao <- getLine
  opcaoContato opcao

opcaoContato :: String -> IO ()
opcaoContato x
  | x == "1" = mudaContato
  | otherwise = invalidOption menuAdm

mudaContato :: IO ()
mudaContato = do
  putStrLn "\nInsira o novo número para contato abaixo"

  numero <- getLine
  file <- openFile "contato.txt" WriteMode
  hPutStr file numero
  hClose file
  putStrLn "\nContato atualizado com sucesso!"
  menuAdm

menuCliente :: IO ()
menuCliente = do
  putStrLn "\nSelecione uma das opções abaixo:"
  putStrLn "1 - Se cadastrar como cliente"
  putStrLn "2 - Logar no sistema como cliente"
  putStrLn "3 - Ver contato do administrador"
  putStrLn "4 - Voltar ao menu principal"
  opcao <- getLine
  opcaoCliente opcao

opcaoCliente :: String -> IO ()
opcaoCliente x
  | x == "1" = cadastrarComoCliente
  | x == "2" = logarComoCliente
  | x == "3" = verContatoDoAdministrador
  | x == "4" = showMenu
  | otherwise = invalidOption menuCliente

segundoMenuCliente :: String -> IO ()
segundoMenuCliente email = do
  putStrLn "\nSelecione o que deseja como cliente"
  putStrLn "1 - Cadastrar um novo animal"
  putStrLn "2 - Listar animais cadastrados"
  putStrLn "3 - Acessar Hotelzinho Pet"
  putStrLn "4 - Remover um animal"
  putStrLn "5 - Agendar serviço para animal"
  putStrLn "x - Retornar para o menu\n"

  opcao <- getLine
  segundaTelaCliente opcao email

segundaTelaCliente :: String -> String -> IO ()
segundaTelaCliente x email
  | x == "1" = cadastraAnimal email
  | x == "2" = listarAnimais email
  | x == "3" = menuHotelzinhoPet
  | x == "4" = removerAnimal email
  | x == "5" = agendaAnimal email
  | otherwise = invalidOption menuCliente

indexCliente :: [Cliente] -> String -> Int -> Int
indexCliente (c : cs) email i
  | obterCliente c "email" == email = i
  | obterCliente c "email" /= email = next
  where
    next = indexCliente cs email (i + 1)

converterEmAnimal a = read a :: Animal

listarAnimais :: String -> IO ()
listarAnimais emailCliente = do
  file <- openFile "animais.txt" ReadMode
  contents <- hGetContents file

  let animaisStr = lines contents
  let animais = map converterEmAnimal animaisStr

  mostrarAnimaisDoCliente emailCliente animais
  showMenu

mostrarAnimaisDoCliente :: String -> [Animal] -> IO ()
mostrarAnimaisDoCliente emailCliente [] = do
  putStrLn ""
mostrarAnimaisDoCliente emailCliente (a : as) = do
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

-- toCliente :: String -> Cliente
-- toCliente c = read c :: Cliente

-- toObjListCliente :: [String] -> [Cliente]
-- toObjListCliente = map toCliente

cadastraAnimal :: String -> IO ()
cadastraAnimal email = do
  putStr "\nInsira o nome do animal: "
  nome <- getLine

  verificaSeJaExisteUmAnimalCadastrado nome email

  putStr "\nInsira a especie do animal: "
  especie <- getLine
  putStr "\nInsira a altura do animal: "
  altura <- getLine
  putStr "\nInsira o peso do animal: "
  peso <- getLine
  putStr "\nInsira o idade do animal: "
  idade <- getLine
  putStrLn ""

  let animal = Animal {nomeAnimal = nome, emailCliente = email, peso = peso, altura = altura, especie = especie, idade = idade}

  appendFile "animais.txt" (show animal ++ "\n")

  putStrLn "\nAnimal Cadastrado com sucessos!\n"
  showMenu
  
agendaAnimal :: String -> IO()
agendaAnimal email = do
  putStrLn "\nQual será o serviço?"
  putStrLn "1 - Agendar consulta veterinária"
  putStrLn "2 - Agendar banho e tosa"

  opcao <- getLine
  opcaoServico opcao email

opcaoServico:: String -> String -> IO()
opcaoServico x email
  | x == "1" = agendarConsulta email
  | x == "2" = agendarBanhoTosa email
  | otherwise = invalidOption (agendaAnimal email)

agendarConsulta:: String -> IO()
agendarConsulta email = do
  fileExists <- doesFileExist "animais.txt"
  if not fileExists 
    then do
      putStrLn "Não há animais cadastrados!"
    else do
      putStrLn "\nData do Atendimento:"
      dataAtendimento <- getLine
      putStrLn "\nNome do Animal:"
      nome <- getLine
        
      file <- openFile "animais.txt" ReadMode
      animaisContent <- hGetContents file
      let animais = lines animaisContent
      let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nome email

      if not hasAnimal then do
          putStrLn ("\nAnimal com o nome: '" ++ nome ++ "' não cadastrado!")
        else do
          let agendamentos = Agendamento {animal = nome, date = dataAtendimento, servicos = "Consulta Veterinaria", concluido = False, emailDoDono = email}
          appendFile "agendamentos.txt" (show agendamentos ++ "\n")
          putStrLn "\nAgendamento Cadastrado com sucessos!\n"
    
    
  showMenu

agendarBanhoTosa:: String -> IO()
agendarBanhoTosa email = do
  fileExists <- doesFileExist "animais.txt"
  if not fileExists 
    then do
      putStrLn "Não há animais cadastrados!"
    else do
      putStrLn "\nData do Atendimento:"
      dataAtendimento <- getLine
      putStrLn "\nNome do Animal:"
      nome <- getLine
        
      file <- openFile "animais.txt" ReadMode
      animaisContent <- hGetContents file
      let animais = lines animaisContent
      let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nome email

      if not hasAnimal
        then do
          putStrLn ("\nAnimal com o nome: '" ++ nome ++ "' não cadastrado!")
        else do
          let agendamentos = Agendamento {animal = nome, date = dataAtendimento, servicos = "Banho e Tosa", concluido = False, emailDoDono = email}
          appendFile "agendamentos.txt" (show agendamentos ++ "\n")
          putStrLn "\nAgendamento Cadastrado com sucessos!\n"
    
    
  showMenu

verificaSeJaExisteUmAnimalCadastrado:: String -> String -> IO()
verificaSeJaExisteUmAnimalCadastrado nome email = do
  animaisFile <- openFile "animais.txt" ReadMode 
  animaisContent <- hGetContents animaisFile
  let animais = lines animaisContent
  let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nome email

  if hasAnimal then do
    putStrLn ("Você já possui um animal cadastrado com o nome '" ++ nome ++ "'.")
    putStr "Deseja inserir os dados de cadastro de animal novamente? (s/n): "
    resposta <- getLine

    if resposta == "s" then do
      hClose animaisFile
      cadastraAnimal email
    else showMenu
  else putStr ""

imprimeClientesCadastrados :: [Cliente] -> Int -> IO ()
imprimeClientesCadastrados [] 0 = putStrLn "Nenhum cliente cadastrado"
imprimeClientesCadastrados [] _ = putStrLn "Clientes listados com sucesso"
imprimeClientesCadastrados (x : xs) n = do
  putStrLn (show n ++ " - " ++ obterNomes x)
  imprimeClientesCadastrados xs (n + 1)

verClientesCadastrados :: IO ()
verClientesCadastrados = do
  file <- openFile "clientes.txt" ReadMode
  contents <- hGetContents file
  let clientes = lines contents
  imprimeClientesCadastrados [read x :: Cliente | x <- clientes] 0

removerCliente :: IO ()
removerCliente = do
  clientesCadastrados <- doesFileExist "clientes.txt"
  if not clientesCadastrados
    then do
      putStrLn "Não há clientes cadastrados!"
    else do
      putStr "\nInsira o email do cliente a ser removido: "
      email <- getLine

      file <- openFile "clientes.txt" ReadMode
      clientesContent <- hGetContents file
      let clientes = lines clientesContent
      let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] email ""

      if not hasCliente
        then do
          putStrLn ("\nCliente com email: '" ++ email ++ "' não existe!")
        else do
          removeFile "clientes.txt"
          let novaListaDeClientes = [read x :: Cliente | x <- clientes, obterEmail (read x :: Cliente) /= email]
          atualizaClientes novaListaDeClientes
          removerAnimaisDeUmCliente email

  showMenu

atualizaClientes :: [Cliente] -> IO ()
atualizaClientes [] = putStrLn "Cliente removido com sucesso!\n"
atualizaClientes (x : xs) = do
  clientesCadastrados <- doesFileExist "clientes.txt"
  if not clientesCadastrados
    then do
      file <- openFile "clientes.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
    else appendFile "clientes.txt" ("\n" ++ show x)
  atualizaClientes xs

obterEmail :: Cliente -> String
obterEmail Cliente {nomeCliente = c, email = e, senha = s, telefone = t} = e

obterSenha :: Cliente -> String
obterSenha (Cliente _ _ senha _) = senha

obterNomes :: Cliente -> String
obterNomes (Cliente nomeCliente _ _ _) = nomeCliente

encontraCliente :: [Cliente] -> String -> String -> Bool
encontraCliente [] email senha = False
-- Procura Cliente somente verificando o email
encontraCliente (c : cs) email ""
  | obterCliente c "email" == email = True
  | obterCliente c "email" /= email = encontrar
  where
    encontrar = encontraCliente cs email ""
-- Procura Cliente verificando o email e a senha
encontraCliente (c : cs) email senha
  | obterCliente c "email" == email && obterCliente c "senha" == senha = True
  | obterCliente c "email" /= email || obterCliente c "senha" /= senha = encontrar
  where
    encontrar = encontraCliente cs email senha

encontraAnimal :: [Animal] -> String -> String -> Bool
encontraAnimal [] nome emailDonoDoAnimal = False
encontraAnimal (c : cs) nome emailDonoDoAnimal
  | obterAnimal c "nomeAnimal" == nome && obterAnimal c "emailCliente" == emailDonoDoAnimal = True
  | not (obterAnimal c "nomeAnimal" == nome && obterAnimal c "emailCliente" == emailDonoDoAnimal) = encontrar
  where
    encontrar = encontraAnimal cs nome emailDonoDoAnimal

encontraERetornaAnimal :: [Animal] -> String -> String -> Animal
encontraERetornaAnimal (c : cs) nome emailDonoDoAnimal
  | obterAnimal c "nomeAnimal" == nome && obterAnimal c "emailCliente" == emailDonoDoAnimal = c
  | not (obterAnimal c "nomeAnimal" == nome && obterAnimal c "emailCliente" == emailDonoDoAnimal) = encontrar
  where
    encontrar = encontraERetornaAnimal cs nome emailDonoDoAnimal

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
      let hasThisClient = encontraCliente ([read x :: Cliente | x <- clientes]) email ""

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
      let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] email senha

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

menuHotelzinhoPet :: IO ()
menuHotelzinhoPet = do
  putStrLn "O Hotelzinho Pet é o serviço de hospedagem de animaizinhos!"
  putStrLn "Você deseja hospedar algum animalzinho no nosso serviço?"
  putStrLn "1 - Agendar animal"
  putStrLn "Caso não tenha interesse, prima qualquer outra tecla"

  opcao <- getLine
  segundaOpcaoHotelzinho opcao

segundaOpcaoHotelzinho :: String -> IO ()
segundaOpcaoHotelzinho x
  | x == "1" = agendaHotelzinho
  | otherwise = invalidOption menuAdm

agendaHotelzinho :: IO ()
agendaHotelzinho = do
  file <- openFile "agendamentos.txt" ReadMode
  disponibilidade <- hGetContents file

  if disponibilidade == "disponível"
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
      op <- getLine
      if op == "s"
        then do
          cadastrarComoCliente
        else menuCliente

verContatoDoAdministrador :: IO ()
verContatoDoAdministrador = do
  file <- openFile "contato.txt" ReadMode
  contato <- hGetContents file
  putStrLn contato

  showMenu

removerAnimal :: String -> IO ()
removerAnimal emailDonoDoAnimal = do
  animaisCadastrados <- doesFileExist "animais.txt"
  if not animaisCadastrados
    then do
      putStrLn "Não há animais cadastrados!"
    else do
      putStr "\nInsira o nome do animal a ser removido: "
      nomeAnimal <- getLine

      animaisContent <- readFile "animais.txt"
      let animais = lines animaisContent
      let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nomeAnimal emailDonoDoAnimal

      if not hasAnimal
        then do
          putStrLn ("\nAnimal de nome: '" ++ nomeAnimal ++ "' não existe!")
        else do
          removeFile "animais.txt"
          let novaListaDeAnimais = [read x :: Animal | x <- animais, not (encontrarAnimalASerRemovido (read x :: Animal) nomeAnimal emailDonoDoAnimal)]
          atualizaAnimais novaListaDeAnimais
  showMenu

removerAnimaisDeUmCliente :: String -> IO ()
removerAnimaisDeUmCliente emailDoCliente = do
  animaisCadastrados <- doesFileExist "animais.txt"
  if not animaisCadastrados then do
      putStrLn "Cliente não possuia animais cadastrados!"
    else do
      animaisContent <- readFile "animais.txt"
      let animais = lines animaisContent

      removeFile "animais.txt"
      let novaListaDeAnimais = [read x :: Animal | x <- animais, obterEmailDoDonoDoAnimal (read x :: Animal) /= emailDoCliente]
      atualizaAnimais novaListaDeAnimais

editarAnimal:: IO ()
editarAnimal = do
  putStr "Nome do animal que vai ser editado: "
  nomeAnimal <- getLine
  putStr "Email do cliente dono do animal: "
  emailDoDono <- getLine

  verificaSeAnimalEClienteExistem nomeAnimal emailDoDono

  putStr "Novo peso do animal: "
  novoPeso <- getLine
  putStr "Nova altura do animal: "
  novaAltura <- getLine
  putStr "Nova idade do animal: "
  novaIdade <- getLine

  animaisContents <- readFile "animais.txt"
  let animais = lines animaisContents

  let dadosAntigosDoAnimal = encontraERetornaAnimal [read x :: Animal | x <- animais] nomeAnimal emailDoDono

  removeFile "animais.txt"
  let novaListaDeAnimais = [read x :: Animal | x <- animais, not (encontrarAnimalASerRemovido (read x :: Animal) nomeAnimal emailDoDono)]


  let animalEditado = Animal {
      nomeAnimal = obterNomeDoAnimal dadosAntigosDoAnimal,
      emailCliente = obterEmailDoDonoDoAnimal dadosAntigosDoAnimal, 
      peso = novoPeso,
      altura = novaAltura,
      especie = obterAnimal dadosAntigosDoAnimal "especie",
      idade = novaIdade
    }
  atualizaAnimais novaListaDeAnimais
  appendFile "animais.txt" ("\n" ++ show animalEditado)
  
  showMenu

verificaSeAnimalEClienteExistem:: String -> String -> IO () 
verificaSeAnimalEClienteExistem nomeAnimal emailCliente = do
  clientesContent <- readFile "clientes.txt"
  let clientes = lines clientesContent

  let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] emailCliente ""

  if not hasCliente then do
    putStrLn ("O cliente '" ++ emailCliente ++ "' não existe. Verifique se o email foi digitado corretamente!")
    showMenu
  else do
    animaisContent <- readFile "animais.txt"
    let animais = lines animaisContent

    let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nomeAnimal emailCliente

    if not hasAnimal then do
      putStrLn ("O cliente '" ++ emailCliente ++ "' não possui o animal '" ++ nomeAnimal ++ "' cadastrado!")
      showMenu
    else putStr ""

encontrarAnimalASerRemovido :: Animal -> String -> String -> Bool
encontrarAnimalASerRemovido animal nomeDoAnimal emailDoDono = do
  obterNomeDoAnimal animal == nomeDoAnimal && obterEmailDoDonoDoAnimal animal == emailDoDono

atualizaAnimais :: [Animal] -> IO ()
atualizaAnimais [] = putStrLn "Lista de animais atualizada com sucesso!\n"
atualizaAnimais (x : xs) = do
  animaisCadastrados <- doesFileExist "animais.txt"
  if not animaisCadastrados
    then do
      file <- openFile "animais.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
    else appendFile "animais.txt" ("\n" ++ show x)
  atualizaAnimais xs

obterNomeDoAnimal :: Animal -> String
obterNomeDoAnimal animal = obterAnimal animal "nomeAnimal"

obterEmailDoDonoDoAnimal :: Animal -> String
obterEmailDoDonoDoAnimal animal = obterAnimal animal "emailCliente"

