{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Char (isDigit, toLower)
import qualified Distribution.Compat.CharParsing as DisponibilidadeHotelzinho
import System.Directory (doesFileExist, removeFile)
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

data DisponibilidadeHotelzinho = DisponibilidadeHotelzinho
  {disponibilidade :: Bool}
  deriving (Read, Show)

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
  { agendamentoId :: Int,
    date :: String,
    servicos :: String,
    concluido :: Bool,
    animal :: String,
    emailDoDono :: String
  }
  deriving (Read, Show)

data Hospedagem = Hospedagem
  { prazoDeHospedagem :: String,
    animalHospedado :: Animal,
    contatoDoDono :: String
  }
  deriving (Read, Show)

printLine :: IO ()
printLine = putStrLn "\n------------------------------------------"

main :: IO ()
main = do
  printLine
  putStr "Boas vindas! PET SHOP FUNCIONAL"
  showMenu

showMenu :: IO ()
showMenu = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"

  putStrLn "1 - Sou Administrador"
  putStrLn "2 - Sou Cliente"
  putStrLn "3 - Sair"
  printLine

  putStr "Opção: "
  opcao <- getLine
  menus opcao

menus :: String -> IO ()
menus x
  | x == "1" = acessoAdm
  | x == "2" = menuCliente
  | x == "3" = encerrarSessao
  | otherwise = invalidOption showMenu

menuAdm :: IO ()
menuAdm = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"
  putStrLn "1 - Ver usuários cadastrados no sistema"
  putStrLn "2 - Remover usuários"
  putStrLn "3 - Alterar disponibilidade hotelzinho"
  putStrLn "4 - listar resumo de agendamentos"
  putStrLn "5 - Atualizar contato Adm"
  putStrLn "6 - Editar dados de um animal"
  putStrLn "7 - Remarcar data de um agendamento"
  putStrLn "8 - Ver serviços agendados pendentes"
  putStrLn "9 - Marcar um servico como concluido"
  putStrLn "0 - Voltar"
  printLine
  putStr "Opção: "
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
  | x == "7" = remarcarDataDoAgendamento
  | x == "8" = listarAgendamentosPendentes
  | x == "9" = marcarServicoComoConcluido
  | x == "0" = showMenu
  | otherwise = invalidOption menuAdm

menuCliente :: IO ()
menuCliente = do
  printLine
  putStrLn "\nSelecione uma das opções abaixo:\n"
  putStrLn "1 - Se cadastrar como cliente"
  putStrLn "2 - Logar no sistema como cliente"
  putStrLn "3 - Ver contato do administrador"
  putStrLn "0 - Voltar ao menu principal"
  printLine
  putStr "Opção: "
  opcao <- getLine
  opcaoCliente opcao

opcaoCliente :: String -> IO ()
opcaoCliente x
  | x == "1" = cadastrarComoCliente
  | x == "2" = logarComoCliente
  | x == "3" = verContatoDoAdministrador
  | x == "0" = showMenu
  | otherwise = invalidOption menuCliente

segundoMenuCliente :: String -> IO ()
segundoMenuCliente email = do
  printLine
  putStrLn "\nSelecione o que deseja como cliente\n"
  putStrLn "1 - Cadastrar um novo animal"
  putStrLn "2 - Listar animais cadastrados"
  putStrLn "3 - Acessar Hotelzinho Pet"
  putStrLn "4 - Remover um animal"
  putStrLn "5 - Agendar serviço para animal"
  putStrLn "6 - Ver agendamentos concluidos"
  putStrLn "7 - Ver agendamentos ainda não concluidos"
  putStrLn "8 - Cancelar agendamento"
  putStrLn "0 - Retornar para o menu"
  printLine
  putStr "Opção: "
  opcao <- getLine
  segundaTelaCliente opcao email

segundaTelaCliente :: String -> String -> IO ()
segundaTelaCliente x email
  | x == "1" = cadastraAnimal email
  | x == "2" = chamalistarAnimais email
  | x == "3" = menuHotelzinhoPet email
  | x == "4" = removerAnimal email
  | x == "5" = agendaAnimal email
  | x == "6" = agendamentosConcluidos True email
  | x == "7" = agendamentosConcluidos False email
  | x == "8" = cancelarAgendamento email
  | x == "0" = menuCliente
  | otherwise = invalidOption (segundoMenuCliente email)

chamalistarAnimais :: String -> IO ()
chamalistarAnimais email = do
  putStr ("\nListagem de animais de '" ++ email ++ "':")
  listarAnimais email
  segundoMenuCliente email

alterarDisponibilidadeHotelzinho :: IO ()
alterarDisponibilidadeHotelzinho = do
  printLine
  putStrLn "\nSelecione qual a disponibilidade do hotelzinho neste momento:"
  putStrLn "1 - Hotelzinho está disponível"
  putStrLn "2 - Hotelzinho NÃO está disponível"
  printLine
  opcao <- getLine
  opcaoHotelzinho opcao

  putStrLn "Alteração realizada com sucesso!"
  menuAdm

opcaoHotelzinho :: String -> IO ()
opcaoHotelzinho x
  | x == "1" = ativaDesativaHotelzinho True
  | x == "2" = ativaDesativaHotelzinho False
  | otherwise = invalidOption alterarDisponibilidadeHotelzinho

atualizarContatoAdm :: IO ()
atualizarContatoAdm = do
  printLine
  putStrLn "\nTem certeza que deseja atualizar o contato do Administrador?"
  putStrLn "\n--Aperte 1 para continuar--"
  printLine
  opcao <- getLine
  opcaoContato opcao

opcaoContato :: String -> IO ()
opcaoContato x
  | x == "1" = mudaContato
  | otherwise = invalidOption menuAdm

agendaAnimal :: String -> IO ()
agendaAnimal email = do
  printLine
  putStrLn "\nQual será o serviço?\n"
  putStrLn "1 - Agendar consulta veterinária"
  putStrLn "2 - Agendar banho"
  putStrLn "3 - Agendar tosa"
  putStrLn "4 - Agendar banho e tosa"
  putStrLn "5 - Agendar consulta, banho e tosa (COMBO)"
  printLine
  putStr "Opção: "
  opcao <- getLine
  opcaoServico opcao email

opcaoServico :: String -> String -> IO ()
opcaoServico x email
  | x == "1" = agendarAgendamento email "Consulta"
  | x == "2" = agendarAgendamento email "Banho"
  | x == "4" = agendarAgendamento email "Tosa"
  | x == "4" = agendarAgendamento email "Banho, Tosa"
  | x == "5" = agendarAgendamento email "Consulta, Banho, Tosa"
  | otherwise = invalidOption (agendaAnimal email)

menuHotelzinhoPet :: String -> IO ()
menuHotelzinhoPet email = do
  printLine
  putStrLn "\nO Hotelzinho Pet é o serviço de hospedagem de animaizinhos!\n"
  putStrLn "Você deseja hospedar algum animalzinho no nosso serviço?\n"
  putStrLn "1 - Agendar animal"
  putStrLn "Caso não tenha interesse, prima qualquer outra tecla"
  printLine
  putStr "Opção: "
  opcao <- getLine
  lerOpcaoHotelzinho opcao email

lerOpcaoHotelzinho :: String -> String -> IO ()
lerOpcaoHotelzinho x email
  | x == "1" = agendaHotelzinho email
  | otherwise = do
    putStrLn "Servico de hotelzinho cancelado"
    segundoMenuCliente email

encerrarSessao :: IO ()
encerrarSessao = do
  printLine
  putStrLn "Saindo... Até a próxima!"
  printLine

invalidOption :: IO () -> IO ()
invalidOption function = do
  putStrLn "\nSelecione uma alternativa válida"
  function

------- Metodos Admnistrador -------

imprimeClientesCadastrados :: [Cliente] -> Int -> IO ()
imprimeClientesCadastrados [] 0 = putStrLn "\nNenhum cliente cadastrado"
imprimeClientesCadastrados [] _ = putStrLn "\nClientes listados com sucesso"
imprimeClientesCadastrados (x : xs) n = do
  putStrLn (show n ++ " - " ++ obterNomes x)
  imprimeClientesCadastrados xs (n + 1)

verClientesCadastrados :: IO ()
verClientesCadastrados = do
  arquivoExiste <- doesFileExist "clientes.txt"

  if arquivoExiste then do
    file <- openFile "clientes.txt" ReadMode
    contents <- hGetContents file
    let clientes = lines contents

    printLine
    imprimeClientesCadastrados [read x :: Cliente | x <- clientes] 0
  else do
    putStrLn "\nNão há clientes cadastrados."
  menuAdm


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
          removerAgendamentosDeUmCliente email

          putStrLn "Cliente removido com sucesso, todos os animais e agendamentos não realizados do cliente também foram removidos!"

  menuAdm

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

acessoAdm :: IO ()
acessoAdm = do
  printLine
  putStrLn "\nFaça acesso como administrador"
  putStr "Senha administrador: "
  senha <- getLine

  adminDados <- readFile "admin.txt"
  let admin = read adminDados :: Admin

  if obterAdmin admin "senha" == senha
    then do
      menuAdm
    else do
      printLine
      putStrLn "Senha inválida!"
      putStr "Deseja tentar fazer login como administrador novamente! (s/n): "
      opcao <- getChar

      if toLower opcao == 's'
        then do
          acessoAdm
        else showMenu

remarcarDataDoAgendamento :: IO ()
remarcarDataDoAgendamento = do
  putStrLn "Digite (x) para desistir de remarcar agendamento"
  putStr "Digite o id do agendamento que será remarcado: "
  id <- getLine

  if id == "x" then do
    menuAdm
  else
    if not (all isDigit id)
      then do
        putStrLn "Formato inválido! Digite um número!"
        remarcarDataDoAgendamento
      else do
        arquivoExiste <- doesFileExist "agendamentos.txt"
        if not arquivoExiste then do
          putStrLn "\nNão existem agendamentos à serem remarcados"
        else do
          agendamentosContent <- readFile "agendamentos.txt"
          let agendamentos = [read x :: Agendamento | x <- lines agendamentosContent]
          let hasAgendamento = encontrarAgendamento agendamentos (read id :: Int)

          if not hasAgendamento
            then do
              putStrLn ("Agendamento com id '" ++ id ++ "' não existe!")
              remarcarDataDoAgendamento
            else do
              let agendamentoDados = encontraERetornaAgendamento agendamentos (read id :: Int)

              if obterAgendamentoStatusDeConcluido agendamentoDados
                then do
                  putStrLn "Esse atendimento já foi realizado e não pode ter sua data alterada!"
                  remarcarDataDoAgendamento
                else do
                  putStr "Nova data do atendimento: "
                  novaData <- getLine

                  let novoAgendamento =
                        Agendamento
                          { agendamentoId = obterAgendamentoId agendamentoDados,
                            animal = obterAgendamento agendamentoDados "animal",
                            date = novaData,
                            servicos = obterAgendamento agendamentoDados "servicos",
                            concluido = False,
                            emailDoDono = obterAgendamento agendamentoDados "emailDoDono"
                          }

                  removeFile "agendamentos.txt"
                  atualizarAgendamentos (novoAgendamento : [x | x <- agendamentos, obterAgendamentoId x /= (read id :: Int)])
                  putStr ("Data do agendamento '" ++ id ++ "' alterado com sucesso!")
  menuAdm

marcarServicoComoConcluido :: IO ()
marcarServicoComoConcluido = do
  putStr "Digite o id do agendamento que será marcado como concluido: "
  id <- getLine

  if not (all isDigit id)
    then do
      putStrLn "Formato inválido! Digite um número!"
      marcarServicoComoConcluido
    else do
      fileExists <- doesFileExist "agendamentos.txt"
      if not fileExists then do
        putStrLn "Não existem agendamentos até o momento."
      else do
        agendamentosContent <- readFile "agendamentos.txt"
        let agendamentos = [read x :: Agendamento | x <- lines agendamentosContent]
        let hasAgendamento = encontrarAgendamento agendamentos (read id :: Int)

        if not hasAgendamento
          then do
            putStrLn ("Agendamento com id '" ++ id ++ "' não existe!")
            marcarServicoComoConcluido
          else do
            let agendamentoDados = encontraERetornaAgendamento agendamentos (read id :: Int)

            if obterAgendamentoStatusDeConcluido agendamentoDados
              then do
                putStrLn "Esse atendimento já foi marcado como concluido!"
                marcarServicoComoConcluido
              else do
                let novoAgendamento =
                      Agendamento
                        { agendamentoId = obterAgendamentoId agendamentoDados,
                          animal = obterAgendamento agendamentoDados "animal",
                          date = obterAgendamento agendamentoDados "date",
                          servicos = obterAgendamento agendamentoDados "servicos",
                          concluido = True,
                          emailDoDono = obterAgendamento agendamentoDados "emailDoDono"
                        }

                removeFile "agendamentos.txt"
                atualizarAgendamentos (novoAgendamento : [x | x <- agendamentos, obterAgendamentoId x /= (read id :: Int)])
                putStr ("Data do agendamento '" ++ id ++ "' alterado com sucesso!")
  menuAdm

listarResumoDeAtendimentos :: IO ()
listarResumoDeAtendimentos = do
  arquivoExiste <- doesFileExist "agendamentos.txt"
  if arquivoExiste then do
    file <- openFile "agendamentos.txt" ReadMode
    contents <- hGetContents file
    let agendamentos = [read x :: Agendamento | x <- lines contents]
    putStrLn "\nListagem de agendendamentos"
    imprimirResumosDeAtendimento agendamentos
  else do
    putStrLn "\nNão existem agendamentos cadastrados"
  menuAdm

imprimirResumosDeAtendimento :: [Agendamento] -> IO ()
imprimirResumosDeAtendimento [] = do
  putStrLn "Nenhum agendamento cadastrado"
  putStrLn ""
  printLine
imprimirResumosDeAtendimento [a] = do
  imprimirAtendimento a
  printLine
imprimirResumosDeAtendimento (a : as) = do
  imprimirAtendimento a
  imprimirResumosDeAtendimento as

imprimirAtendimento :: Agendamento -> IO ()
imprimirAtendimento a = do
  putStrLn "\n"
  putStrLn ("Data: " ++ obterAgendamento a "date")
  putStrLn ("Concluído: " ++ obterAgendamento a "concluido")
  putStrLn ("Nome do animal: " ++ obterAgendamento a "animal")
  putStrLn ("Dono: " ++ obterAgendamento a "emailDoDono")
  putStrLn ("Serviços: " ++ obterAgendamento a "servicos")
  putStrLn ("Concluído: " ++ show (obterAgendamentoStatusDeConcluido a))
  putStrLn ("ID: " ++ show (obterAgendamentoId a))

ativaDesativaHotelzinho :: Bool -> IO ()
ativaDesativaHotelzinho x = do
  removeFile "disponibilidadeHotelzinho.txt"
  disponibilidadeFile <- openFile "disponibilidadeHotelzinho.txt" WriteMode

  let disponibilidade =
        DisponibilidadeHotelzinho
          { disponibilidade = x
          }
  hPutStr disponibilidadeFile (show disponibilidade)
  hFlush disponibilidadeFile
  hClose disponibilidadeFile

mudaContato :: IO ()
mudaContato = do
  adminContent <- readFile "admin.txt"
  let adminDados = read adminContent :: Admin

  putStr "\nInsira o novo número de contato: "
  novoNumero <- getLine

  removeFile "admin.txt"
  adminFile <- openFile "admin.txt" WriteMode

  let admin =
        Admin
          { nomeAdmin = obterAdmin adminDados "nome",
            senhaAdmin = obterAdmin adminDados "senha",
            telefoneAdmin = novoNumero
          }

  hPutStr adminFile (show admin)
  hFlush adminFile
  hClose adminFile

  putStrLn "\nContato atualizado com sucesso!"
  menuAdm

removerAnimaisDeUmCliente :: String -> IO ()
removerAnimaisDeUmCliente emailDoCliente = do
  animaisCadastrados <- doesFileExist "animais.txt"
  if not animaisCadastrados
    then do
      putStrLn "Cliente não possuia animais cadastrados!"
    else do
      animaisContent <- readFile "animais.txt"
      let animais = lines animaisContent

      removeFile "animais.txt"
      let novaListaDeAnimais = [read x :: Animal | x <- animais, obterAnimal (read x :: Animal) "emailCliente" /= emailDoCliente]
      atualizaAnimais novaListaDeAnimais

editarAnimal :: IO ()
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

  let animalEditado =
        Animal
          { nomeAnimal = obterAnimal dadosAntigosDoAnimal "nomeAnimal",
            emailCliente = obterAnimal dadosAntigosDoAnimal "emailCliente",
            peso = novoPeso,
            altura = novaAltura,
            especie = obterAnimal dadosAntigosDoAnimal "especie",
            idade = novaIdade
          }
  atualizaAnimais (novaListaDeAnimais ++ [animalEditado])

  menuAdm

listarAgendamentosPendentes :: IO ()
listarAgendamentosPendentes = do
   
   fileExists <- doesFileExist "agendamentos.txt"
   if not fileExists then do
     putStr "Ainda não existem agendamentos realizados"
     menuAdm
   else do
    file <- openFile "agendamentos.txt" ReadMode
    contents <- hGetContents file

    let agendamentosStr = lines contents
    let agendamentos = map converterEmAgendamento agendamentosStr

    putStrLn "\nListagem de agendamentos pendentes:"
    mostrarAgendamentosPendentes agendamentos
    menuAdm

mostrarAgendamentosPendentes :: [Agendamento] -> IO ()
mostrarAgendamentosPendentes [] = do
  putStrLn ""
mostrarAgendamentosPendentes (a : as) = do
  if obterAgendamentoStatusDeConcluido a
    then do
      mostrarAgendamentosPendentes as
    else do
      putStrLn ""
      putStrLn ("Nome: " ++ obterAgendamento a "animal")
      putStrLn ("Data: " ++ obterAgendamento a "date")
      putStrLn ("Servico: " ++ obterAgendamento a "servicos")
      putStrLn ("Email do Dono: " ++ obterAgendamento a "emailDoDono")
      mostrarAgendamentosPendentes as

------------------------------------

--------- Metodos Clientes ---------

agendamentosConcluidos :: Bool -> String -> IO ()
agendamentosConcluidos status email = do
  fileExists <- doesFileExist "agendamentos.txt"
  if not fileExists then do
    putStrLn "Não existem agendamentos efetuados até o momento"
  else do
    file <- openFile "agendamentos.txt" ReadMode
    contents <- hGetContents file

    let agendamentosStr = lines contents
    let agendamentos = map converterEmAgendamento agendamentosStr

    printLine
    putStrLn "\nListagem de agendamentos:"
    mostrarAgendamentosDoCliente agendamentos email status
  segundoMenuCliente email

mostrarAgendamentosDoCliente :: [Agendamento] -> String -> Bool -> IO ()
mostrarAgendamentosDoCliente [] email status = do
  putStrLn ""
mostrarAgendamentosDoCliente (a : as) email status = do
  if ((obterAgendamentoStatusDeConcluido a == status) && (obterAgendamento a "emailDoDono" == email))
    then do
      putStrLn ""
      putStrLn ("Serviço: " ++ (obterAgendamento a "servicos"))
      putStrLn ("email: " ++ obterAgendamento a "emailDoDono")
      putStrLn ("data: " ++ obterAgendamento a "date")
      putStrLn ("animal: " ++ obterAgendamento a "animal")
      putStr ("Concluído: " ++ (show (obterAgendamentoStatusDeConcluido a)))
      mostrarAgendamentosDoCliente as email status
    else mostrarAgendamentosDoCliente as email status

cadastraAnimal :: String -> IO ()
cadastraAnimal email = do
  putStr "\nInsira o nome do animal: "
  nome <- getLine

  verificaSeJaExisteUmAnimalCadastrado nome email

  putStr "Insira a especie do animal: "
  especie <- getLine
  putStr "Insira a altura do animal: "
  altura <- getLine
  putStr "Insira o peso do animal: "
  peso <- getLine
  putStr "Insira o idade do animal: "
  idade <- getLine
  putStrLn ""

  let animal = Animal {nomeAnimal = nome, emailCliente = email, peso = peso, altura = altura, especie = especie, idade = idade}

  animaisCadastrados <- doesFileExist "animais.txt"

  if animaisCadastrados
    then do
      appendFile "animais.txt" ("\n" ++ show animal)

      putStrLn "Animal Cadastrado com sucessos!"
    else appendFile "animais.txt" (show animal)
  segundoMenuCliente email

converterEmAgendamento :: String -> Agendamento
converterEmAgendamento a = read a :: Agendamento

agendarAgendamento :: String -> String -> IO ()
agendarAgendamento email servico = do
  fileExists <- doesFileExist "animais.txt"
  if not fileExists
    then do
      putStrLn "Não há animais cadastrados!"
    else do
      putStr "\nData do Atendimento: "
      dataAtendimento <- getLine
      putStr "Nome do Animal: "
      nome <- getLine

      file <- openFile "animais.txt" ReadMode
      animaisContent <- hGetContents file
      let animais = lines animaisContent
      let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nome email

      if not hasAnimal
        then do
          putStrLn ("\nAnimal com o nome: '" ++ nome ++ "' não cadastrado!")
        else do
          
          fileExists <- doesFileExist "agendamentos.txt"
          
          if not fileExists then do
            
            let agendamento = Agendamento {agendamentoId = 0, animal = nome, date = dataAtendimento, servicos = servico, concluido = False, emailDoDono = email}
            
            atualizarAgendamentos [agendamento]
            putStrLn "Agendamento cadastrado com sucesso!"

          else do
            agendamentosContent <- readFile "agendamentos.txt"
            let agendamentos = [read x :: Agendamento | x <- lines agendamentosContent]
            let agendamentoId = makeAgendamentoId agendamentos

            let agendamento = Agendamento {agendamentoId = agendamentoId, animal = nome, date = dataAtendimento, servicos = servico, concluido = False, emailDoDono = email}

            let antigaListaDeAgendamentos = agendamentos
            let novaListaDeAgendamentos = antigaListaDeAgendamentos ++ [agendamento]

            removeFile "agendamentos.txt"
            atualizarAgendamentos novaListaDeAgendamentos
            putStrLn "Agendamento cadastrado com sucesso!"
  segundoMenuCliente email

cadastrarComoCliente :: IO ()
cadastrarComoCliente = do
  putStr "\nInsira seu nome: "
  nome <- getLine

  putStr "Insira seu email: "
  email <- getLine

  putStr "Insira sua senha: "
  senha <- getLine

  putStr "Insira seu telefone: "
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
          menuCliente
        else do
          criarCliente nome email senha telefone
    else do
      criarCliente nome email senha telefone

criarCliente :: String -> String -> String -> String -> IO ()
criarCliente nome email senha telefone = do
  let cliente = Cliente {nomeCliente = nome, email = email, senha = senha, telefone = telefone}

  clientesCadastrados <- doesFileExist "clientes.txt"

  if clientesCadastrados
    then do
      file <- appendFile "clientes.txt" ("\n" ++ show cliente)
      putStrLn "Cliente cadastrado com sucesso!"
      menuCliente
    else do
      file <- appendFile "clientes.txt" (show cliente)
      menuCliente

logarComoCliente :: IO ()
logarComoCliente = do
  printLine
  putStr "Insira seu email: "
  email <- getLine
  fileExists <- doesFileExist "clientes.txt"

  if fileExists
    then do
      putStr "Insira sua senha: "
      senha <- getLine
      file <- openFile "clientes.txt" ReadMode
      contents <- hGetContents file
      let clientes = lines contents
      let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] email senha

      if hasCliente
        then do
          putStrLn "\nLogin realizado com sucesso"
          segundoMenuCliente email
        else do
          putStrLn "\nNome ou senha incorretos"
          menuCliente
      hClose file
    else do
      putStrLn "Nenhum cliente não cadastrado. Por favor, cadastre-se"
      cadastrarComoCliente

agendaHotelzinho :: String -> IO ()
agendaHotelzinho email = do
  file <- openFile "disponibilidadeHotelzinho.txt" ReadMode
  disponibilidadeContent <- hGetContents file
  let disponibilidade = read disponibilidadeContent :: DisponibilidadeHotelzinho
  if obterDisponibilidade disponibilidade
    then do
      
      fileExists <- doesFileExist "animais.txt"
      if not fileExists then do
        putStrLn "\nNão existem animais à serem agendados para o hotelzinho"
        segundoMenuCliente email
      else do
        putStrLn "Aqui estão os seus animais:\n"
        listarAnimais email

        putStrLn "----------------------------------------------------------------"
        putStrLn "Insira o nome do animal que você deseja hospedar no hotelzinho: "

        nomeDoAnimal <- getLine
        animaisContent <- readFile "animais.txt"

        let animais = lines animaisContent
        let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nomeDoAnimal email

        if not hasAnimal
          then do
            putStrLn "\nAVISO!"
            putStrLn ("O cliente '" ++ email ++ "' não possui o animal '" ++ nomeDoAnimal ++ "' cadastrado!\n")
            putStrLn "Retornando ao menu principal...\n"
            segundoMenuCliente email
          else do
            putStr "Até que dia você deseja fazer a reserva do hotelzinho pet? inserir em formato DIA/MES/ANO: "
            periodo <- getLine

            animaisContents <- readFile "animais.txt"
            let animais = lines animaisContents

            agendarAnimalNoHotelzinho (encontraERetornaAnimal [read x :: Animal | x <- animais] nomeDoAnimal email) email periodo
    else do
      putStrLn "Infelizmente o serviço de hotelzinho não está disponível no momento. Tente novamente mais tarde!"
      segundoMenuCliente email

agendarAnimalNoHotelzinho :: Animal -> String -> String -> IO ()
agendarAnimalNoHotelzinho animal email periodo = do
  let hospedagem = Hospedagem {prazoDeHospedagem = periodo, animalHospedado = animal, contatoDoDono = email}

  hospedagensCadastradas <- doesFileExist "hospedagem.txt"

  file <- appendFile "hospedagem.txt" (show hospedagem ++ "\n")
  putStrLn "|-------------------------------------------|"
  putStrLn "|Reserva de hospedagem efetuada com sucesso!|"
  putStrLn "|-------------------------------------------|"
 
  segundoMenuCliente email

verContatoDoAdministrador :: IO ()
verContatoDoAdministrador = do
  adminContent <- readFile "admin.txt"
  let admin = read adminContent :: Admin
  putStr "\nContato: "
  putStrLn (obterAdmin admin "telefone")

  menuCliente

removerAnimal :: String -> IO ()
removerAnimal emailDonoDoAnimal = do
  animaisCadastrados <- doesFileExist "animais.txt"
  if not animaisCadastrados
    then do
      putStrLn "Não há animais cadastrados!"
    else do
      putStr "\nInsira o nome do animal a ser removido: "
      nomeAnimal <- getLine
      putStrLn ""

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
          removerAgendamentosDeUmAnimal emailDonoDoAnimal nomeAnimal

          putStrLn "Animal removido com sucesso e agendamentos pendentes desse animal cancelados!"
  segundoMenuCliente emailDonoDoAnimal

removerAgendamentosDeUmAnimal :: String -> String -> IO ()
removerAgendamentosDeUmAnimal emailDoCliente nomeAnimal = do
  agendamentosCadastrados <- doesFileExist "agendamentos.txt"
  if not agendamentosCadastrados
    then do
      putStrLn "Animal não possuia agendamentos!"
    else do
      agendamentosContent <- readFile "agendamentos.txt"
      let agendamentos = lines agendamentosContent

      removeFile "agendamentos.txt"
      let novaListaDeAgendamentos = [read x :: Agendamento | x <- agendamentos, not (verificaAgendamentoASerRemovido (read x :: Agendamento) emailDoCliente nomeAnimal)]
      atualizarAgendamentos novaListaDeAgendamentos

removerAgendamentosDeUmCliente :: String -> IO ()
removerAgendamentosDeUmCliente emailDoCliente = do
  agendamentosCadastrados <- doesFileExist "agendamentos.txt"
  if not agendamentosCadastrados
    then do
      putStrLn "Cliente não possuia agendamentos!"
    else do
      agendamentosContent <- readFile "agendamentos.txt"
      let agendamentos = lines agendamentosContent

      removeFile "agendamentos.txt"
      let novaListaDeAgendamentos = [read x :: Agendamento | x <- agendamentos, not (verificaAgendamentoASerRemovidoByCliente (read x :: Agendamento) emailDoCliente)]
      atualizarAgendamentos novaListaDeAgendamentos

cancelarAgendamento :: String -> IO ()
cancelarAgendamento emailCliente = do
  
  fileExists <- doesFileExist "agendamentos.txt"
  if not fileExists then do
    putStrLn "Não existem agendamentos à serem cancelados."
    segundoMenuCliente emailCliente
  else do
    file <- openFile "agendamentos.txt" ReadMode
    contents <- hGetContents file
    putStrLn "Escolha um agendamento para cancelar: \n"
    escolherAgendamento [read a :: Agendamento | a <- lines contents, obterAgendamento (read a :: Agendamento) "emailDoDono" == emailCliente] emailCliente

escolherAgendamento :: [Agendamento] -> String -> IO ()
escolherAgendamento [] emailCliente = do
  putStrLn "Nenhum agendamento cadastrado \n"
  segundoMenuCliente emailCliente
escolherAgendamento [a] emailCliente = do
  imprimirEscolhaAgendamento a
  printLine
  putStr "opção: "
  opcao <- getLine
  removerAgendamento opcao emailCliente
escolherAgendamento (a : as) emailCliente = do
  imprimirEscolhaAgendamento a
  escolherAgendamento as emailCliente

imprimirEscolhaAgendamento :: Agendamento -> IO ()
imprimirEscolhaAgendamento a = do
  putStrLn (obterAgendamento a "agendamentoId" ++ " - " ++ "Animal: " ++ obterAgendamento a "animal" ++ "; " ++ "Data: " ++ obterAgendamento a "date")

removerAgendamento :: String -> String -> IO ()
removerAgendamento opcao emailCliente = do
  contents <- readFile "agendamentos.txt"
  let agendamentos = [read a :: Agendamento | a <- lines contents]
  let hasAgendamento = encontrarAgendamento agendamentos (read opcao :: Int)

  if not hasAgendamento
    then do
      putStrLn "Agendamento não encontrado"
      cancelarAgendamento emailCliente
    else do
      removeFile "agendamentos.txt"
      atualizarAgendamentos [a | a <- agendamentos, obterAgendamento a "agendamentoId" /= opcao]
      putStrLn "Agendamento cancelado com sucesso!"
      segundoMenuCliente emailCliente

------------------------------------

-------- Metodos auxiliares --------

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

obterAdmin :: Admin -> String -> String
obterAdmin Admin {nomeAdmin = n, senhaAdmin = s, telefoneAdmin = t} prop
  | prop == "nome" = n
  | prop == "senha" = s
  | prop == "telefone" = t

indexCliente :: [Cliente] -> String -> Int -> Int
indexCliente (c : cs) email i
  | obterCliente c "email" == email = i
  | obterCliente c "email" /= email = next
  where
    next = indexCliente cs email (i + 1)

converterEmAnimal :: String -> Animal
converterEmAnimal a = read a :: Animal

listarAnimais :: String -> IO ()
listarAnimais emailCliente = do
  arquivoExiste <- doesFileExist "animais.txt"
  if arquivoExiste then do
    file <- openFile "animais.txt" ReadMode
    contents <- hGetContents file

    let animaisStr = lines contents
    let animais = map converterEmAnimal animaisStr

    mostrarAnimaisDoCliente emailCliente animais
  else do 
    putStrLn " não existem animais cadastrados até o momento"
    segundoMenuCliente emailCliente
  

mostrarAnimaisDoCliente :: String -> [Animal] -> IO ()
mostrarAnimaisDoCliente emailCliente [] = do
  putStrLn ""
mostrarAnimaisDoCliente emailCliente (a : as) = do
  if obterAnimal a "emailCliente" /= emailCliente
    then do
      mostrarAnimaisDoCliente emailCliente as
    else do
      putStrLn "\n"
      putStrLn ("Nome: " ++ obterAnimal a "nomeAnimal")
      putStrLn ("Especie: " ++ obterAnimal a "especie")
      putStrLn ("Peso: " ++ obterAnimal a "peso")
      putStrLn ("Altura: " ++ obterAnimal a "altura")
      putStr ("Idade: " ++ obterAnimal a "idade")
      mostrarAnimaisDoCliente emailCliente as

toStringListCliente :: [Cliente] -> String
toStringListCliente (x : xs) = show x ++ "\n" ++ toStringListCliente xs
toStringListCliente [] = ""

toCliente :: String -> Cliente
toCliente c = read c :: Cliente

toObjListCliente :: [String] -> [Cliente]
toObjListCliente = map toCliente

atualizarAgendamentos :: [Agendamento] -> IO ()
atualizarAgendamentos [] = putStrLn "Agendamentos atualizados com sucesso!\n"
atualizarAgendamentos (x : xs) = do
  agendamentosCadastrados <- doesFileExist "agendamentos.txt"
  if not agendamentosCadastrados
    then do
      file <- openFile "agendamentos.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
    else appendFile "agendamentos.txt" ("\n" ++ show x)
  atualizarAgendamentos xs

verificaSeJaExisteUmAnimalCadastrado :: String -> String -> IO ()
verificaSeJaExisteUmAnimalCadastrado nome email = do
  fileExist <- doesFileExist "animais.txt"

  if fileExist then do
    animaisFile <- openFile "animais.txt" ReadMode
    animaisContent <- hGetContents animaisFile
    let animais = lines animaisContent
    let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nome email

    if hasAnimal
      then do
        putStrLn ("Você já possui um animal cadastrado com o nome '" ++ nome ++ "'.")
        putStr "Deseja inserir os dados de cadastro de animal novamente? (s/n): "
        resposta <- getLine

        if resposta == "s"
          then do
            hClose animaisFile
            cadastraAnimal email
          else segundoMenuCliente email
      else putStr ""
  else do
    putStr ""

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

obterDisponibilidade :: DisponibilidadeHotelzinho -> Bool
obterDisponibilidade DisponibilidadeHotelzinho {disponibilidade = d} = d

verificaSeAnimalEClienteExistem :: String -> String -> IO ()
verificaSeAnimalEClienteExistem nomeAnimal emailCliente = do
  arquivoExiste <- doesFileExist "animais.txt"
  if not arquivoExiste then do
    putStrLn "\nNão existe animal associado à este cliente."
  else do
    clientesContent <- readFile "clientes.txt"
    let clientes = lines clientesContent
    let hasCliente = encontraCliente [read x :: Cliente | x <- clientes] emailCliente ""
    if not hasCliente
      then do
        putStrLn ("O cliente '" ++ emailCliente ++ "' não existe. Verifique se o email foi digitado corretamente!")
        menuAdm
      else do
        animaisContent <- readFile "animais.txt"
        let animais = lines animaisContent

        let hasAnimal = encontraAnimal [read x :: Animal | x <- animais] nomeAnimal emailCliente

        if not hasAnimal
          then do
            putStrLn ("O cliente '" ++ emailCliente ++ "' não possui o animal '" ++ nomeAnimal ++ "' cadastrado!")
            menuAdm
          else putStr ""

encontrarAnimalASerRemovido :: Animal -> String -> String -> Bool
encontrarAnimalASerRemovido animal nomeDoAnimal emailDoDono = do
  obterAnimal animal "nomeAnimal" == nomeDoAnimal && obterAnimal animal "emailCliente" == emailDoDono

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

makeAgendamentoId :: [Agendamento] -> Int
makeAgendamentoId [] = 0
makeAgendamentoId [agendamento] = obterAgendamentoId agendamento + 1
makeAgendamentoId (agendamento : resto) = do
  makeAgendamentoId resto

obterAgendamentoId :: Agendamento -> Int
obterAgendamentoId Agendamento {agendamentoId = i, date = d, servicos = s, concluido = c, animal = a, emailDoDono = e} = i

obterAgendamentoStatusDeConcluido :: Agendamento -> Bool
obterAgendamentoStatusDeConcluido Agendamento {agendamentoId = i, date = d, servicos = s, concluido = c, animal = a, emailDoDono = e} = c

encontrarAgendamento :: [Agendamento] -> Int -> Bool
encontrarAgendamento [] agendamentoId = False
encontrarAgendamento (c : cs) agendamentoId
  | obterAgendamentoId c == agendamentoId = True
  | obterAgendamentoId c /= agendamentoId = encontrar
  where
    encontrar = encontrarAgendamento cs agendamentoId

encontraERetornaAgendamento :: [Agendamento] -> Int -> Agendamento
encontraERetornaAgendamento (c : cs) agendamentoId
  | obterAgendamentoId c == agendamentoId = c
  | obterAgendamentoId c /= agendamentoId = encontrar
  where
    encontrar = encontraERetornaAgendamento cs agendamentoId

obterAgendamento :: Agendamento -> String -> String
obterAgendamento Agendamento {agendamentoId = i, date = d, servicos = s, concluido = c, animal = a, emailDoDono = e} prop
  | prop == "agendamentoId" = show i
  | prop == "date" = d
  | prop == "animal" = a
  | prop == "concluido" = show c
  | prop == "servicos" = s
  | prop == "emailDoDono" = e

verificaAgendamentoASerRemovido :: Agendamento -> String -> String -> Bool
verificaAgendamentoASerRemovido agendamento emailDoDono nomeDoAnimal = do
  obterAgendamento agendamento "animal" == nomeDoAnimal && obterAgendamento agendamento "emailDoDono" == emailDoDono && not (obterAgendamentoStatusDeConcluido agendamento)

verificaAgendamentoASerRemovidoByCliente :: Agendamento -> String -> Bool
verificaAgendamentoASerRemovidoByCliente agendamento emailDoDono = do
  obterAgendamento agendamento "emailDoDono" == emailDoDono && not (obterAgendamentoStatusDeConcluido agendamento)
