:- include('./cliente.pl').
:- include('./adm.pl').
:- include('./servico.pl').

letreiro :-
	writeln("Bem-vindo ao sistema do "), nl,
	writeln("██████╗ ███████╗████████╗███████╗██╗  ██╗ ██████╗ ██████╗"), 
	writeln("██╔══██╗██╔════╝╚══██╔══╝██╔════╝██║  ██║██╔═══██╗██╔══██╗"),
	writeln("██████╔╝█████╗     ██║   ███████╗███████║██║   ██║██████╔╝"),
	writeln("██╔═══╝ ██╔══╝     ██║   ╚════██║██╔══██║██║   ██║██╔═══╝ "),
	writeln("██║     ███████╗   ██║   ███████║██║  ██║╚██████╔╝██║     "),
	writeln("╚═╝     ╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝     ").
                                                          

main :- 
	tty_clear, 
	mostraMenu, nl.
	

mostraMenu :-
	letreiro, nl,
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Sou Administrador"),
	writeln("2 - Sou Cliente"),
	writeln("3 - Encerrar programa"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> tty_clear, login_adm -> tty_clear, menuAdm;
	Option == "2" -> tty_clear, menuCliente;
	Option == "3" -> tty_clear, sair;
	opcaoInvalida,
	mostraMenu, nl, halt).

menuAdm :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Ver usuarios cadastrados no sistema"),
	writeln("2 - Remover usuários"),
	writeln("3 - Editar dados de um animal"),
	writeln("4 - Alterar disponibilidade do hotelzinho"),
	writeln("5 - Atualizar contato do administrador"),
	writeln("6 - Ver serviços agendados pendentes"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> tty_clear, listaClientes, menuAdm;
	Option == "2" -> tty_clear, remove_cliente, menuAdm;
	Option == "3" -> tty_clear, editar_dados_animal, tty_clear, menuAdm;
	Option == "4" -> tty_clear, alterar_configuracao_hotelzinho, menuAdm;
	Option == "5" -> tty_clear, editar_contato_administrador, menuAdm;
	Option == "6" -> tty_clear, listarServicosPendentes, tty_clear, menuAdm;
	Option == "0" -> tty_clear, mostraMenu;
	opcaoInvalida,
	menuAdm).

menuCliente :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Se cadastrar como cliente"),
	writeln("2 - Logar no sistema como cliente"),
	writeln("3 - Ver contato do administrador"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> tty_clear, cadastraCliente, menuCliente;
	Option == "2" -> (tty_clear, login_cliente(Email) -> tty_clear, segundoMenuCliente(Email) ; mostraMenu);
	Option == "3" -> (tty_clear, exibir_contato_admin, tty_clear, menuCliente);
	Option == "0" -> tty_clear, mostraMenu;
	opcaoInvalida,
	menuCliente).

segundoMenuCliente(Email) :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Cadastrar um animal"),
	writeln("2 - Listar meus animais"),
	writeln("3 - Agendar um serviço"),
	writeln("4 - Listar agendamentos"),
	writeln("5 - Remover um animal"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> (tty_clear, cadastraAnimal(Email), tty_clear, segundoMenuCliente(Email));
	(Option == "2" -> (tty_clear, listarAnimais(Email), tty_clear, segundoMenuCliente(Email)));
	(Option == "3" -> (tty_clear, menuServico(Email), tty_clear, segundoMenuCliente(Email)));
	(Option == "4" -> (tty_clear, listarServicos(Email), tty_clear, segundoMenuCliente(Email)));
	(Option == "5" -> (tty_clear, removeAnimal(Email), tty_clear, segundoMenuCliente(Email)));
	Option == "0" -> tty_clear, mostraMenu;
	opcaoInvalida,
	segundoMenuCliente).

exibir_contato_admin:- nl,
	consult('./data/bd_adm.pl'),
	administrador("adm",_,Contato),
	writeln(Contato),

	writeln("pressione qualquer tecla para voltar ao menu"),
	read_line_to_string(user_input, _).

sair :- halt.

alterar_configuracao_hotelzinho :-nl,
	writeln("Alterando a disponibilidade do hotelzinho..."),
	exibe_configuracao_hotelzinho, 
	writeln("--------------------------------------------"), 
	writeln("1 - Tornar o hotelzinho disponível"),
	writeln("2 - Tornar o hotelzinho indisponível"),
	writeln("3 - Voltar ao menu"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> ativa_desativa_hotelzinho("disponivel");
	 Option == "2" -> ativa_desativa_hotelzinho("indisponivel");
	 Option == "3" -> tty_clear, menuAdm;
	 opcaoInvalida, alterar_configuracao_hotelzinho).

ativa_desativa_hotelzinho(Status) :-
	setup_bd_configuracao_hotelzinho,
	retract(configuracao_hotelzinho("status", _)),
	assert(configuracao_hotelzinho("status", Status)),
	tell('./data/bd_configuracao_hotelzinho.pl'), nl,
	listing(configuracao_hotelzinho/2),
	told,
	tty_clear,nl,
	writeln("Configuração de hotelzinho atualizada com sucesso"),
	writeln("Aperte qualquer tecla para voltar ao menu."),
	read_line_to_string(user_input, _),
	tty_clear.
	
exibe_configuracao_hotelzinho :-nl,
	setup_bd_configuracao_hotelzinho,
	(configuracao_hotelzinho(_,"disponivel"),
	writeln("O hotelzinho encontra-se disponível");
	configuracao_hotelzinho(_,"indisponivel"),
	writeln("O hotelzinho encontra-se indisponível")).


opcaoInvalida :-
	 writeln("Opcao invalida!"), nl.