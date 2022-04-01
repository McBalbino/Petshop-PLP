:- include('./cliente.pl').
:- include('./adm.pl').

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
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> tty_clear, listaClientes, menuAdm;
	Option == "2" -> tty_clear, remove_cliente, tty_clear, menuAdm;
	Option == "3" -> tty_clear, editar_dados_animal, tty_clear, menuAdm;
	Option == "0" -> tty_clear, mostraMenu;
	opcaoInvalida,
	menuAdm).

menuCliente :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Se cadastrar como cliente"),
	writeln("2 - Logar no sistema como cliente"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> tty_clear, cadastraCliente, tty_clear, menuCliente;
	Option == "2" -> (tty_clear, login_cliente(Email) -> tty_clear, segundoMenuCliente(Email) ; tty_clear, mostraMenu);
	Option == "0" -> tty_clear, mostraMenu;
	opcaoInvalida,
	menuCliente).

segundoMenuCliente(Email) :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Cadastrar um animal"),
	writeln("2 - Remover um animal"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> (tty_clear, cadastraAnimal(Email), tty_clear, segundoMenuCliente(Email));
	(Option == "2" -> (tty_clear, removeAnimal(Email), tty_clear, segundoMenuCliente(Email)));
	Option == "0" -> tty_clear, mostraMenu;
	opcaoInvalida,
	segundoMenuCliente).

sair :- halt.

opcaoInvalida :-
	 writeln("Opcao invalida!"), nl.