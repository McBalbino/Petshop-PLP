:- include('./cliente.pl').
:- include('./adm.pl').

main :- 
	apresentacao, 
	mostraMenu, nl.
	
apresentacao :- 
	writeln("Bem-vindo ao sistema do PETSHOP!"), nl.

mostraMenu :- 
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Sou Administrador"),
	writeln("2 - Sou Cliente"),
	writeln("3 - Encerrar programa"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> login_adm -> menuAdm;
	Option == "2" -> menuCliente;
	Option == "3" -> sair;
	opcaoInvalida,
	mostraMenu, nl, halt).

menuAdm :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Ver usuarios cadastrados no sistema"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> listaClientes, menuAdm;
	Option == "0" -> mostraMenu;
	opcaoInvalida,
	menuAdm).

menuCliente :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Se cadastrar como cliente"),
	writeln("2 - Logar no sistema como cliente"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> cadastraCliente, menuCliente;
	Option == "2" -> (login_cliente(Email) -> segundoMenuCliente(Email) ; mostraMenu);
	Option == "0" -> mostraMenu;
	opcaoInvalida,
	menuCliente).

segundoMenuCliente(Email) :-
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Cadastrar um animal"),
	writeln("0 - Retornar ao menu principal"),
	read_line_to_string(user_input, Option),
	(Option == "1" -> (cadastraAnimal(Email), segundoMenuCliente(Email));
	Option == "0" -> mostraMenu;
	opcaoInvalida,
	segundoMenuCliente).

sair :- halt.

opcaoInvalida :-
	 writeln("Opcao invalida!"), nl.