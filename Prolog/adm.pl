setup_bd_adm :-
	consult('./data/bd_clientes.pl').

setup_bd_login :-
	consult('./data/bd_adm.pl').

arquivo_vazio_adm :-
	\+(predicate_property(administrador(_,_), dynamic)).

loginAdm :-
	nl,
	writeln("Insira seu email: "),
	read_line_to_string(user_input, Email),
	writeln("Insira sua senha: "),
	read_line_to_string(user_input, Senha),
	(administrador(Email, Senha) -> nl, writeln("Login realizado com sucesso!");
	writeln("Senha incorreta."), nl, false).

login_adm :-
	setup_bd_login,
	arquivo_vazio_adm -> writeln("Administrador não cadastrado."), nl, false;
	(administrador(_, _)) -> loginAdm;
	writeln("Administrador não cadastrado."), nl, false.

listaClientes :- 
	setup_bd_adm,
	findall(N, cliente(N, _, _, _), ListaClientes),
	exibeClientes(ListaClientes),
	told.

exibeClientes([]).

exibeClientes([H|T]) :-
	writeln(H),
	exibeClientes(T).

