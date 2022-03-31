:- consult('./data/bd_clientes.pl').
:- consult('./data/bd_animais.pl').

setup_bd_cliente :-
	consult('./data/bd_clientes.pl').

setup_bd_login :-
	consult('./data/bd_adm.pl').

arquivo_vazio_adm :-
	\+(predicate_property(administrador(_,_,_), dynamic)).

loginAdm :-
	nl,
	writeln("Insira seu email: "),
	read_line_to_string(user_input, Email),
	writeln("Insira sua senha: "),
	read_line_to_string(user_input, Senha),
	(administrador(Email, Senha, _) -> nl, writeln("Login realizado com sucesso!");
	writeln("Senha incorreta."), nl, false).

login_adm :-
	setup_bd_login,
	arquivo_vazio_adm -> writeln("Administrador não cadastrado."), nl, false;
	(administrador(_, _, _)) -> loginAdm;
	writeln("Administrador não cadastrado."), nl, false.

listaClientes :- 
	setup_bd_cliente,
	findall(N, cliente(N, _, _, _), ListaClientes),
	exibeClientes(ListaClientes),
	told.

exibeClientes([]) :-
	nl,
	writeln("Lista de clientes vazia.").

exibeClientes([H]) :-
	writeln(H).

exibeClientes([H|T]) :-
	writeln(H),
	exibeClientes(T).

add_clientes([]).
add_clientes([[Nome,Email,Senha,Telefone]|T]) :- 
	add_cliente(Nome,Email,Senha,Telefone), add_clientes(T).

add_cliente(Nome,Email,Senha,Telefone) :- 
	assertz(cliente(Nome,Email,Senha,Telefone)).

list_clientes(C) :- 
	findall([Nome,Email,Senha,Telefone], cliente(Nome,Email,Senha,Telefone), C).

remove_cliente :- 
	nl,
	writeln("Insira o email da conta a ser excluida: "),
	read_line_to_string(user_input, Email),
    list_clientes(C),
    remove_animal(Email),
    retractall(cliente(_,_,_,_)),
    remove_cliente_aux(C, Email, C_att),
    add_clientes(C_att),
    tell('./data/bd_clientes.pl'), nl,
    listing(cliente/4),
    told.

remove_cliente_aux([],_,[]) :-
	nl,
	writeln("Usuário inexistente").
remove_cliente_aux([H|T], Email, T) :- member(Email, H).
remove_cliente_aux([H|T], Email, [H|Out]) :- remove_cliente_aux(T, Email, Out).

add_animais([]).
add_animais([[Nome, Email, Especie, Peso, Altura, Idade]|T]) :- 
	add_animal(Nome, Email, Especie, Peso, Altura, Idade), add_animais(T).

add_animal(Nome, Email, Especie, Peso, Altura, Idade) :- 
	assertz(animal(Nome, Email, Especie, Peso, Altura, Idade)).

list_animais(A) :- 
	findall([Nome, Email, Especie, Peso, Altura, Idade], animal(Nome, Email, Especie, Peso, Altura, Idade), A).


remove_animal(Email) :- 
    list_animais(A), 
    retractall(animal(_,_,_,_,_,_)),
    remove_animal_aux(A, Email, C_att),
    add_animais(C_att),
    tell('./data/bd_animais.pl'), nl,
    listing(animal/6),
    told.

remove_animal_aux([],_,[]).
remove_animal_aux([H|T], Email, Out) :- member(Email, H), remove_animal_aux(T, Email, Out).
remove_animal_aux([H|T], Email, [H|Out]) :- remove_animal_aux(T, Email, Out).