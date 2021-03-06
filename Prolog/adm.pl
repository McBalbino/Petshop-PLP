:- consult('./data/bd_clientes.pl').
:- consult('./data/bd_animais.pl').
:- consult('./data/bd_configuracao_hotelzinho.pl').


setup_bd_cliente :-
	consult('./data/bd_clientes.pl').

setup_bd_login :-
	consult('./data/bd_adm.pl').

setup_bd_configuracao_hotelzinho :-
	consult('./data/bd_configuracao_hotelzinho.pl').

arquivo_vazio_adm :-
	\+(predicate_property(administrador(_,_,_), dynamic)).

loginAdm :-
	nl,
	writeln("Insira seu email: "),
	read_line_to_string(user_input, Email),
	writeln("Insira sua senha: "),
	read_line_to_string(user_input, Senha),
	(administrador(Email, Senha, _) -> nl, writeln("Login realizado com sucesso!"), nl;
	writeln("Senha incorreta."), nl, false).

login_adm :-
	setup_bd_login,
	arquivo_vazio_adm -> writeln("Administrador não cadastrado."), nl, false;
	(administrador(_, _, _)) -> loginAdm;
	writeln("Administrador não cadastrado."), nl, false.

listaClientes :- 
	setup_bd_cliente,
	findall(N, cliente(N, _, _, _), ListaClientes),
	writeln("Usuários cadastrados: "),
	exibeClientes(ListaClientes),
	told, nl,
	fimListagemClientes.

fimListagemClientes:-
	writeln("Clique em enter para continuar: "),
	read_line_to_string(user_input, _).

exibeClientes([]) :-
	nl,
	writeln("Nenhum usuário cadastrado.").

exibeClientes([H]) :-
	write("- "),
	writeln(H).

exibeClientes([H|T]) :-
	write("- "),
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
    remove_servico(Email),
    remove_animal(Email),
    retractall(cliente(_,_,_,_)),
    remove_cliente_aux(C, Email, C_att),
    add_clientes(C_att),
    tell('./data/bd_clientes.pl'), nl,
    listing(cliente/4),
    told, nl,
    fimMetodoAdm.

remove_cliente_aux([],_,[]) :-
	nl,
	writeln("Usuário inexistente"), nl.
remove_cliente_aux([H|T], Email, T) :- member(Email, H).
remove_cliente_aux([H|T], Email, [H|Out]) :- remove_cliente_aux(T, Email, Out).

add_animais([]).
add_animais([[Nome, Email, Especie, Peso, Altura, Idade]|T]) :- 
	add_animal(Nome, Email, Especie, Peso, Altura, Idade), add_animais(T).

add_animal(Nome, Email, Especie, Peso, Altura, Idade) :- 
	assertz(animal(Nome, Email, Especie, Peso, Altura, Idade)).

list_animais(A) :- 
	findall([Nome, Email, Especie, Peso, Altura, Idade], animal(Nome, Email, Especie, Peso, Altura, Idade), A).

fimMetodoAdm:-
	writeln("Clique em enter para continuar: "),
	read_line_to_string(user_input, _).

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

add_servicos([]).
add_servicos([[Id, Nome, Email, Data, Servico, Status]|T]) :- 
	add_servico(Id, Nome, Email, Data, Servico, Status), add_servicos(T).

add_servico(Id, Nome, Email, Data, Servico, Status) :- 
	assertz(servico(Id, Nome, Email, Data, Servico, Status)).

list_servicos(S) :- 
	findall([Id, Nome, Email, Data, Servico, Status], servico(Id, Nome, Email, Data, Servico, Status), S).

remove_servico(Email) :- 
    list_servicos(S), 
    retractall(servico(_,_,_,_,_,_)),
    remove_servico_aux(S, Email, C_att),
    add_servicos(C_att),
    tell('./data/bd_servicos.pl'), nl,
    listing(servico/6),
    told.

remove_servico_aux([],_,[]).
remove_servico_aux([H|T], Email, Out) :- member(Email, H), remove_servico_aux(T, Email, Out).
remove_servico_aux([H|T], Email, [H|Out]) :- remove_servico_aux(T, Email, Out).


adicionaAnimalComNovosDados :-
	setup_bd_animal,
	tell('./data/bd_animais.pl'), nl,
	listing(animal/6),
	told.

atualizaAnimal([Nome|[EmailDoDono|[Especie|_]]], Peso, Altura, Idade) :-
	assertz(animal(Nome, EmailDoDono, Especie, Peso, Altura, Idade)),
	adicionaAnimalComNovosDados,
	writeln("Dados do animal atualizados com sucesso!").

pega_novos_dados_animal(Peso, Altura, Idade) :-
	nl,
	writeln("Insira o novo peso do animal: "),
	read_line_to_string(user_input, Peso),

	writeln("Insira a nova altura do animal: "),
	read_line_to_string(user_input, Altura),

	writeln("Insira a nova idade do animal: "),
	read_line_to_string(user_input, Idade).

edita_dados_animal_aux([],_,_,[]).
edita_dados_animal_aux([H|T], Nome, Email, Out) :- 
	member(Nome, H), 
	member(Email, H),
	edita_dados_animal_aux(T, Nome, Email, Out), 
	pega_novos_dados_animal(Peso, Altura, Idade),
	removeAnimalAux(Nome, Email),
	atualizaAnimal(H, Peso, Altura, Idade).
edita_dados_animal_aux([H|T], Nome, Email, [H|Out]) :- edita_dados_animal_aux(T, Nome, Email, Out).

editar_dados_animal_aux(Nome, Email) :-
	list_animais(A),
	retractall(animal(_,_,_,_,_,_)),
	edita_dados_animal_aux(A, Nome, Email, C_att),
	add_animais(C_att),
	tell('./data/bd_animais.pl'), nl,
	listing(animal/6),
	told.
	
editar_dados_animal :-nl,
	writeln("Insira o nome do animal a ser atualizado: "),
	read_line_to_string(user_input, Nome),
	writeln("Insira o email do dono do animal: "),
	read_line_to_string(user_input, Email),
	editar_dados_animal_aux(Nome, Email).

editar_contato_administrador :-
	setup_bd_login,
	writeln("Confirme o email do adm"),
	read_line_to_string(user_input, Email),
	writeln("Confirme a senha do adm"),
	read_line_to_string(user_input, Senha),

	(administrador(Email, Senha, _) -> nl, 
		writeln("insira o número do contato a ser atualizado."),
		read_line_to_string(user_input, Contato), nl, 
		retract(administrador(Email, Senha, _)),
		assert(administrador(Email,Senha,Contato)),	
		tell('./data/bd_adm.pl'), nl,
		listing(administrador/3),
		told, 
		tty_clear,
		writeln("Contato atualizado com sucesso."),
		writeln("Pressione qualquer tecla para retornar ao menu."),
		read_line_to_string(user_input, _),
		tty_clear;
	writeln("Senha incorreta."), nl, false).
	

