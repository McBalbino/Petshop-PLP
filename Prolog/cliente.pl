setup_bd :-
	consult('./data/bd_clientes.pl').

setup_bd_animal :-
	consult('./data/bd_animais.pl').

arquivo_vazio :-
	\+(predicate_property(cliente(_,_,_,_), dynamic)).

adicionaCliente :-
	setup_bd,
	tell('./data/bd_clientes.pl'), nl,
	listing(cliente/4),
	told.

cadastraCliente :-
	setup_bd,
	nl, writeln("Insira seu nome: "),
	read_line_to_string(user_input, Nome),
	nl, writeln("Insira seu email: "),
	read_line_to_string(user_input, Email),
	nl, writeln("Insira sua senha: "),
	read_line_to_string(user_input, Senha),
	nl, writeln("Insira seu telefone: "),
	read_line_to_string(user_input, Telefone),
	nl,
	(get_emails_clientes(Emails), member(Email, Emails) -> nl, writeln("Email já cadastrado."), nl;
	assertz(cliente(Nome, Email, Senha, Telefone)),
	adicionaCliente,
	writeln("Cliente cadastrado com sucesso!"),nl).

get_emails_clientes(Emails) :- 
	findall(Email, cliente(_,Email,_,_), Emails).

loginCliente(Email) :-
	nl,
	writeln("Insira seu email: "),
	read_line_to_string(user_input, Email),
	writeln("Insira sua senha: "),
	read_line_to_string(user_input, Senha),
	(cliente(_, Email, Senha, _) -> nl, writeln("Login realizado com sucesso!"), nl;
	writeln("Senha incorreta."), nl, false).

login_cliente(Email) :-
	setup_bd,
	arquivo_vazio -> writeln("Cliente não cadastrado."), nl, false;
	(cliente(_, _, _, _) -> loginCliente(Email);
	writeln("Cliente não cadastrado."), nl, false).

adicionaAnimal :-
	setup_bd_animal,
	tell('./data/bd_animais.pl'), nl,
	listing(animal/6),
	told.

cadastraAnimal(Email) :-
	setup_bd_animal,
	nl, writeln("Insira o nome do animal: "),
	read_line_to_string(user_input, Nome),
	nl, writeln("Insira a especie do animal: "),
	read_line_to_string(user_input, Especie),
	nl, writeln("Insira o peso do animal: "),
	read_line_to_string(user_input, Peso),
	nl, writeln("Insira a altura do animal: "),
	read_line_to_string(user_input, Altura),
	nl, writeln("Insira a idade do animal: "),
	read_line_to_string(user_input, Idade),
	nl,
	(get_nome_animal(A), member([Nome, Email],  A) -> nl, writeln("Animal já cadastrado."), nl;
	assertz(animal(Nome, Email, Especie, Peso, Altura, Idade)),
	adicionaAnimal,
	writeln("Animal cadastrado com sucesso!"), nl).

get_nome_animal(A) :- 
	findall([Nome,Email], animal(Nome,Email,_,_,_,_), A).

listAnimais(A) :- 
	findall([Nome, Email, Especie, Peso, Altura, Idade], animal(Nome, Email, Especie, Peso, Altura, Idade), A).

removeAnimalAux([],_,_,[]) :-
	nl,
	writeln("Animal inexistente").
removeAnimalAux([H|T], Nome, Email, Out) :- 
	member(Nome, H), 
	member(Email, H),
	removeAnimalAux(T, Nome, Email, Out), 
	writeln("Animal removido com sucesso!").
removeAnimalAux([H|T], Nome, Email, [H|Out]) :- removeAnimalAux(T, Nome, Email, Out).

addAnimal(Nome, Email, Especie, Peso, Altura, Idade) :- 
	assertz(animal(Nome, Email, Especie, Peso, Altura, Idade)).

addAnimais([]).
addAnimais([[Nome, Email, Especie, Peso, Altura, Idade]|T]) :-
	addAnimal(Nome, Email, Especie, Peso, Altura, Idade), addAnimais(T).

removeAnimalAux(Nome, Email) :- 
	listAnimais(A), 
	retractall(animal(_,_,_,_,_,_)),
	removeAnimalAux(A, Nome, Email, C_att),
	addAnimais(C_att),
	tell('./data/bd_animais.pl'), nl,
	listing(animal/6),
	told.

removeAnimal(Email) :-
	nl,
	writeln("Insira nome do animal a ser removido: "),
	read_line_to_string(user_input, Nome),
	removeAnimalAux(Nome, Email).
	