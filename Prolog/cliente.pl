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
	assertz(cliente(Nome, Email, Senha, Telefone)),
	adicionaCliente,
	writeln("Cliente cadastrado com sucesso!"),nl.

loginCliente(Email) :-
	nl,
	writeln("Insira seu email: "),
	read_line_to_string(user_input, Email),
	writeln("Insira sua senha: "),
	read_line_to_string(user_input, Senha),
	(cliente(_, Email, Senha, _) -> nl, writeln("Login realizado com sucesso!");
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
	assertz(animal(Nome, Email, Especie, Peso, Altura, Idade)),
	adicionaAnimal,
	writeln("Animal cadastrado com sucesso!").

