setup_bd_servico :-
	consult('./data/bd_servicos.pl').

menuServico(Email):- 
	writeln("Selecione uma das opções abaixo:"),
	writeln("1 - Agendar consulta veterinária para o animal"),
	writeln("2 - Agendar banho e tosa para o animal"),
	writeln("3 - Agendar tosa para o animal"),
	writeln("4 - Agendar banho para o animal"),
	writeln("5 - Agendar consulta, banho e tosa para o animal"),
	read_line_to_string(user_input, Option),
	(
		(Option == "1" -> (tty_clear, cadastraServico(Email, "Consulta veterinária")));
		(Option == "2" -> (tty_clear, cadastraServico(Email, "Banho e tosa")));
		(Option == "3" -> (tty_clear, cadastraServico(Email, "Tosa")));
		(Option == "4" -> (tty_clear, cadastraServico(Email, "Banho")));
		(Option == "5" -> (tty_clear, cadastraServico(Email, "Consulta veterinária, banho e tosa")));
		tty_clear, writeln("Opção inválida!"), menuServico(Email)
	).

cadastraServico(Email, Servico):- 
	setup_bd_servico,
	writeln("Nome do animal: "),
	read_line_to_string(user_input, NomeAnimal),
	writeln("Data do atendimento: "),
	read_line_to_string(user_input, Data),
	assertz(servico(NomeAnimal, Email, Data, Servico, "Pendente")),
	adicionaServico,
	writeln("Serviço cadastrado com sucesso!"),
	nl.

adicionaServico:-
	setup_bd_servico,
	tell('./data/bd_servicos.pl'), nl,
	listing(servico/5),
	told.

listarServicos(Email):-
	setup_bd_servico,
	findall(NomeAnimal, servico(NomeAnimal, Email, _, _, _), ListaNomes),
	findall(Servico, servico(_, Email, _, Servico, _), ListaServicos),
	exibirServicos(ListaNomes, ListaServicos).

listarServicosPendentes:-
	setup_bd_servico,
	findall(NomeAnimal, servico(NomeAnimal, _, _, _, "Pendente"), ListaNomes),
	findall(Servico, servico(_, _, _, Servico, "Pendente"), ListaServicos),
	exibirServicos(ListaNomes, ListaServicos).

exibirServicos([], []):-
	writeln("Nenhum serviço cadastrado."), 
	nl,
	fimListagem.

exibirServicos([N], [S]):-
	write("Nome animal: "),
	writeln(N),
	write("Serviços: "),
	writeln(S), 
	nl,
	fimListagem.

exibirServicos([N|TN], [S|TS]):-
	write("Nome animal: "),
	writeln(N),
	write("Serviços: "),
	writeln(S),
	nl,
	exibirServicos(TN, TS).

fimListagem:-
	writeln("Clique em enter para continuar: "),
	read_line_to_string(user_input, _).


