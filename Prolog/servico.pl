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
	findall(_, servico(_,_, _, _, _, _), LS),
	length(LS, Tamanho),
	Id is Tamanho+1,
	assertz(servico(Id, NomeAnimal, Email, Data, Servico, "Pendente")),
	adicionaServico,
	writeln("Serviço cadastrado com sucesso!"),
	nl.

adicionaServico:-
	setup_bd_servico,
	tell('./data/bd_servicos.pl'), nl,
	listing(servico/6),
	told.

fimMetodoServico:-
	writeln("Clique em enter para continuar: "),
	read_line_to_string(user_input, _).

marcarServicoConcluido :- 
	setup_bd_servico,
	writeln("Informe o id do serviço a ser marcado como conluido: "),
	read_line_to_string(user_input, IdStr),
	number_codes(Id, IdStr),
	((servico(Id,_,_,_,_,"Concluido") -> nl, writeln("Servico já concluido!");
	retract(servico(Id, NomeAnimal, Email, Data, Servico, "Pendente")),
	assertz(servico(Id, NomeAnimal, Email, Data, Servico, "Concluido")),
	tell('./data/bd_servicos.pl'),
	listing(servico/6),
	told);
	writeln("Servico não cadastrado"), nl),
	fimMetodoServico.


listarServicosConcluidosDoCliente(Email):-
	setup_bd_servico,
	findall(NomeAnimal, servico(_,NomeAnimal, Email, _, _, "Concluido"), ListaNomes),
	findall(Servico, servico(_,_, Email, _, Servico, "Concluido"), ListaServicos),
	exibirServicos(ListaNomes, ListaServicos).

listarServicosPendentesDoCliente(Email):-
	setup_bd_servico,
	findall(NomeAnimal, servico(_,NomeAnimal, Email, _, _, "Pendente"), ListaNomes),
	findall(Servico, servico(_,_, Email, _, Servico, "Pendente"), ListaServicos),
	exibirServicos(ListaNomes, ListaServicos).

listarServicosPendentes:-
	setup_bd_servico,
	findall(NomeAnimal, servico(_,NomeAnimal, _, _, _, "Pendente"), ListaNomes),
	findall(Servico, servico(_,_, _, _, Servico, "Pendente"), ListaServicos),
	exibirServicos(ListaNomes, ListaServicos).


exibe_servicos([]).

exibe_servicos([H]) :-
	writeln(H).

exibe_servicos([H|T]) :-
	writeln(H),
	exibe_servicos(T).

exibirServicos([], []):-
	writeln("Nenhum serviço disponível."), 
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


