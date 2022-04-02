setup_bd_hotelzinho:-
    consult('./data/bd_hotelzinho.pl').

menuHotelzinho(Email):-
    writeln("Boas vindas ao Hotelzinho Pet!"),
    writeln("---------------------------------"),
    nl, 
    writeln("Selecione uma opção abaixo:"),
    writeln("1 - Inserir um animal no Hotelzinho"),
    writeln("2 - Exibir solicitações de hospedagem do Hotelzinho"),
    writeln("3 - Voltar ao menu principal."),
    nl, 
    read_line_to_string(user_input, Option),
    (Option == "1" -> tty_clear, inserirNoHotelzinho(Email), tty_clear, segundoMenuCliente(Email);
    Option == "2" -> tty_clear, exibirNoHotelzinho(Email), tty_clear, segundoMenuCliente(Email);
    Option == "3" -> tty_clear, segundoMenuCliente(Email);
    opcaoInvalida, menuHotelzinho(Email)).


inserirNoHotelzinho(Email) :-
    (configuracao_hotelzinho(_,"disponivel") -> (    
        writeln("Aqui estão seus animaizinhos:"),
        writeln("--------------------------------"),
        listarAnimais(Email),
        nl,
        writeln("Insira o nome do animalzinho que você deseja solicitar hospedagem no hotelzinho:"),
        read_line_to_string(user_input, NomeAnimal),
        (animal(NomeAnimal,Email,_,_,_,_) -> (
                writeln("Insira uma data para a solicitação de hospedagem: "),
                read_line_to_string(user_input, Data),
                assert(hotelzinho(NomeAnimal, Email, Data)),
                writeln("Animal hospedado com sucesso!"),
                fimListagem); 
            writeln("Não consta, para este cliente, um animal com esse nome."), nl,fimListagem),  
    
        writeln("Serviço concluido!"),
        nl,
        setup_bd_hotelzinho,
        tell('./data/bd_hotelzinho.pl'), nl,
        listing(hotelzinho/3),
        told,
        tty_clear)); 

    writeln("O hotelzinho não encontra-se disponivel no momento. Tente novamente mais tarde!"),
    fimListagem.



exibirNoHotelzinho(Email):-
    setup_bd_hotelzinho,
    hotelzinho_vazio -> writeln("Não há nenhum agendamento de Hotelzinho."),fimListagem,nl;
        writeln("Seguem abaixo as solicitações de reserva para o hotelzinho:"),
        nl, 
        listing((hotelzinho(_,Email,_))),
        nl,
        writeln("Pressione enter para continuar."),
        fimListagem.
    


hotelzinhoAdm:- 
    setup_bd_hotelzinho,
    writeln("----------------------------"),
    writeln("GERENCIAMENTO DO HOTELZINHO"),
    writeln("----------------------------"),
    nl,
    writeln("Você deseja: "),
    writeln("1 - Listar as solicitações de hospedagem."),
    writeln("2 - Excluir solicitações de um determinado animal."),
    writeln("3 - Voltar ao menu principal."),
    read_line_to_string(user_input, Option),
    (Option == "1" -> listing(hotelzinho(_,_,_)),fimListagem, hotelzinhoAdm;
    Option == "2" -> tty_clear, apagarSolicitacaoHotelzinho, hotelzinhoAdm;
    Option == "3" -> tty_clear, menuAdm;
    tty_clear, writeln("Opção inválida!"), hotelzinhoAdm).





apagarSolicitacaoHotelzinho:-
    setup_bd_hotelzinho,
    writeln("Dando como concluída uma solicitação de hospedagem..."),
    writeln("-----------------------------------------------------"),
    writeln("Insira o nome do animal desejado: "),

    read_line_to_string(user_input, NomeAnimal),

    writeln("Insira o email do dono deste animal:"),

    read_line_to_string(user_input,Email), 

    retractall(hotelzinho(NomeAnimal,Email,_)),
    writeln("Solicitações referentes a este animal foram apagadas."),
    tell('./data/bd_hotelzinho.pl'), nl,
    listing(hotelzinho/3),
    told,
    tty_clear.


hotelzinho_vazio :-
	\+(predicate_property(hotelzinho(_,_,_), dynamic)).