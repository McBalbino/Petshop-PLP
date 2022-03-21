:- module('cliente', [cadastraCliente/0]).
:- use_module(library(apply)).
:- use_module(library(csv)).

setup_bd :-
	consult('./data/bd_clientes.pl').

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
