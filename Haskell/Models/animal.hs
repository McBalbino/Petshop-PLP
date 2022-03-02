

data Animal = Animal{
    nome:: String,
    especie:: String,
    peso:: Int,
    altura:: Float,
    idade:: Int,
    saude:: Int,
    agendamentos:: [Agendamento],
    historicoDeServicos:: [Historico] 
}