

data Animal = Animal{
    nome:: String,
    especie:: String,
    raca:: String,
    peso:: Int,
    altura:: Float,
    idade:: Int,
    saude:: Int,
    agendamentos:: [Agendamento],
    historicoDeServicos:: [Historico] 
}