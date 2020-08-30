#Com outliers
funcionario = read.csv(file = 'FuncionariosTI.csv')

#Sem outliers
funcionario = read.csv(file = 'FuncionariosTI.csv')[5:162, ]

#Separação Por Regiões
estados = data.frame(Regiao = c(),
                     Salário = c(),
                     TempoDeEmpresa = c())

funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                               c('AM', 'AC', 'RO', 'RR', 'PA', 'AP', 'TO'),]
estados = rbind(estados,
                data.frame(
                  Regiao = c(replicate(nrow(funcionarioSEP), 'Norte')),
                  Salário = c(funcionarioSEP$Salario.Bruto),
                  TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa)
                ))

funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                               c('PB', 'RN', 'PE', 'AL', 'PI', 'BA', 'SE', 'MA', 'CE'), ]

estados = rbind(estados,
                data.frame(
                  Regiao = c(replicate(nrow(funcionarioSEP), 'Nordeste')),
                  Salário = c(funcionarioSEP$Salario.Bruto),
                  TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa)
                ))

funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                               c('MT' , 'GO', 'MS', 'DF'), ]

estados = rbind(estados,
                data.frame(
                  Regiao = c(replicate(nrow(funcionarioSEP), 'Centro-Oeste')),
                  Salário = c(funcionarioSEP$Salario.Bruto),
                  TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa)
                ))

funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                               c('MG', 'SP', 'RJ', 'ES'), ]

estados = rbind(estados,
                data.frame(
                  Regiao = c(replicate(nrow(funcionarioSEP), 'Sudeste')),
                  Salário = c(funcionarioSEP$Salario.Bruto),
                  TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa)
                ))

funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                               c('RS' , 'SC', 'PR'), ]

estados = rbind(estados,
                data.frame(
                  Regiao = c(replicate(nrow(funcionarioSEP), 'Sul')),
                  Salário = c(funcionarioSEP$Salario.Bruto),
                  TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa)
                ))
#Função pizza sobre a quantidade de funcionarios em cada região
pie(
  c(
    length(estados[estados$Regiao == 'Norte', ]),
    length(estados[estados$Regiao == 'Nordeste', ]),
    length(estados[estados$Regiao == 'Centro-Oeste', ]),
    length(estados[estados$Regiao == 'Sudeste', ]),
    length(estados[estados$Regiao == 'Sul', ])
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

#Histograma sobre os salários de cada região

hist(estados[estados$Regiao == 'Norte',]$Salário,
     xlab = 'Salários',
     col = 'purple',
     main = 'Região Norte')

hist(estados[estados$Regiao == 'Nordeste',]$Salário,
     xlab = 'Salários',
     col = 'yellow',
     main = 'Região Nordeste')

hist(estados[estados$Regiao == 'Sudeste',]$Salário,
     xlab = 'Salários',
     col = 'red',
     main = 'Região Sudeste')

hist(estados[estados$Regiao == 'Centro-Oeste',]$Salário,
     xlab = 'Salários',
     col = 'lightgreen',
     main = 'Região Centro-Oeste')

hist(estados[estados$Regiao == 'Sul',]$Salário,
     xlab = 'Salários',
     col = 'lightblue',
     main = 'Região Sul')

#boxplot sobre salário de cada região

boxplot(estados[estados$Regiao == 'Norte', ]$Salário)

boxplot(estados[estados$Regiao == 'Nordeste', ]$Salário)

boxplot(estados[estados$Regiao == 'Centro-Oeste', ]$Salário)

boxplot(estados[estados$Regiao == 'Sudeste', ]$Salário)

boxplot(estados[estados$Regiao == 'Sul', ]$Salário)

#Comparação de regiões entre si
#Comparação da mediana de salários das regiões
pie(
  c(
    median(estados[estados$Regiao == 'Norte', ]$Salário),
    median(estados[estados$Regiao == 'Nordeste', ]$Salário),
    median(estados[estados$Regiao == 'Centro-Oeste', ]$Salário),
    median(estados[estados$Regiao == 'Sudeste', ]$Salário),
    median(estados[estados$Regiao == 'Sul', ]$Salário)
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

#Comparação da mediana de salários das regiões de acordo com o tempo de empresa

pie(
  c(
    median(estados[estados$Regiao == 'Norte' &
                     estados$TempoDeEmpresa <= 3, ]$Salário),
    median(estados[estados$Regiao == 'Nordeste' &
                     estados$TempoDeEmpresa <= 3, ]$Salário),
    median(estados[estados$Regiao == 'Centro-Oeste' &
                     estados$TempoDeEmpresa <= 3, ]$Salário),
    median(estados[estados$Regiao == 'Sudeste' &
                     estados$TempoDeEmpresa <= 3, ]$Salário),
    median(estados[estados$Regiao == 'Sul' &
                     estados$TempoDeEmpresa <= 3, ]$Salário)
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

pie(
  c(
    median(estados[estados$Regiao == 'Norte' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6, ]$Salário),
    median(estados[estados$Regiao == 'Nordeste' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6, ]$Salário),
    median(estados[estados$Regiao == 'Centro-Oeste' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6, ]$Salário),
    median(estados[estados$Regiao == 'Sudeste' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6, ]$Salário),
    median(estados[estados$Regiao == 'Sul' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6, ]$Salário)
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

pie(
  c(
    median(estados[estados$Regiao == 'Sudeste' &
                     estados$TempoDeEmpresa > 6 &
                     estados$TempoDeEmpresa <= 9, ]$Salário)
  ),
  labels = c('Sudeste'),
  col = rainbow(c(16, 21, 356, 98, 123))
)
