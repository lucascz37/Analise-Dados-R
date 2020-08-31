#Com outliers
funcionario = read.csv(file = 'FuncionariosTI.csv')

#Sem outliers
funcionario = read.csv(file = 'FuncionariosTI.csv')[5:162,]

#Separação Por Regiões
source('functions.R')
estados = filterRegion()

#Função pizza sobre a quantidade de funcionarios em cada região
pie(
  c(
    length(estados[estados$Regiao == 'Norte',]),
    length(estados[estados$Regiao == 'Nordeste',]),
    length(estados[estados$Regiao == 'Centro-Oeste',]),
    length(estados[estados$Regiao == 'Sudeste',]),
    length(estados[estados$Regiao == 'Sul',])
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

#Histograma sobre os salários de cada região

hist(estados[estados$Regiao == 'Norte', ]$Salario,
     xlab = 'Salários',
     col = 'purple',
     main = 'Região Norte')

hist(estados[estados$Regiao == 'Nordeste', ]$Salario,
     xlab = 'Salários',
     col = 'yellow',
     main = 'Região Nordeste')

hist(estados[estados$Regiao == 'Sudeste', ]$Salario,
     xlab = 'Salários',
     col = 'red',
     main = 'Região Sudeste')

hist(estados[estados$Regiao == 'Centro-Oeste', ]$Salario,
     xlab = 'Salários',
     col = 'lightgreen',
     main = 'Região Centro-Oeste')

hist(estados[estados$Regiao == 'Sul', ]$Salario,
     xlab = 'Salários',
     col = 'lightblue',
     main = 'Região Sul')

#boxplot sobre salário de cada região

boxplot(estados[estados$Regiao == 'Norte',]$Salario)

boxplot(estados[estados$Regiao == 'Nordeste',]$Salario)

boxplot(estados[estados$Regiao == 'Centro-Oeste',]$Salario)

boxplot(estados[estados$Regiao == 'Sudeste',]$Salario)

boxplot(estados[estados$Regiao == 'Sul',]$Salario)

#Comparação de regiões entre si
#Comparação da mediana de salários das regiões
pie(
  c(
    median(estados[estados$Regiao == 'Norte',]$Salario),
    median(estados[estados$Regiao == 'Nordeste',]$Salario),
    median(estados[estados$Regiao == 'Centro-Oeste',]$Salario),
    median(estados[estados$Regiao == 'Sudeste',]$Salario),
    median(estados[estados$Regiao == 'Sul',]$Salario)
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

#Comparação da mediana de salários das regiões de acordo com o tempo de empresa

pie(
  c(
    median(estados[estados$Regiao == 'Norte' &
                     estados$TempoDeEmpresa <= 3,]$Salario),
    median(estados[estados$Regiao == 'Nordeste' &
                     estados$TempoDeEmpresa <= 3,]$Salario),
    median(estados[estados$Regiao == 'Centro-Oeste' &
                     estados$TempoDeEmpresa <= 3,]$Salario),
    median(estados[estados$Regiao == 'Sudeste' &
                     estados$TempoDeEmpresa <= 3,]$Salario),
    median(estados[estados$Regiao == 'Sul' &
                     estados$TempoDeEmpresa <= 3,]$Salario)
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

pie(
  c(
    median(estados[estados$Regiao == 'Norte' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6,]$Salario),
    median(estados[estados$Regiao == 'Nordeste' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6,]$Salario),
    median(estados[estados$Regiao == 'Centro-Oeste' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6,]$Salario),
    median(estados[estados$Regiao == 'Sudeste' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6,]$Salario),
    median(estados[estados$Regiao == 'Sul' &
                     estados$TempoDeEmpresa > 3 &
                     estados$TempoDeEmpresa <= 6,]$Salario)
  ),
  labels = c('Norte', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Sul'),
  col = rainbow(c(16, 21, 356, 98, 123))
)

pie(c(median(estados[estados$Regiao == 'Sudeste' &
                       estados$TempoDeEmpresa > 6 &
                       estados$TempoDeEmpresa <= 9,]$Salario)),
    labels = c('Sudeste'),
    col = rainbow(c(16, 21, 356, 98, 123)))


print(density((c(2,2,2,2,3,3,5))))
