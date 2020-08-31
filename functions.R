filterRegion = function() {
  estados = data.frame(
    Regiao = c(),
    Salario = c(),
    TempoDeEmpresa = c(),
    ExperienciaProfissional = c()
  )
  
  funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                                 c('AM', 'AC', 'RO', 'RR', 'PA', 'AP', 'TO'), ]
  estados = rbind(
    estados,
    data.frame(
      Regiao = c(replicate(nrow(funcionarioSEP), 'Norte')),
      Salario = c(funcionarioSEP$Salario.Bruto),
      TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa),
      ExperienciaProfissional = c(funcionarioSEP$Experiencia.Profissional)
    )
  )
  
  funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                                 c('PB', 'RN', 'PE', 'AL', 'PI', 'BA', 'SE', 'MA', 'CE'),]
  
  estados = rbind(
    estados,
    data.frame(
      Regiao = c(replicate(nrow(funcionarioSEP), 'Nordeste')),
      Salario = c(funcionarioSEP$Salario.Bruto),
      TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa),
      ExperienciaProfissional = c(funcionarioSEP$Experiencia.Profissional)
    )
  )
  
  funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                                 c('MT' , 'GO', 'MS', 'DF'),]
  
  estados = rbind(
    estados,
    data.frame(
      Regiao = c(replicate(nrow(funcionarioSEP), 'Centro-Oeste')),
      Salario = c(funcionarioSEP$Salario.Bruto),
      TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa),
      ExperienciaProfissional = c(funcionarioSEP$Experiencia.Profissional)
    )
  )
  
  funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                                 c('MG', 'SP', 'RJ', 'ES'),]
  
  estados = rbind(
    estados,
    data.frame(
      Regiao = c(replicate(nrow(funcionarioSEP), 'Sudeste')),
      Salario = c(funcionarioSEP$Salario.Bruto),
      TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa),
      ExperienciaProfissional = c(funcionarioSEP$Experiencia.Profissional)
    )
  )
  
  funcionarioSEP = funcionario[toupper(funcionario$UF) %in%
                                 c('RS' , 'SC', 'PR'),]
  
  estados = rbind(
    estados,
    data.frame(
      Regiao = c(replicate(nrow(funcionarioSEP), 'Sul')),
      Salario = c(funcionarioSEP$Salario.Bruto),
      TempoDeEmpresa = c(funcionarioSEP$Tempo.de.Empresa),
      ExperienciaProfissional = c(funcionarioSEP$Experiencia.Profissional)
    )
  )
  
  return(estados)
}
