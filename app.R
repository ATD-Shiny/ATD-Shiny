################################################################################
# Trabalho Prático: Shiny                                                      #
# Disciplina: ATD [MEGAF]                                                      #
# Docente: Nuno Lavado - nlavado@isec.pt                                       #
#                                                                              #
# Grupo:  João Gonçalves  [2020149226]  - a2020149226@isec.pt                  #
#         Luís Pato       [2005009772]  - a21150211@isec.pt                    #
#         Samuel Martinho [2006005673]  - a21170106@isec.pt                    #
#                                                                              #
# Data:   2020/11/21      Data última revisão:  2020/11/25 - LP                #
#                                               2021/01/10 - JG, LP, SM        #
#                                               2021/01/13 - JG, LP, SM        #
#                                                                              #
################################################################################

library(shiny)

############# MAD ; GE ; GF ########
library(shinyjs)

############# GF ###################
library(shinyFiles)
library(gdata)

############# LOG ##################
library(leaflet)
library(dplyr)



####################################
### Variáveis                   ####
####################################

############# MAD ##################
# Data Frame dos Dados da Rede
dfDados <<- data.frame()   


############# GE ###################
meses = c(1,2,3,4,5,6,7,8,9,10,11,12)
names(meses) = c("Janeiro",
                 "Fevereiro",
                 "Março",
                 "Abril",
                 "Maio",
                 "Junho",
                 "Julho",
                 "Agosto",
                 "Setembro",
                 "Outubro",
                 "Novembro",
                 "Dezembro")


############# LOG ##################
city_info = data.frame()


############# GF ###################
df = data.frame()



####################################
### Funções                     ####
####################################

############# MAD ##################
# Ler os números delimitados por vírgulas das InputBox 
ler_nr <- function(tI_nr, df_rw) {
  
  lnr <- matrix(as.numeric(unlist(strsplit(tI_nr,","))),
                1,
                length(unlist(strsplit(tI_nr,","))),
                dimnames = list(df_rw,1:length(unlist(strsplit(tI_nr,","))))
                )

  return(lnr)
}


# Aumenta as colunas de uma linha para o número máximo de colunas da tabela
aum_col <- function(ncolMax, tI_colMin) {
  
  if (ncolMax == ncol(tI_colMin))
    acl <- tI_colMin
  else{
    aux <- cbind(tI_colMin,
                 matrix(t(vector(mode = "integer", ncolMax-ncol(tI_colMin))),
                        nrow = 1,
                        dimnames = (list(nrow(tI_colMin),(ncol(tI_colMin)+1):ncolMax))
                        )
                 )
    
    row.names(aux) <- c(row.names(tI_colMin))
    
    acl <- aux
  }
  
  return(acl)
  
}


# Adiciona linha na tabela
adc_lin <- function(nr_lin, txtImp) {
  

  return(aln)
}


############# GE ###################
#Termo tarifário fixo (EUR/mês)
trmTrfFx <- function(nvTnsEl){
  switch(nvTnsEl,
         "1" = {
           # BTN
           0.00
         },
         "2" = {
           # BTE
           23.47
         },
         "3" = {
           # MT
           44.80
         },
         "4" = {
           # AT
           74.24
         },
         {
           # MAT
           0.00
         }
  )
}


#Número de dias por mês (dias)
nrDsMes <- function(mes){
  ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
  ###!!!! FALTA: calcular os dias de fim-de-semana !!!#####
  ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
  switch(mes,
         "1" = {
           # jan
           31
         },
         "2" = {
           # fev
           28
         },
         "3" = {
           # mar
           31
         },
         "4" = {
           # abr
           30
         },
         "5" = {
           # mai
           31
         },
         "6" = {
           # jun
           30
         },
         "7" = {
           # jul
           31
         },
         "8" = {
           # ago
           31
         },
         "9" = {
           # set
           30
         },
         "10" = {
           # out
           31
         },
         "11" = {
           # nov
           30
         },
         "12" = {
           # dez
           31
         }
  )
}


#Hora Legal: Verão = 0 ; Inverno = 1
hrLgl <- function(mes){
  
  mes <- as.numeric(mes)
  
  if (mes >= 4 & mes <= 10)
    return(0)
  else
    return(1)
  
}


#Número de Horas de ponta diárias (horas)
nrHPD <- function(prHr, mes, nvTnsEl){
  # nvTnsEl: 1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT
  # prHr   : Períodos Horários : 1-Ciclo Semanal          [nvTnsEl: 1 ; 2 ; 3 ; 4 ; 5]
  #                              2-Ciclo Semanal Opcional [nvTnsEl: 3 ; 4 ; 5]
  #                              3-Ciclo Diário           [nvTnsEl: 1 ; 2]
  
  prHr <- as.numeric(prHr)
  hL <- as.numeric(hrLgl(mes))
  nvTnsEl <- as.numeric(nvTnsEl)
  
  if (prHr == 1 & hL == 0)
    return(3)
  else if (prHr == 1 & hL == 1)
    return(5)
  else if (prHr == 2 & hL == 0 & nvTnsEl >= 3)
    return(3)
  else if (prHr == 2 & hL == 1 & nvTnsEl >= 3)
    return(5)
  else if (prHr == 3 & hL == 0 & nvTnsEl <= 2)
    return(4)
  else if (prHr == 3 & hL == 1 & nvTnsEl <= 2)
    return(4)
  
}

#Potência Horas de ponta (kW/mês)
ptHP <- function(enAtHP, prHr, mes, nvTnsEl){
  # enAtHP  - Energia Ativa de Horas de Ponta
  # prHr    - Períodos Horários : 1-Ciclo Semanal ; 
  #                               2-Ciclo Semanal Opcional ; 
  #                               3-Ciclo Diário
  # mes     - Mês da fatura
  # nvTnsEl - Nível Tensão Elétrica
  
  ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
  ###!!!! FALTA: calcular os dias de fim-de-semana !!!#####
  ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
  
  prHr <- as.numeric(prHr)
  enAtHP <- as.numeric(enAtHP)
  
  if(prHr != 3){
    #Semana: de 2ª feira a 6ª feira
    diasCons <- (as.numeric(nrDsMes(mes)) - 4 * 2)
  }
  else{
    #Semana: de 2ª feira a dom
    diasCons <- as.numeric(nrDsMes(mes))
  }
  
  vlr <- (enAtHP / (as.numeric(nrHPD(prHr, mes, nvTnsEl)) * diasCons))
  
  return(vlr)
  
}


#Tarifa da Potência (EUR/kW.mês)
trPtnc <- function(nvTnsEl, opTrf, tipo){
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  # tipo    : H - Horas de ponta ; C - Contratada
  
  nvTnsEl <- as.numeric(nvTnsEl)
  opTrf <- as.numeric(opTrf)
  
  if(nvTnsEl == 1){
    #!!!!! FALTA TRATAR !!!!!
    #1-BTN
    return(1)
  }
  else if(nvTnsEl == 2){
    #2-BTE
    #Tarifa de longas utilizações ; Horas de ponta
    if(opTrf == 1 & tipo == "H")
      vlr <- 20.758
    #Tarifa de longas utilizações ; Contratada
    else if(opTrf == 1 & tipo == "C")
      vlr <- 1.503
    #Tarifa de médias utilizações ; Horas de ponta
    else if(opTrf == 2 & tipo == "H")
      vlr <- 15.419
    #Tarifa de médias utilizações ; Contratada
    else if(opTrf == 2 & tipo == "C")
      vlr <- 0.708
  }
  else if(nvTnsEl == 3){
    #3-MT
    #Tarifa de longas utilizações ; Horas de ponta
    if(opTrf == 1 & tipo == "H")
      vlr <- 9.920
    #Tarifa de longas utilizações ; Contratada
    else if(opTrf == 1 & tipo == "C")
      vlr <- 1.522
    #Tarifa de médias utilizações ; Horas de ponta
    else if(opTrf == 2 & tipo == "H")
      vlr <- 9.994
    #Tarifa de médias utilizações ; Contratada
    else if(opTrf == 2 & tipo == "C")
      vlr <- 1.437
    #Tarifa de curtas utilizações ; Horas de ponta
    else if(opTrf == 3 & tipo == "H")
      vlr <- 14.492
    #Tarifa de curtas utilizações ; Contratada
    else if(opTrf == 3 & tipo == "C")
      vlr <- 0.654
  }
  else if(nvTnsEl == 4){
    #4-AT
    #Tarifa de longas utilizações ; Horas de ponta
    if(opTrf == 1 & tipo == "H")
      vlr <- 6.413
    #Tarifa de longas utilizações ; Contratada
    else if(opTrf == 1 & tipo == "C")
      vlr <- 0.876
    #Tarifa de médias utilizações ; Horas de ponta
    else if(opTrf == 2 & tipo == "H")
      vlr <- 6.215
    #Tarifa de médias utilizações ; Contratada
    else if(opTrf == 2 & tipo == "C")
      vlr <- 0.724
    #Tarifa de curtas utilizações ; Horas de ponta
    else if(opTrf == 3 & tipo == "H")
      vlr <- 12.587
    #Tarifa de curtas utilizações ; Contratada
    else if(opTrf == 3 & tipo == "C")
      vlr <- 0.533
  }
  else if(nvTnsEl == 5){
    #5-MAT
    #Tarifa de todas as utilizações ; Horas de ponta
    if(opTrf == 1 & tipo == "H")
      vlr <- 1.331
    #Tarifa de todas as utilizações ; Contratada
    else if(opTrf == 1 & tipo == "C")
      vlr <- 0.702
  }
  
  return(vlr)
  
}


#Encargos com a Potência Horas de ponta (EUR/mês)
encPHP <- function(enAtHP, prHr, mes, nvTnsEl, opTrf){
  # enAtHP  : Energia Ativa de Horas de Ponta
  # prHr    : Períodos Horários : 1-Ciclo Semanal ; 
  #                               2-Ciclo Semanal Opcional ; 
  #                               3-Ciclo Diário
  # mes     : Mês da fatura
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  
  vlr <- (as.numeric(ptHP(enAtHP, prHr, mes, nvTnsEl)) * as.numeric(trPtnc(nvTnsEl, opTrf, "H")))
  
  return(vlr)
  
}


#Encargos com a Potência Contratada (EUR/mês)
encPC <- function(potC, nvTnsEl, opTrf){
  # potC    : Potência Contratada
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  
  vlr <- (as.numeric(potC) * as.numeric(trPtnc(nvTnsEl, opTrf, "C")))
  
  return(vlr)
  
}


#Encargos com a Potência (EUR/mês)
encPot <- function(potC, nvTnsEl, opTrf, enAtHP, prHr, mes){
  # encPC(potC, nvTnsEl, opTrf)               : funtion Encargos com a Potência Contratada (EUR/mês)
  # encPHP(enAtHP, prHr, mes, nvTnsEl, opTrf) : funtion Encargos com a Potência Horas de ponta (EUR/mês)
  #
  # potC    : Potência Contratada
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  # enAtHP  : Energia Ativa de Horas de Ponta
  # prHr    : Períodos Horários : 1-Ciclo Semanal ; 
  #                               2-Ciclo Semanal Opcional ; 
  #                               3-Ciclo Diário
  # mes     : Mês da fatura
  
  vlr <- (as.numeric(encPC(potC, nvTnsEl, opTrf)) + as.numeric(encPHP(enAtHP, prHr, mes, nvTnsEl, opTrf)))
  
  return(vlr)
  
}


#Tarifa da Energia Ativa (EUR/kWh)
trEA <- function(nvTnsEl, opTrf, prTrf, tipo){
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  # prTrf   : Período Tarifário : 1 - Período I   [jan ; fev ; mar]
  #                               2 - Período II  [abr ; mai ; jun]
  #                               3 - Período III [jul ; ago ; set]
  #                               4 - Período IV  [out ; nov ; dez]
  # tipo    : HP  - Horas de ponta
  #           HC  - Horas cheias
  #           HVN - Horas de vazio normal
  #           HSV  - Horas de super vazio
  
  nvTnsEl <- as.numeric(nvTnsEl)
  opTrf <- as.numeric(opTrf)
  prTrf <- as.numeric(prTrf)
  
  if(nvTnsEl == 1){
    #!!!!! FALTA TRATAR !!!!!
    #1-BTN
    return(1)
  }
  else if(nvTnsEl == 2){
    #2-BTE
    #Tarifa de longas utilizações ; Período I e Período IV
    if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.1638
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1305
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0864
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0751
    #Tarifa de longas utilizações ; Período II e Período III
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.1622
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1305
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.085
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0764
    #Tarifa de médias utilizações ; Período I e Período IV
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.2142
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1351
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0905
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0792
    #Tarifa de médias utilizações ; Período II e Período III
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.2134
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1322
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0901
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0797
  }
  else if(nvTnsEl == 3){
    #3-MT
    #Tarifa de longas utilizações ; Período I e Período IV
    if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.1390
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1115
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0773
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0674
    #Tarifa de longas utilizações ; Período II e Período III
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.1400
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1119
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0786
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0725
    #Tarifa de médias utilizações ; Período I e Período IV
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.1434
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1150
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0779
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0676
    #Tarifa de médias utilizações ; Período II e Período III
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.1484
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1127
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0809
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0725
    #Tarifa de curtas utilizações ; Período I e Período IV
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.2100
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1198
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0812
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0724
    #Tarifa de curtas utilizações ; Período II e Período III
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.2092
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1194
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0815
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0760
  }
  else if(nvTnsEl == 4){
    #4-AT
    #Tarifa de longas utilizações ; Período I e Período IV
    if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.1214
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.0998
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0754
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0646
    #Tarifa de longas utilizações ; Período II e Período III
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.1204
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1004
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0766
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0708
    #Tarifa de médias utilizações ; Período I e Período IV
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.1338
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1015
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0754
    else if(opTrf == 2 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0661
    #Tarifa de médias utilizações ; Período II e Período III
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.1348
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1032
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0784
    else if(opTrf == 2 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0708
    #Tarifa de curtas utilizações ; Período I e Período IV
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.1563
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.1143
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0756
    else if(opTrf == 3 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0675
    #Tarifa de curtas utilizações ; Período II e Período III
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.1557
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.1140
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0784
    else if(opTrf == 3 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0714
  }
  else if(nvTnsEl == 5){
    #5-MAT
    #Tarifa de todas as utilizações ; Período I e Período IV
    if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HP")
      vlr <- 0.0298
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HC")
      vlr <- 0.0223
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HVN")
      vlr <- 0.0133
    else if(opTrf == 1 & (prTrf == 1 | prTrf == 4) & tipo == "HSV")
      vlr <- 0.0133
    #Tarifa de todas as utilizações ; Período II e Período III
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HP")
      vlr <- 0.0297
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HC")
      vlr <- 0.0223
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HVN")
      vlr <- 0.0133
    else if(opTrf == 1 & (prTrf == 2 | prTrf == 3) & tipo == "HSV")
      vlr <- 0.0133
  }
  
  return(vlr)
  
}


#Encargos por tipo de Energia Ativa (EUR/mês)
encEA <- function(nvTnsEl, opTrf, prTrf, tipo, enAt){
  # trEA(nvTnsEl, opTrf, prTrf, tipo) : funtion Tarifa da Energia Ativa (EUR/kWh)
  #
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  # prTrf   : Período Tarifário : 1 - Período I   [jan ; fev ; mar]
  #                               2 - Período II  [abr ; mai ; jun]
  #                               3 - Período III [jul ; ago ; set]
  #                               4 - Período IV  [out ; nov ; dez]
  # tipo    : HP  - Horas de ponta
  #           HC  - Horas cheias
  #           HVN - Horas de vazio normal
  #           HSV  - Horas de super vazio
  # enAt    : Energia Ativa
  
  vlr <- (as.numeric(trEA(nvTnsEl, opTrf, prTrf, tipo)) * as.numeric(enAt))
  
  return(vlr)
  
}


#Encargos Totais da Energia Ativa (EUR/mês)
encTEA <- function(nvTnsEl, opTrf, prTrf, enAtHP, enAtHC, enAtHVN, enAtHSV){
  # encEA(nvTnsEl, opTrf, prTrf, tipo, enAt) : funtion Encargos por tipo de Energia Ativa (EUR/mês)
  #
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  # prTrf   : Período Tarifário : 1 - Período I   [jan ; fev ; mar]
  #                               2 - Período II  [abr ; mai ; jun]
  #                               3 - Período III [jul ; ago ; set]
  #                               4 - Período IV  [out ; nov ; dez]
  # enAtHP  : Energia Ativa Horas de ponta
  # enAtHC  : Energia Ativa Horas cheias
  # enAtHVN : Energia Ativa Horas de vazio normal
  # enAtHSV : Energia Ativa Horas de super vazio
  #
  # tipo    : HP  - Horas de ponta
  #           HC  - Horas cheias
  #           HVN - Horas de vazio normal
  #           HSV - Horas de super vazio
  
  
  vlr <- (as.numeric(encEA(nvTnsEl, opTrf, prTrf, "HP", enAtHP))   + 
            as.numeric(encEA(nvTnsEl, opTrf, prTrf, "HC", enAtHC))   + 
            as.numeric(encEA(nvTnsEl, opTrf, prTrf, "HVN", enAtHVN)) + 
            as.numeric(encEA(nvTnsEl, opTrf, prTrf, "HSV", enAtHSV))
  )
  
  return(vlr)
  
}


#Energia Reativa Indutiva (kVArh)
enrRI <- function(enRI, enAtHP, enAtHC){
  # enRI : Energia Reativa Indutiva
  # Energia Ativa Fora das Horas de Vazio (EAFHV) : enAtHP - Energia Ativa Horas de ponta
  #                                                 enAtHC - Energia Ativa Horas cheias
  
  
  #Energia Ativa Fora das Horas de Vazio
  enAFHV <- (enAtHP + enAtHC)
  
  #tg φ
  tgFi <- (enRI / enAFHV)
  
  if ( tgFi < 0.3)
    # (30% EAFHV) * 0
    qtd <- 0
  else if ( tgFi >= 0.3 & tgFi < 0.4){
    # (40% EAFHV : 30% EAFHV) * 0.33
    qtd <- ((enAFHV * (tgFi - 0.3)) * 0.33)
  }
  else if ( tgFi >= 0.4 & tgFi < 0.5){
    # (40% EAFHV : 30% EAFHV) * 0.33
    qtd <- ((enAFHV * 0.1) * 0.33)
    # (50% EAFHV : 40% EAFHV) * 1
    qtd <- (qtd + ((enAFHV * (tgFi - 0.4)) * 1))
  }
  else if ( tgFi >= 0.5){
    # (40% EAFHV : 30% EAFHV) * 0.33
    qtd <- ((enAFHV * 0.1) * 0.33)
    # (50% EAFHV : 40% EAFHV) * 1
    qtd <- (qtd + ((enAFHV * 0.1) * 1))
    # (enRI - 50% EAFHV) * 3
    qtd <- (qtd + ((enAFHV * (tgFi - 0.5)) * 3))
  }
  
  return(qtd)
  
}


#Tarifa da Energia Reativa (EUR/kvarh)
trER <- function(nvTnsEl, tipo){
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # tipo    : I - Indutiva ; C - Capacitiva
  
  nvTnsEl <- as.numeric(nvTnsEl)
  
  if(nvTnsEl == 1){
    #1-BTN
    vlr <- 0.0
  }
  else if(nvTnsEl == 2){
    #2-BTE
    #Indutiva
    if(tipo == "I")
      vlr <- 0.0300
    #Capacitiva
    else if(tipo == "C")
      vlr <- 0.0228
  }
  else if(nvTnsEl == 3){
    #3-MT
    #Indutiva
    if(tipo == "I")
      vlr <- 0.0252
    #Capacitiva
    else if(tipo == "C")
      vlr <- 0.0189
  }
  else if(nvTnsEl == 4){
    #4-AT
    #Indutiva
    if(tipo == "I")
      vlr <- 0.0231
    #Capacitiva
    else if(tipo == "C")
      vlr <- 0.0173
  }
  else if(nvTnsEl == 5){
    #!!!!! FALTA TRATAR !!!!!
    #5-MAT
    vlr <- 0.0
  }
  
  return(vlr)
  
}


#Encargos com a Energia Reativa Indutiva (EUR/mês)
encERI <- function(enRI, enAtHP, enAtHC, nvTnsEl){
  # enrRI(enRI, enAtHP, enAtHC) : function Energia Reativa Indutiva (kVArh)
  # trER(nvTnsEl, tipo)          : function Tarifa da Energia Reativa (EUR/kvarh)
  #
  # enRI : Energia Reativa Indutiva
  # Energia Ativa Fora das Horas de Vazio (EAFHV) : enAtHP - Energia Ativa Horas de ponta
  #                                                 enAtHC - Energia Ativa Horas cheias
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  
  
  # tipo    : I - Indutiva ; C - Capacitiva
  vlr <- (as.numeric(enrRI(enRI, enAtHP, enAtHC)) * as.numeric(trER(nvTnsEl, "I")))
  
  return(vlr);
  
}


#Encargos com a Energia Reativa Capacitiva (EUR/mês)
encERC <- function(enRC, nvTnsEl){
  # trER(nvTnsEl, tipo)          : function Tarifa da Energia Reativa (EUR/kvarh)
  #
  # enRC : Energia Reativa Capacitiva
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  
  
  # tipo    : I - Indutiva ; C - Capacitiva
  vlr <- (as.numeric(enRC) * as.numeric(trER(nvTnsEl, "C")))
  
  return(vlr)
  
}


#Encargos com a Energia Reativa (EUR/mês)
encER <- function(enRI, enAtHP, enAtHC, nvTnsEl, enRC){
  # encERI(enRI, enAtHP, enAtHC, nvTnsEl, tipo) : function Encargos com a Energia Reativa Indutiva (EUR/mês)
  # encERC(enRC, nvTnsEl) : function Encargos com a Energia Reativa Capacitiva (EUR/mês)
  #
  # enRI : Energia Reativa Indutiva
  # Energia Ativa Fora das Horas de Vazio (EAFHV) : enAtHP - Energia Ativa Horas de ponta
  #                                                 enAtHC - Energia Ativa Horas cheias
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # enRC : Energia Reativa Capacitiva
  
  vlr <- (as.numeric(encERI(enRI, enAtHP, enAtHC, nvTnsEl)) + as.numeric(encERC(enRC, nvTnsEl)))
  
  return(vlr)
  
}


#Calcular fatura mensal (EUR/mês)
calcFat <- function(nvTnsEl, potC, opTrf, enAtHP, enAtHC, enAtHVN, enAtHSV, prHr, mes, prTrf, enRI, enRC){
  # trmTrfFx(nvTnsEl)                                               : function Termo tarifário fixo (EUR/mês)
  # encPot(potC, nvTnsEl, opTrf, enAtHP, prHr, mes)                 : function Encargos com a Potência (EUR/mês)
  # encTEA(nvTnsEl, opTrf, prTrf, enAtHP, enAtHC, enAtHVN, enAtHSV) : function Encargos Totais da Energia Ativa (EUR/mês)
  # encER(enRI, enAtHP, enAtHC, nvTnsEl, enRC)                      : function Encargos com a Energia Reativa (EUR/mês)
  #
  # nvTnsEl : Nível Tensão Elétrica [1-BTN ; 2-BTE ; 3-MT ; 4-AT ; 5-MAT]
  # potC    : Potência Contratada
  # opTrf   : 2-BTE:       Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #           3-MT ; 4-AT: Opção Tarifária : 1 - Tarifa de longas utilizações
  #                                          2 - Tarifa de médias utilizações
  #                                          3 - Tarifa de curtas utilizações
  #           5-MAT:       Opção Tarifária : 1 - S/Tarifa por utilizações
  # enAtHP  : Energia Ativa Horas de ponta
  # enAtHC  : Energia Ativa Horas cheias
  # enAtHVN : Energia Ativa Horas de vazio normal
  # enAtHSV : Energia Ativa Horas de super vazio
  # prHr    : Períodos Horários : 1-Ciclo Semanal ; 
  #                               2-Ciclo Semanal Opcional ; 
  #                               3-Ciclo Diário
  # mes     : Mês da fatura
  # prTrf   : Período Tarifário : 1 - Período I   [jan ; fev ; mar]
  #                               2 - Período II  [abr ; mai ; jun]
  #                               3 - Período III [jul ; ago ; set]
  #                               4 - Período IV  [out ; nov ; dez]
  # enRI : Energia Reativa Indutiva
  # enRC : Energia Reativa Capacitiva
  
  txIVA = 23
  
  
  vlr <- (as.numeric(trmTrfFx(nvTnsEl)) + 
            as.numeric(encPot(potC, nvTnsEl, opTrf, enAtHP, prHr, mes)) +
            as.numeric(encTEA(nvTnsEl, opTrf, prTrf, enAtHP, enAtHC, enAtHVN, enAtHSV)) +
            as.numeric(encER(enRI, enAtHP, enAtHC, nvTnsEl, enRC))
  )
  
  vlrIVA <- (vlr * (23 / 100))
  
  vlrFat <- c(vlr, vlrIVA, vlr + vlrIVA)
  
  return(vlrFat)
  
}



############# ATD ##################
#Graus de liberdade [n-1]
grsLbd <- function(n){
  # n : Dimensão da Amostra
  
  gL <- (n - 1)
  
  return(gL)
}



################################################################################
# Define UI                                                               ######
################################################################################
ui <- fluidPage(
    useShinyjs(),  # Set up shinyjs

    # Application title
    # titlePanel("MEGAF"),

    navbarPage("MEGAF",
        tabPanel("ATD",
                 h2("Calcular Intervalo de Confiança (IC):"),
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("mp",HTML("Média da População [µ]"), NULL),
                     radioButtons("dpp_c", "Desvio-padrão da População [σ] desconhecido:",
                                  c("Sim" = "1",
                                    "Não" = "0")),
                     
                     hidden(
                       numericInput("dpp",HTML("Desvio-padrão da População [σ]"), NULL)
                     ),

                     numericInput("x",HTML("Média da Amostra [x]"), NULL),
                     numericInput("s",HTML("Desvio-padrão da Amostra [s]"), NULL),
                     numericInput("n",HTML("Dimensão da Amostra [n]"), NULL),
                     numericInput("gc",HTML("Grau de Confiança [1-α]"), NULL),
                     hidden(
                       actionButton("actBtnATD","Calcular")
                     )
                   ),
                   mainPanel(
                     # h4("Consola do R"),
                     uiOutput("uiOtpATD")
                   )
                 )
        ),
        tabPanel("GE",
            h2("Cálculo da fatura de energia elétrica"),
            sidebarLayout(
              sidebarPanel(
                # Nível de Tensão Elétrica (kV)
                numericInput("nmcInpNvTE",HTML("N&iacute;vel de Tens&atilde;o El&eacute;trica (kV)"), NULL),
                
                hr(),
                
                # Potência Contratada (kW)
                numericInput("nmcInpPC",HTML("Pot&ecirc;ncia Contratada (kW)"), NULL),
                
                hr(),
                
                # Mês da fatura
                selectInput("sIMes", HTML("M&ecirc;s da fatura"),
                            choices = meses,
                            selected = NULL
                ),
                
                hr(),
                
                
                # Energia Ativa
                span(id = "spanEA", HTML("<strong>Energia Ativa</strong>")),
                
                # Energia Ativa: Horas de ponta (kWh)
                numericInput("nmcInpHP",HTML("<span style=\"font-weight:normal;font-size:13px\">Horas de ponta (kWh)</span>"), NULL),
                # Energia Ativa: Horas cheias (kWh)
                numericInput("nmcInpHC",HTML("<span style=\"font-weight:normal;font-size:13px\">Horas cheias (kWh)</span>"), NULL),
                # Energia Ativa: Horas de vazio normal (kWh)
                numericInput("nmcInpHVN",HTML("<span style=\"font-weight:normal;font-size:13px\">Horas de vazio normal (kWh)</span>"), NULL),
                # Energia Ativa: Horas de super vazio (kWh)
                numericInput("nmcInpHSV",HTML("<span style=\"font-weight:normal;font-size:13px\">Horas de super vazio (kWh)</span>"), NULL),
                
                hr(id = "hrEA"),
                
                
                # Energia Reativa
                span(id = "spanER", HTML("<strong>Energia Reativa</strong>")),
                
                # Energia Reativa: Indutiva (kVArh)
                numericInput("nmcInpERI",HTML("<span style=\"font-weight:normal;font-size:13px\">Indutiva (kVArh)</span>"), 0),
                # Energia Reativa: Capacitiva (kVArh)
                numericInput("nmcInpERC",HTML("<span style=\"font-weight:normal;font-size:13px\">Capacitiva (kVArh)</span>"), 0),
                
                hr(id = "hrER"),
                
                # Radio buttons Opção Tarifária
                radioButtons("rbOpTrf", label = HTML("Op&ccedil;&atilde;o Tarif&aacute;ria"),
                             choiceNames = list(HTML("<span>Tarifa de longas utiliza&ccedil;&otilde;es</span>"),
                                                HTML("<span>Tarifa de m&eacute;dias utiliza&ccedil;&otilde;es</span>"),
                                                HTML("<span>Tarifa de curtas utiliza&ccedil;&otilde;es</span>")
                             ),
                             choiceValues = list(1,
                                                 2,
                                                 3
                             ),
                             selected = NA
                ),
                
                hr(),
                
                # Radio buttons Períodos Horários
                radioButtons("rbPHrs", label = HTML("Per&iacute;odos Hor&aacute;rios"),
                             choiceNames = list(HTML("<span>Ciclo Semanal</span><br/>
                                                     <span style=\"font-size:11px\">[BTN ; BTE ; MT ; AT ; MAT]</span>"),
                                                HTML("<span>Ciclo Semanal Opcional</span><br/>
                                                     <span style=\"font-size:11px\">[MT ; AT ; MAT]</span>"),
                                                HTML("<span>Ciclo Diário</span><br/>
                                                     <span style=\"font-size:11px\">[BTN ; BTE]</span>")
                             ),
                             choiceValues = list(1,
                                                 2,
                                                 3
                             ),
                             selected = NA
                ),
                
                hr(),
                
                hidden(
                  # Radio buttons Nível Tensão Elétrica
                  radioButtons("rbNvTE", label = HTML("N&iacute;vel de Tens&atilde;o El&eacute;trica"),
                               choiceNames = list(HTML("<span>Baixa Tensão Normal (BTN)</span><br/>
                                                     <span style=\"font-size:11px\">[T &#8804; 1kV ; PC&sup1; &#8804; 41,4kW]</span>"),
                                                  HTML("<span>Baixa Tensão Especial (BTE)</span><br/>
                                                     <span style=\"font-size:11px\">[T &#8804; 1kV ; PC&sup1; > 41,4kW]</span>"),
                                                  HTML("<span>Média Tensão (MT)</span><br/>
                                                     <span style=\"font-size:11px\">[1 kV < T &#8804; 45kV]</span>"),
                                                  HTML("<span>Alta Tensão (AT)</span><br/>
                                                     <span style=\"font-size:11px\">[45kV < TEF&sup2; &#8804; 110kV]</span>"),
                                                  HTML("<span>Muita Alta Tensão (MAT)</span><br/>
                                                     <span style=\"font-size:11px\">[TEF&sup2; > 110kV]</span>")
                               ),
                               choiceValues = list(1,
                                                   2,
                                                   3,
                                                   4,
                                                   5
                               ),
                               selected = NA
                  ),
                  
                  span(id = "spanLgdRbNvTE", HTML("<span style=\"font-size:11px\">&sup1; Pot&ecirc;ncia Contratada<br/>
                      &sup2; Tens&atilde;o Entre Fases</span>")),
                  
                  hr(id = "hrRbNvTE")
                  
                ),
                
                hidden(
                  # Radio buttons Período Tarifário
                  radioButtons("rbPrTrf", label = HTML("Per&iacute;odo Tarif&aacute;rio"),
                               choiceNames = list(HTML("<span>Per&iacute;odo I</span><br/>
                                                         <span style=\"font-size:11px\">[jan ; fev ; mar]</span>"),
                                                  HTML("<span>Per&iacute;odo II</span><br/>
                                                         <span style=\"font-size:11px\">[abr ; mai ; jun]</span>"),
                                                  HTML("<span>Per&iacute;odo III</span><br/>
                                                         <span style=\"font-size:11px\">[jul ; ago ; set]</span>"),
                                                  HTML("<span>Per&iacute;odo IV</span><br/>
                                                         <span style=\"font-size:11px\">[out ; nov ; dez]</span>")
                               ),
                               choiceValues = list(1,
                                                   2,
                                                   3,
                                                   4
                               ),
                               selected = NA
                  ),
                  
                  hr(id = "hrRbPrTrf")
                ),
                
                hidden(
                  actionButton("actBtnClcr","Calcular")
                )
              ),
              
              # Show Valor da Fatura
              mainPanel(
                uiOutput("uiOutValue")
              )
            )
        ),
        
        tabPanel("GF",
            h2("Cálculo das Rendabilidades"),
            sidebarLayout(
                sidebarPanel(
                    # Input: Select a file ----
                    fileInput("file_gf", "Escolha o ficheiro .csv",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    
                    hr(),
                    
                    hidden(
                      actionButton("update_bt","Calcular Rendabilidades Totais", width = "225px"),
                      actionButton("dados_1ano","Calcular Rendabilidades apenas no 1o Ano", width = "225px"),
                      actionButton("dados_2ano","Calcular Rendabilidades apenas no 2o Ano", width = "225px"),
                      actionButton("dados_3ano","Calcular Rendabilidades apenas no 3o Ano", width = "225px")
                    ),
                    
                    hr(),
                    
                    hidden(
                        # Input: Select a dataset ----
                        selectInput("rentSelect", "Graficos Comparativos:",
                                    choices = c("Rentabilidade das Vendas",
                                                "Rendibilidade Operacional do Ativo",
                                                "Rendibilidade do Investimento Total",
                                                "Rendibilidade dos Capitais Proprios", 
                                                "Rotacao do Ativo",
                                                "Custo medio dos capitais Alheios/Passivo", 
                                                "Grau de endividamento", 
                                                "Autonomia Financeira"
                                    )
                        ),
                        
                        actionButton("update_gr","Gerar Graficos")
                    )
                ),
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  # Output: Data file ----
                  tableOutput("contents"),
                  
                  uiOutput("calculos"),
                  
                  plotOutput(outputId = "distPlot_1")
                )
            )
        ),
        tabPanel("LOG",
            h2("Representação Geográfica das 500 Cidades Mundiais com Maior População"),
            sidebarLayout(
                sidebarPanel(
                    fileInput("file_log", "Escolha o ficheiro .csv",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
                         
                    ),
                    # Botão para abrir o ficheiro csv e mostrar o mapa
                    actionButton("update_map","Gerar Mapa")
                ),
             
                # Mostra o mapa
                mainPanel(
                 leafletOutput(outputId = "mapa", width = 1000, height = 1000)
                )
            )
        ),
        tabPanel("MAD",
            h2("Forma dos Arcos Emergentes (FSF - Forward Star Form)"),
            sidebarLayout(
                sidebarPanel(
                    textInput("point",HTML("point<span style=\"color:red\">*</span>") ,placeholder = "0,1,10,25"),
                    textInput("suc", HTML("suc<span style=\"color:red\">*</span>"), placeholder = "0,1,10,25"),
                    textInput("dist", HTML("dist<span style=\"color:red\">*</span>"), placeholder = "0,1,10,25"),
                    textInput("capacidade", "capacidade", placeholder = "0,1,10,25"),
                    HTML(paste("<p style=\"color:red\">*Preenchimento obrigatório!</p>" ,strong("Nota:"), "Números inteiros positivos (delimitados por vírgulas)" ,"<br/>" ,"<br/>")),
                    actionButton("up_bt_mad","Ver dados"),
                ),
                mainPanel(
                    h4("Dados da Rede"),
                    tableOutput(outputId = "table.output"),
                )
            )
        ),
        navbarMenu("Ajuda",
                   tabPanel("Sobre",
                            HTML(paste("<pre>",
                                       "<strong>Trabalho Prático:</strong> Shiny<br/>",
                                       "<strong>Docente         :</strong> Nuno Lavado - nlavado@isec.pt<br/>",
                                       "<strong>Disciplina      :</strong> Análise e Tratamento de Dados<br/>",
                                       "<strong>Curso           :</strong> Mestrado em Engenharia e Gestão de Ativos Físicos<br/>",
                                       hr(),
                                       "<strong>Bibliotecas a instalar no R:</strong> install.packages(shiny)<br/>",
                                       "                             install.packages(shinyjs)<br/>",
                                       "                             install.packages(shinyFiles)<br/>",
                                       "                             install.packages(gdata)<br/>",
                                       "                             install.packages(leaflet)<br/>",
                                       "                             install.packages(dplyr)<br/>",
                                       "<br/>",
                                       "<strong>Bibliotecas no R:</strong> library(shiny)<br/>",
                                       "                  library(shinyjs)<br/>",
                                       "                  library(shinyFiles)<br/>",
                                       "                  library(gdata)<br/>",
                                       "                  library(leaflet)<br/>",
                                       "                  library(dplyr)<br/>",
                                       "<br/>",
                                       "<strong>Executar na consola do R:</strong> shiny::runGist(\"2883dc8ad9d89df3bd3eb5a9949f8822\")",
                                       hr(),
                                       "<strong>Executar num browser:</strong> https://atd-shiny.shinyapps.io/atd-shiny_20210100/",
                                       "</pre>"
                                 )
                            ),
                   ),
                   tabPanel("Créditos",
                            HTML(paste("<pre>",
                                       "<strong>Desenvolvido por:</strong> João Gonçalves  [2020149226]  - a2020149226@isec.pt<br/>",
                                       "                  Luís Pato       [2005009772]  - a21150211@isec.pt<br/>",
                                       "                  Samuel Martinho [2006005673]  - a21170106@isec.pt<br/>",
                                       hr(),
                                       "Versão: janeiro, 2021",
                                       "</pre>"
                                 )
                            )
                   )
        )
    )
)
    
    
        
################################################################################
# Define server                                                           ######
################################################################################
server <- function(input, output, session) {
  
  ############# MAD ############################################################
  ###### Número de colunas do Data Frame ##
  nrColDF <- reactive(max(ncol(pointTb())
                          ,ncol(sucTb())
                          ,ncol(distTb())
                          ,ncol(capacidadeTb())
                          )
                      )

    

  ###### pointTb  ###########################
  pointTb <- eventReactive(input$up_bt_mad, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())
    
    pTb <- ler_nr(input$point, "point")

        
    return(pTb)
  })
  
  
  
  ###### sucTb  ###########################
  sucTb <- eventReactive(input$up_bt_mad, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())
    
    sTb <- ler_nr(input$suc, "suc")
    

    return(sTb)
  })

  
    
  ###### distTb  ###########################
  distTb <- eventReactive(input$up_bt_mad, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())
    
    dTb <- ler_nr(input$dist, "dist")
    
    
    return(dTb)
  })
  
  
  
  ###### capacidadeTb  ###########################
  capacidadeTb <- eventReactive(input$up_bt_mad, {
    
    if (input$point == "" | input$suc == "" | input$dist == "" | input$capacidade == "") return(invisible())
    
    cTb <- ler_nr(input$capacidade, "capacidade")
    
    
    return(cTb)
  })
  

  
  ###### tabela  ###########################
  tabela <- eventReactive(input$up_bt_mad, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())

    ##### point ##############################################################
    if(row.names(pointTb()) == "point")
      if (nrColDF() < ncol(pointTb())){
  
        dfDados <<- data.frame(pointTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
  
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(pointTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
  
        dfDados <<- aum_col(nrColDF(), pointTb())
        
      }
      else{
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados <<- aum_col(nrColDF(), pointTb())

      }

    
    ##### suc ##############################################################
    if(row.names(sucTb()) == "suc"){
      if (nrColDF() < ncol(sucTb())){
        
        dfDados <<- data.frame(sucTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(sucTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
        dfDados <<- aum_col(nrColDF(), sucTb())
        
      }
      else if (nrow(dfDados) > 1){
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados["suc",] <<- aum_col(nrColDF(), sucTb())

      }
      else if (nrow(dfDados) == 1 & nrColDF() > ncol(sucTb())){
        
        dfDados <<- aum_col(nrColDF(), pointTb())
        dfDados <<- aum_col(nrColDF(), sucTb())
        
      }
      else{

        dfDados <<- rbind(dfDados,sucTb())
        
      }
    }


    ##### dist ##############################################################
    if(row.names(distTb()) == "dist")
      if (nrColDF() < ncol(distTb())){
  
        dfDados <<- data.frame(distTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(distTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
        dfDados <<- aum_col(nrColDF(), distTb())
        
      }
      else{
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados <<- rbind(dfDados,distTb())
        
      }
    

    ##### capacidade ########################################################
    if(input$capacidade != "" && row.names(capacidadeTb()) == "capacidade")
      if (input$capacidade != "" && nrColDF() < ncol(capacidadeTb())){
        
        dfDados <<- data.frame(capacidadeTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(capacidadeTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
        dfDados <<- aum_col(nrColDF(), capacidadeTb())
        
      }
      else{
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados <<- rbind(dfDados,capacidadeTb())
        
      }
    

    dfDados[dfDados == 0] <<- ""
    

    return(dfDados)
    
  })

  observeEvent(input$up_bt_mad, {
      output$table.output <- renderTable(tabela(), digits = 0, bordered = TRUE, rownames = TRUE)
  })
  
  
  
  
  ############# GE  ############################################################
  reactive(req(input$nmcInpNvTE))
  reactive(req(input$nmcInpPC))
  reactive(req(input$sIMes))
  
  
  observeEvent(input$nmcInpNvTE & input$nmcInpPC & as.numeric(input$sIMes), {    
    
    # Radio buttons Nível Tensão Elétrica
    if(is.na(input$nmcInpNvTE) | is.na(input$nmcInpPC)){
      hide("rbNvTE")
      hide("spanLgdRbNvTE")
      hide("hrRbNvTE")
      hide("actBtnClcr")
    }
    else if(input$nmcInpNvTE > 1 & input$nmcInpNvTE <= 45){
      updateRadioButtons(session, "rbNvTE", selected = 3)
      show("rbNvTE")
      disable("rbNvTE")
      show("spanLgdRbNvTE")
      show("hrRbNvTE")
      show("actBtnClcr")
    }
    else if(input$nmcInpNvTE > 45 & input$nmcInpNvTE <= 110){
      updateRadioButtons(session, "rbNvTE", selected = 4)
      show("rbNvTE")
      disable("rbNvTE")
      show("spanLgdRbNvTE")
      show("hrRbNvTE")
      show("actBtnClcr")
    }
    else if(input$nmcInpNvTE > 110){
      updateRadioButtons(session, "rbNvTE", selected = 5)
      show("rbNvTE")
      disable("rbNvTE")
      show("spanLgdRbNvTE")
      show("hrRbNvTE")
      show("actBtnClcr")
    }
    else if(input$nmcInpPC <= 41.4){
      updateRadioButtons(session, "rbNvTE", selected = 1)
      show("rbNvTE")
      disable("rbNvTE")
      show("spanLgdRbNvTE")
      show("hrRbNvTE")
      show("actBtnClcr")
    }
    else if(input$nmcInpPC > 41.4){
      updateRadioButtons(session, "rbNvTE", selected = 2)
      show("rbNvTE")
      disable("rbNvTE")
      show("spanLgdRbNvTE")
      show("hrRbNvTE")
      show("actBtnClcr")
    }
    
    
    # Radio buttons Período Tarifário
    if(is.na(input$sIMes)){
      hide("rbPrTrf")
      hide("hrRbPrTrf")
    }
    else if(as.numeric(input$sIMes) >= 1 & as.numeric(input$sIMes) <= 3){
      updateRadioButtons(session, "rbPrTrf", selected = 1)
      show("rbPrTrf")
      disable("rbPrTrf")
      show("hrRbPrTrf")
      
    }
    else if(as.numeric(input$sIMes) >= 4 & as.numeric(input$sIMes) <= 6){
      updateRadioButtons(session, "rbPrTrf", selected = 2)
      show("rbPrTrf")
      disable("rbPrTrf")
      show("hrRbPrTrf")
      
    }
    else if(as.numeric(input$sIMes) >= 7 & as.numeric(input$sIMes) <= 9){
      updateRadioButtons(session, "rbPrTrf", selected = 3)
      show("rbPrTrf")
      disable("rbPrTrf")
      show("hrRbPrTrf")
      
    }
    else if(as.numeric(input$sIMes) >= 10 & as.numeric(input$sIMes) <= 12){
      updateRadioButtons(session, "rbPrTrf", selected = 4)
      show("rbPrTrf")
      disable("rbPrTrf")
      show("hrRbPrTrf")
      
    }
  })
  

  observeEvent(input$actBtnClcr, {
    runjs("window.scrollTo(0, 0)")
    
    fatura <- calcFat(input$rbNvTE, input$nmcInpPC, input$rbOpTrf, input$nmcInpHP, input$nmcInpHC, input$nmcInpHVN, input$nmcInpHSV, input$rbPHrs, input$sIMes, input$rbPrTrf, input$nmcInpERI, input$nmcInpERC)
    encargosPot <- encPot(input$nmcInpPC, input$rbNvTE, input$rbOpTrf, input$nmcInpHP, input$rbPHrs, input$sIMes)
    encargosEnrgAtv <- encTEA(input$rbNvTE, input$rbOpTrf, input$rbPrTrf, input$nmcInpHP, input$nmcInpHC, input$nmcInpHVN, input$nmcInpHSV)
    encargosEnrgRtv <- encER(input$nmcInpERI, input$nmcInpHP, input$nmcInpHC, input$rbNvTE, input$nmcInpERC)
    
    
    output$uiOutValue <- renderUI({ 
      HTML(paste("<hr style=\"color: #000000;\">",
                 "<pre>",
                 "Valor da Fatura s/IVA de", names(meses[as.numeric(input$sIMes)]), " : ", format(round(as.numeric(fatura[1]), 2), nsmall=2, big.mark=".", decimal.mark=",", justify="right", width=12), "€<br/>",
                 "Valor do IVA da Fatura de", names(meses[as.numeric(input$sIMes)]), ": ", format(round(as.numeric(fatura[2]), 2), nsmall=2, big.mark=".", decimal.mark=",", justify="right", width=12), "€<br/>",
                 "Valor da Fatura c/IVA de", names(meses[as.numeric(input$sIMes)]), " : ", format(round(as.numeric(fatura[3]), 2), nsmall=2, big.mark=".", decimal.mark=",", justify="right", width=12), "€",
                 "</pre>",
                 "<hr style=\"color: #000000;\">",
                 "<pre>",
                 "Termo tarifário fixo de", names(meses[as.numeric(input$sIMes)]), "        :",format(round(as.numeric(trmTrfFx(input$rbNvTE)), 2), nsmall=2, big.mark=".", decimal.mark=",", width=12), "€<br/>",
                 "Encargos com Potência de", names(meses[as.numeric(input$sIMes)]), "       :",format(round(as.numeric(encargosPot), 2), nsmall=2, big.mark=".", decimal.mark=",", width=12),            "€<br/>",
                 "Encargos com Energia Ativa de", names(meses[as.numeric(input$sIMes)]), "  :",format(round(as.numeric(encargosEnrgAtv), 2), nsmall=2, big.mark=".", decimal.mark=",", width=12),        "€<br/>",
                 "Encargos com Energia Reativa de", names(meses[as.numeric(input$sIMes)]), ":",format(round(as.numeric(encargosEnrgRtv), 2), nsmall=2, big.mark=".", decimal.mark=",", width=12),        "€<br/>",
                 "</pre>"
                )
          ) 
    })
  })
  

  
  ############# LOG ############################################################
  # Lê o ficheiro os campos do ficheiro .csv
  observeEvent(input$update_map, {
      city_info <- read.csv(input$file_log$datapath,
                            header = TRUE,
                            sep = ";",
                            quote = '"')
      
      #Faz o render da cartografia, adiciona os marcadores com base na localização e a janela POP UP    
      output$mapa <- renderLeaflet({
          leaflet() %>%
          addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
          setView(lat = 30, lng = 30, zoom = 2) %>%
          setMaxBounds(lng1 = -140, lat1 = -70, lng2 = 155, lat2 = 70) %>%
          addMarkers(data = city_info,
                     lng = ~city_info$lng,
                     lat = ~city_info$lat,
                     popup = paste(
                       "<b>Cidade: </b>",city_info$city,"<br>",
                       "<b>País: </b>",city_info$country,"<br>",
                       "<b>População: </b>",city_info$population,"<br>"
                     )
          )
      })
  })
  


  ############# GF #############################################################
  #Obter dados do csv
  output$contents <- renderTable({
    req(input$file_gf)
    
    df <<- read.csv(input$file_gf$datapath,
                    header = TRUE,
                    sep = ";",
                    quote = '"'
    )
    updateActionButton(session, "dados_1ano", HTML(paste("<span style=\"font-size:10px\">Calcular Rendabilidades apenas no ano ", df$Ano[1], "</span>")))
    updateActionButton(session, "dados_2ano", HTML(paste("<span style=\"font-size:10px\">Calcular Rendabilidades apenas no ano ", df$Ano[2], "</span>")))
    updateActionButton(session, "dados_3ano", HTML(paste("<span style=\"font-size:10px\">Calcular Rendabilidades apenas no ano ", df$Ano[3], "</span>")))
    show("update_bt")
    show("dados_1ano")
    show("dados_2ano")
    show("dados_3ano")
    
    show("rentSelect")
    show("update_gr")
    
    return(df)
  })
  
  
  ################ Calcular dados para os 3 anos #################################################################################
  observeEvent(input$update_bt, {
    hide("distPlot_1")
    show("calculos")
    output$calculos <- renderUI({
      HTML(paste(tags$strong("_________________________________________________________________"), "<br/>",
                 tags$strong("Resultados dos anos de ",df$Ano[1],",",df$Ano[2],"e",df$Ano[3]), "<br/>", "<br/>",
                 "Rentabilidade das Vendas de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Operacional[1] / df$Vendas[1])*100,2))),"%", "<br/>",
                 "Rentabilidade das Vendas de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Operacional[2] / df$Vendas[2])*100,2))),"%", "<br/>",
                 "Rentabilidade das Vendas de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Operacional[3] / df$Vendas[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade Operacional do Ativo de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Operacional[1] / df$Activo[1])*100,2))),"%", "<br/>",
                 "Rendibilidade Operacional do Ativo de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Operacional[2] / df$Activo[2])*100,2))),"%", "<br/>",
                 "Rendibilidade Operacional do Ativo de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Operacional[3] / df$Activo[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade do Investimento Total de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Liquido[1] / df$Activo[1])*100,2))),"%", "<br/>",
                 "Rendibilidade do Investimento Total de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Liquido[2] / df$Activo[2])*100,2))),"%", "<br/>",
                 "Rendibilidade do Investimento Total de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Liquido[3] / df$Activo[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade dos Capitais Proprios de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Liquido[1] / df$Capital_Proprio[1])*100,2))),"%", "<br/>",
                 "Rendibilidade dos Capitais Proprios de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Liquido[2] / df$Capital_Proprio[2])*100,2))),"%", "<br/>",
                 "Rendibilidade dos Capitais Proprios de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Liquido[3] / df$Capital_Proprio[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rotacao do Ativo de ", df$Ano[1], ": ", tags$strong(format(round(df$Vendas[1] / df$Activo[1],2))), "<br/>",
                 "Rotacao do Ativo de ", df$Ano[2], ": ", tags$strong(format(round(df$Vendas[2] / df$Activo[2],2))), "<br/>",
                 "Rotacao do Ativo de ", df$Ano[3], ": ", tags$strong(format(round(df$Vendas[3] / df$Activo[3],2))), "<br/>", "<br/>",
                 "Custo medio dos capitais Alheios/Passivo de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Operacional[1] / df$Passivo[1])*100,2))),"%", "<br/>",
                 "Custo medio dos capitais Alheios/Passivo de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Operacional[2] / df$Passivo[2])*100,2))),"%", "<br/>",
                 "Custo medio dos capitais Alheios/Passivo de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Operacional[3] / df$Passivo[3])*100,2))),"%", "<br/>", "<br/>",
                 "Grau de endividamento de ", df$Ano[1], ": ", tags$strong(format(round(df$Passivo[1] / df$Capital_Proprio[1],2))), "<br/>",
                 "Grau de endividamento de ", df$Ano[2], ": ", tags$strong(format(round(df$Passivo[2] / df$Capital_Proprio[2],2))), "<br/>",
                 "Grau de endividamento de ", df$Ano[3], ": ", tags$strong(format(round(df$Passivo[3] / df$Capital_Proprio[3],2))), "<br/>", "<br/>",
                 "Autonomia Financeira de ", df$Ano[1], ": ", tags$strong(format(round(df$Capital_Proprio[1] / df$Activo[1],2))), "<br/>",
                 "Autonomia Financeira de ", df$Ano[2], ": ", tags$strong(format(round(df$Capital_Proprio[2] / df$Activo[2],2))), "<br/>",
                 "Autonomia Financeira de ", df$Ano[3], ": ", tags$strong(format(round(df$Capital_Proprio[3] / df$Activo[3],2))), "<br/>", "<br/>"
      )
      )
    })
  })
  
  
  ################ Calcular dados para o 1o ano #################################################################################
  observeEvent(input$dados_1ano, {
    hide("distPlot_1")
    show("calculos")
    output$calculos <- renderUI({
      HTML(paste(tags$strong("_________________________________________________________________"), "<br/>",
                 tags$strong("Resultados do ano de ",df$Ano[1]), "<br/>", "<br/>",
                 "Rentabilidade das Vendas de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Operacional[1] / df$Vendas[1])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade Operacional do Ativo de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Operacional[1] / df$Activo[1])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade do Investimento Total de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Liquido[1] / df$Activo[1])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade dos Capitais Proprios de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Liquido[1] / df$Capital_Proprio[1])*100,2))),"%", "<br/>", "<br/>",
                 "Rotacao do Ativo de ", df$Ano[1], ": ", tags$strong(format(round(df$Vendas[1] / df$Activo[1],2))), "<br/>", "<br/>",
                 "Custo medio dos capitais Alheios/Passivo de ", df$Ano[1], ": ", tags$strong(format(round((df$Resultado_Operacional[1] / df$Passivo[1])*100,2))),"%", "<br/>", "<br/>",
                 "Grau de endividamento de ", df$Ano[1], ": ", tags$strong(format(round(df$Passivo[1] / df$Capital_Proprio[1],2))), "<br/>", "<br/>",
                 "Autonomia Financeira de ", df$Ano[1], ": ", tags$strong(format(round(df$Capital_Proprio[1] / df$Activo[1],2))), "<br/>"
      )
      )
    })
  })
  
  
  ################ Calcular dados para o 2o ano #################################################################################
  observeEvent(input$dados_2ano, {
    hide("distPlot_1")
    show("calculos")
    output$calculos <- renderUI({
      HTML(paste(tags$strong("_________________________________________________________________"), "<br/>",
                 tags$strong("Resultados do ano de ",df$Ano[2]), "<br/>", "<br/>",
                 "Rentabilidade das Vendas de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Operacional[2] / df$Vendas[2])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade Operacional do Ativo de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Operacional[2] / df$Activo[2])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade do Investimento Total de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Liquido[2] / df$Activo[2])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade dos Capitais Proprios de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Liquido[2] / df$Capital_Proprio[2])*100,2))),"%", "<br/>", "<br/>",
                 "Rotacao do Ativo de ", df$Ano[2], ": ", tags$strong(format(round(df$Vendas[2] / df$Activo[2],2))), "<br/>", "<br/>",
                 "Custo medio dos capitais Alheios/Passivo de ", df$Ano[2], ": ", tags$strong(format(round((df$Resultado_Operacional[2] / df$Passivo[2])*100,2))),"%", "<br/>", "<br/>",
                 "Grau de endividamento de ", df$Ano[2], ": ", tags$strong(format(round(df$Passivo[2] / df$Capital_Proprio[2],2))), "<br/>", "<br/>",
                 "Autonomia Financeira de ", df$Ano[2], ": ", tags$strong(format(round(df$Capital_Proprio[2] / df$Activo[2],2))), "<br/>"
      )
      )
    })
  })
  
  
  ################ Calcular dados para o 3o ano #################################################################################
  observeEvent(input$dados_3ano, {
    hide("distPlot_1")
    show("calculos")
    output$calculos <- renderUI({
      HTML(paste(tags$strong("_________________________________________________________________"), "<br/>",
                 tags$strong("Resultados do ano de ",df$Ano[3]), "<br/>", "<br/>",
                 "Rentabilidade das Vendas de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Operacional[3] / df$Vendas[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade Operacional do Ativo de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Operacional[3] / df$Activo[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade do Investimento Total de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Liquido[3] / df$Activo[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rendibilidade dos Capitais Proprios de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Liquido[3] / df$Capital_Proprio[3])*100,2))),"%", "<br/>", "<br/>",
                 "Rotacao do Ativo de ", df$Ano[3], ": ", tags$strong(format(round(df$Vendas[3] / df$Activo[3],2))), "<br/>", "<br/>",
                 "Custo medio dos capitais Alheios/Passivo de ", df$Ano[3], ": ", tags$strong(format(round((df$Resultado_Operacional[3] / df$Passivo[3])*100,2))),"%", "<br/>", "<br/>",
                 "Grau de endividamento de ", df$Ano[3], ": ", tags$strong(format(round(df$Passivo[3] / df$Capital_Proprio[3],2))), "<br/>", "<br/>",
                 "Autonomia Financeira de ", df$Ano[3], ": ", tags$strong(format(round(df$Capital_Proprio[3] / df$Activo[3],2))), "<br/>"
      )
      )
    })
  })
  
  
  ################ Mostrar Graficos ###############################################################################
  
  ################### Selacao dos Graficos #######################################
  datasetInput <- observeEvent(input$update_gr, {
    switch(input$rentSelect,
           distPlot_1 = "Rentabilidade das Vendas",
           distPlot_2 = "Rendibilidade Operacional do Ativo",
           distPlot_3 = "Rendibilidade do Investimento Total",
           distPlot_4 = "Rendibilidade dos Capitais Proprios",
           distPlot_5 = "Rotacao do Ativo",
           distPlot_6 = "Custo medio dos capitais Alheios/Passivo",
           distPlot_7 = "Grau de endividamento",
           distPlot_8 = "Autonomia Financeira"
           
    )
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$update_gr, {
    hide("calculos")
    show("distPlot_1")
    
    if(input$rentSelect == "Rentabilidade das Vendas"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round(((df$Resultado_Operacional / df$Vendas)*100),2)
        
        # barplot(y~x, breaks = 3, col = "#75AADB", border = "White",
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round(((df$Resultado_Operacional / df$Vendas)*100),2),
                xlab = "Anos",
                ylab = "Rendibilidade das Vendas [%]",
                main = "Evolucao da Rendibilidade das Vendas ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Rendibilidade Operacional do Ativo"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Resultado_Operacional / df$Activo)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Resultado_Operacional / df$Activo)*100,2),
                xlab = "Anos",
                ylab = "Rendibilidade Operacional do Ativo [%]",
                main = "Evolucao da Rendibilidade Operacional do Ativo ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Rendibilidade do Investimento Total"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Resultado_Liquido / df$Activo)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Resultado_Liquido / df$Activo)*100,2),
                xlab = "Anos",
                ylab = "Rendibilidade do Investimento Total [%]",
                main = "Evolucao da Rendibilidade do Investimento Total ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Rendibilidade dos Capitais Proprios"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Resultado_Liquido / df$Capital_Proprio)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Resultado_Liquido / df$Capital_Proprio)*100,2),
                xlab = "Anos",
                ylab = "Rendibilidade dos Capitais Proprios [%]",
                main = "Evolucao da Rendibilidade dos Capitais Proprios ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Rotacao do Ativo"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Vendas / df$Activo)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Vendas / df$Activo)*100,2),
                xlab = "Anos",
                ylab = "Rotacao do Ativo [-]",
                main = "Evolucao da Rotacao do Ativo ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Custo medio dos capitais Alheios/Passivo"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Resultado_Operacional / df$Passivo)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Resultado_Operacional / df$Passivo)*100,2),
                xlab = "Anos",
                ylab = "Custo medio dos capitais Alheios/Passivo [%]",
                main = "Evolucao do Custo medio dos capitais Alheios/Passivo ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Grau de endividamento"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Passivo / df$Capital_Proprio)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Passivo / df$Capital_Proprio)*100,2),
                xlab = "Anos",
                ylab = "Grau de Endividamento [-]",
                main = "Evolucao do Grau de Endividamento ao longo dos anos")
      })
    }
    else if(input$rentSelect == "Autonomia Financeira"){
      output$distPlot_1 <- renderPlot({
        x    <- df$Ano
        y    <- round((df$Capital_Proprio / df$Activo)*100,2)
        
        barplot(y~x, col = "#75AADB", border = "White",
                legend.text = round((df$Capital_Proprio / df$Activo)*100,2),
                xlab = "Anos",
                ylab = "Autonomia Financeira [-]",
                main = "Evolucao da Autonomia Financeira ao longo dos anos")
      })
    }
  })

  
  
  ############# ATD ############################################################
  observeEvent(input$dpp_c, {
      if(input$dpp_c == "0"){
         show("dpp")
      }
      else if (input$dpp_c == "1"){
         hide("dpp")
      }
  })
  
  observeEvent(input$mp & as.numeric(input$dpp_c) & input$dpp & input$x & input$s & input$n & input$gc, {
      if(is.na(input$x) | is.na(input$s) | is.na(input$n) | is.na(input$gc) | (input$dpp_c == "0" & is.na(input$dpp))){
          hide("actBtnATD")
      }
      else{
          show("actBtnATD")
      }
  })
  
    
  observeEvent(input$actBtnATD, {
    output$uiOtpATD <- renderUI({
      if(input$dpp_c == "0"){
        HTML(paste("<hr style=\"color: #000000;\">",
                   "<pre>",
                   "Graus de liberdade [n-1]        = ", input$n - 1, "<br/>",
                   "α                               = ", 1 - input$gc, "<br/>",
                   "α/2                             = ", (1 - input$gc)/2, "<br/>",
                   "1 - α/2                         = ", 1 - (1 - input$gc)/2, "<br/>",
                   "z(1-α/2)                        = ", qnorm(1 - (1 - input$gc)/2), "<br/>",
                   "Margem de Erro [ME]             = ", qnorm(1 - (1 - input$gc)/2)*input$dpp/input$n^0.5, "<br/>",
                   "Intervalo de Confiança Inferior = ", input$x - qnorm(1 - (1 - input$gc)/2)*input$dpp/input$n^0.5, "<br/>",
                   "Intervalo de Confiança Superior = ", input$x + qnorm(1 - (1 - input$gc)/2)*input$dpp/input$n^0.5, "<br/>",
                   "Intervalo de Confiança [IC]     = ]", input$x - qnorm(1 - (1 - input$gc)/2)*input$dpp/input$n^0.5, " , ", input$x + qnorm(1 - (1 - input$gc)/2)*input$dpp/input$n^0.5, "[<br/>",
                   "</pre>"
             )
        )
      }
      else{
        HTML(paste("<hr style=\"color: #000000;\">",
                   "<pre>",
                   "Graus de liberdade [n-1]        = ", input$n - 1, "<br/>",
                   "α                               = ", 1 - input$gc, "<br/>",
                   "α/2                             = ", (1 - input$gc)/2, "<br/>",
                   "1 - α/2                         = ", 1 - (1 - input$gc)/2, "<br/>",
                   "t(1-α/2)                        = ", qt(1 - (1 - input$gc)/2,input$n - 1), "<br/>",
                   "Margem de Erro [ME]             = ", qt(1 - (1 - input$gc)/2,input$n - 1)*input$s/input$n^0.5, "<br/>",
                   "Intervalo de Confiança Inferior = ", input$x - (qt(1 - (1 - input$gc)/2,input$n - 1)*input$s/input$n^0.5), "<br/>",
                   "Intervalo de Confiança Superior = ", input$x + (qt(1 - (1 - input$gc)/2,input$n - 1)*input$s/input$n^0.5), "<br/>",
                   "Intervalo de Confiança [IC]     = ]", input$x - (qt(1 - (1 - input$gc)/2,input$n - 1)*input$s/input$n^0.5), " , ", input$x + (qt(1 - (1 - input$gc)/2,input$n - 1)*input$s/input$n^0.5), "[<br/>",
                   "</pre>"
             )
        )
      }
      
    })
  })
    
}


################################################################################
# Run the application                                                     ######
################################################################################
shinyApp(ui = ui, server = server)

