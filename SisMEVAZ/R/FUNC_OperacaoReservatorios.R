#' Ler Dados de um Açude
#'
#' Essa função extrai os dados geométricos e de identificação de um reservatório (açude) com base no ID fornecido.
#'
#' @param nID ID do reservatório a ser buscado.
#' @param DadosRes Data frame contendo informações gerais dos reservatórios (como cota vertente e nome).
#' @param DadosCAV Data frame contendo informações da relação cota-área-volume dos reservatórios.
#' @return Uma lista contendo a identificação do reservatório e suas características geométricas.
#' @export
LerAcude <- function(nID, DadosRes, DadosCAV) {
  Nome_Reserv <- DadosRes$ACUDE[DadosRes$ID == nID]

  if (length(Nome_Reserv) == 0) {
    print(paste("Reservatório", nID, "não encontrado!"))
  } else {
    Identif_Reserv <- list(ID = nID, Nome = Nome_Reserv)

    Cota   <- DadosCAV$COTA[DadosCAV$ID == nID]           # em m
    Area   <- DadosCAV$AREA_KM2[DadosCAV$ID == nID]       # em km²
    Volume <- DadosCAV$VOLUME_M3[DadosCAV$ID == nID] / 10^6 # de m³ para hm³
    CAV    <- data.frame(Cota, Area, Volume)

    cota_vert      <- DadosRes$COTA_VERT_M[DadosRes$ID == nID]
    Vol_Max_Reserv <- Interpola(CAV$Cota, CAV$Volume, cota_vert) # em hm³
    Vol_Max_Operacional_Reserv <- Vol_Max_Reserv                # Alterável
    Vol_Min_Reserv <- 0                                         # Alterável

    Geometria_Reserv <- list(
      CAV = CAV,
      Vol_Max = Vol_Max_Reserv,
      Vol_Max_Operacional = Vol_Max_Operacional_Reserv,
      Vol_Min = Vol_Min_Reserv
    )

    return(list(Identif = Identif_Reserv, Geometria = Geometria_Reserv))
  }
}


#' Interpolação Linear Simples
#'
#' Essa função realiza uma interpolação linear para determinar o valor correspondente a uma entrada.
#'
#' @param vecIn Vetor de valores de entrada (ex.: cotas).
#' @param vecOut Vetor de valores de saída (ex.: volumes ou áreas).
#' @param input Valor de entrada para o qual será calculada a interpolação.
#' @return Valor interpolado correspondente.
#' @export
Interpola <- function(vecIn, vecOut, input) {
  if (input <= min(vecIn)) {
    return(min(vecOut))
  } else if (input >= max(vecIn)) {
    return(max(vecOut))
  } else {
    inter <- findInterval(input, vecIn)
    R <- vecOut[inter] + (input - vecIn[inter]) *
      (vecOut[inter + 1] - vecOut[inter]) / (vecIn[inter + 1] - vecIn[inter])
    return(R)
  }
}

#' Dinâmica Mensal Simplificada de um Reservatório
#'
#' Calcula o volume de um reservatório ao final de um mês considerando entradas, saídas e evaporação.
#'
#' @param Vol_Ini Volume inicial no início do mês (em hm³).
#' @param Aflu Afluência no mês (em hm³/mês).
#' @param Evap Evaporação no mês (em m/mês).
#' @param Retirada Retirada no mês (em hm³/mês).
#' @param Geom_Reserv Estrutura geométrica do reservatório, incluindo curvas cota-área-volume.
#' @return Vetor contendo o volume final, retirada efetiva e vertimento.
#' @export
Dinamica_mensal_Simplificada <- function(Vol_Ini, Aflu, Evap, Retirada, Geom_Reserv) {
  Area_Ini <- Interpola(Geom_Reserv$CAV$Volume, Geom_Reserv$CAV$Area, Vol_Ini)
  Vol <- Vol_Ini + Aflu - Retirada - Evap * Area_Ini
  Area_Fin <- Interpola(Geom_Reserv$CAV$Volume, Geom_Reserv$CAV$Area, Vol)
  Area <- (Area_Ini + Area_Fin) / 2
  Vol <- Vol_Ini + Aflu - Retirada - Evap * Area

  if (Vol < Geom_Reserv$Vol_Min) {
    RetiradaOrig <- Retirada
    Retirada <- Retirada - (Geom_Reserv$Vol_Min - Vol)
    if (Retirada >= 0) {
      Vol <- Geom_Reserv$Vol_Min
    } else {
      Vol <- Vol + RetiradaOrig
      Retirada <- 0
      if (Vol < 0) Vol <- 0
    }
  }

  vert <- 0
  if (Vol > Geom_Reserv$Vol_Max_Operacional) {
    vert <- Vol - Geom_Reserv$Vol_Max_Operacional
    Vol <- Geom_Reserv$Vol_Max_Operacional
  }

  return(c(Vol, Retirada, vert))
}

#' @title Calcula Garantia do Reservatório
#' @description Calcula a garantia de atendimento para um reservatório, dado um valor de retirada.
#' @param Acude Lista contendo informações do reservatório, incluindo geometria (Vol_Min, Vol_Max_Operacional).
#' @param Ret Retirada mensal em metros cúbicos por segundo (m³/s).
#' @param SerieAfluEvap Data frame com séries de afluências (m³/s) e evaporações (mm/mês).
#' @param Voliniperc Percentual do volume útil inicial (padrão: 0.5).
#' @return Garantia do reservatório (valor entre 0 e 1).
#' @importFrom stats uniroot
#' @export
FuncGarantia <- function(Acude, Ret, SerieAfluEvap, Voliniperc = 0.5, garantia_anual=F) {
  Ret <- Ret * 3600 * 24 * 30 / 10^6 # Converte para hm³/mês
  Vol_ini <- Acude$Geometria$Vol_Min +
    (Acude$Geometria$Vol_Max_Operacional - Acude$Geometria$Vol_Min) * Voliniperc
  Vol <- Vol_ini

  Datas <- SerieAfluEvap$Datas
  SerieAflu <- SerieAfluEvap$Aflu * 3600 * 24 * 30 / 10^6 # Converte para hm³/mês
  SerieEvap <- SerieAfluEvap$Evap / 1000 # Converte para m/mês
  vec_falha_men <- rep(0,length(Datas))


  for (i in 1:length(Datas)) {
    estado <- Dinamica_mensal_Simplificada(Vol, SerieAflu[i], SerieEvap[i], Ret, Acude$Geometria)
    Vol <- estado[1]
    Ret_Efetiva <- estado[2]
    if (Ret_Efetiva < Ret) {
      vec_falha_men[i] <- 1
    }
  }

  if(garantia_anual){
    anos <- unique(year(Datas))
    vec_falha_ano <- sapply(anos, function(ano){ifelse(test = sum(vec_falha_men[year(Datas)==ano])==0,yes = 0,no = 1)})
    Falha <- sum(vec_falha_ano) / length(anos)
  }else{
    Falha <- sum(vec_falha_men) / length(Datas)
  }

  Garantia <- 1 - Falha

  return(Garantia)
}

#' @title Discretiza Garantia
#' @description Ajusta o valor da garantia para o maior valor discreto possível, considerando o número de séries.
#' @param nSerie Número total de séries temporais.
#' @param Garantia Garantia desejada (valor contínuo).
#' @return Garantia ajustada ao maior valor discreto possível.
#' @export
DiscretizaGarantia <- function(nSerie, Garantia) {
  passo_garantia <- 1 / nSerie
  if (Garantia %% passo_garantia == 0) {
    GarantiaPossivel <- Garantia
  } else {
    GarantiaPossivel <- (Garantia %/% passo_garantia + 1) * passo_garantia
  }
  return(GarantiaPossivel)
}

#' @title Encontra Retirada para Garantia Desejada
#' @description Calcula o valor de retirada que resulta na garantia desejada, ajustada para o maior valor discreto possível.
#' @param Acude Lista contendo informações do reservatório.
#' @param Garantia Garantia desejada (valor contínuo).
#' @param SerieAfluEvap Data frame com séries de afluências (m³/s) e evaporações (mm/mês).
#' @param Voliniperc Percentual do volume útil inicial (padrão: 0.5).
#' @param onlyR Se TRUE, retorna apenas a retirada calculada. Caso contrário, retorna também a garantia ajustada.
#' @return Retirada calculada ou vetor com retirada e garantia ajustada.
#' @importFrom stats uniroot
#' @export
AcheRetirada <- function(Acude, Garantia, SerieAfluEvap, Voliniperc = 0.5, onlyR = TRUE, garantia_anual=FALSE) {

  if(garantia_anual){
    n_anos<- length(unique(year(SerieAfluEvap$Datas)))
    GarantiaPossivel <- DiscretizaGarantia(n_anos, Garantia)
  }else{
    GarantiaPossivel <- DiscretizaGarantia(nrow(SerieAfluEvap), Garantia)
  }


  f <- function(Ret) {
    return(FuncGarantia(Acude, Ret, SerieAfluEvap, Voliniperc, garantia_anual=garantia_anual) - GarantiaPossivel)
  }

  VolUtil <- Acude$Geometria$Vol_Max_Operacional - Acude$Geometria$Vol_Min
  Rmax <- VolUtil
  c <- 0

  while (FuncGarantia(Acude, Rmax, SerieAfluEvap, Voliniperc, garantia_anual=garantia_anual) > GarantiaPossivel && c < 10) {
    Rmax <- 1.5 * Rmax
    c <- c + 1
  }

  if (c == 10) {
    message(sprintf("Avaliar limite de busca para Acude %s (%s)",
                    Acude$Identif$Nome, Acude$Identif$ID))
  }

  resp <- uniroot(f, c(0, Rmax))
  R <- resp$root

  if (!onlyR) {
    G <- FuncGarantia(Acude, R, SerieAfluEvap, Voliniperc, garantia_anual=garantia_anual)
    return(c(R, G))
  } else {
    return(R)
  }
}

#' @title Simula Reservatório Mensalmente
#' @description Realiza a simulação do comportamento de um reservatório ao longo de uma série temporal mensal.
#' @param Acude Lista contendo informações do reservatório.
#' @param SerieAfluEvapRet Data frame com séries de afluências (m³/s), evaporações (mm/mês) e retiradas (m³/s).
#' @param Vol_ini_perc Percentual do volume útil inicial.
#' @return Data frame com resultados da simulação, incluindo volumes, retiradas e vertimentos.
#' @export
SimulaAcudeMensal <- function(Acude, SerieAfluEvapRet, Vol_ini_perc) {
  Vol_ini <- Acude$Geometria$Vol_Min +
    (Acude$Geometria$Vol_Max_Operacional - Acude$Geometria$Vol_Min) * Vol_ini_perc
  Vol <- Vol_ini

  Datas <- SerieAfluEvapRet$Datas
  SerieAflu <- SerieAfluEvapRet$Aflu * 3600 * 24 * 30 / 10^6 # Converte para hm³/mês
  SerieEvap <- SerieAfluEvapRet$Evap / 1000 # Converte para m/mês
  SerieRet <- SerieAfluEvapRet$Ret * 3600 * 24 * 30 / 10^6 # Converte para hm³/mês

  Vol_fimmes_hm3 <- numeric()
  Ret_m3s <- numeric()
  Ver_m3s <- numeric()

  for (i in 1:length(Datas)) {
    estado <- Dinamica_mensal_Simplificada(Vol, SerieAflu[i], SerieEvap[i], SerieRet[i], Acude$Geometria)
    Vol <- estado[1]

    Vol_fimmes_hm3[i] <- Vol
    Ret_m3s[i] <- estado[2] * 10^6 / (3600 * 24 * 30)
    Ver_m3s[i] <- estado[3] * 10^6 / (3600 * 24 * 30)
  }

  Sim.df <- data.frame(
    Datas = Datas,
    Aflu_m3s = SerieAflu * 10^6 / (3600 * 24 * 30),
    Vol_fimmes_hm3 = Vol_fimmes_hm3,
    Ret_m3s = Ret_m3s,
    Ver_m3s = Ver_m3s
  )

  return(Sim.df)
}




