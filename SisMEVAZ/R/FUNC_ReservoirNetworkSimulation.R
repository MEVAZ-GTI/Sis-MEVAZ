

#OBS: função requer metadados do arquivo FUNC_OperacaoReservatorios.R
#' Title
#'
#' @param AfluInc Lista nomeada(indice doas açudes) com a série de afluÇencias incrementais
#' (nomeadas com as datas). Todas as séries com o memso comprimento e correpontende às séries de Evap
#' @param EvapSerie Lista nomeada (indice doas açudes na mesma ordem que em AfluInc) com a série de
#'  evaporação dos lagos (nomeadas com as datas). Todas as séries com o memso comprimento e correpontende
#'  às séries de Evap
#' @param Ord_aflu Matriz com a primeira coluna referente aos índices dos açudes e com a segunda coluna
#' igual ao indice do reservatório imediatamente à jusante. Caso não haja reservatório a jusante o valor
#' deve ser 0.
#' @param ID_final Indice do reservatório final da cascata a ser simulada
#' @param vecVoliniperc Vetor com percentual da capacidade no inicio da simulação de cada reservatório.
#' Nomeado com o ID do reservatório na mesma ordem que AfluInc e EvapSerie.
#'
#' @return Resultados da simulação
#' @export
ReservoirNetworkSimulation<-function(ID_final,Garantia=0.9,AfluInc,EvapSerie,Ord_aflu,DadosRes, DadosCAV, vecVoliniperc, returnSim = T, garantia_anual=FALSE){

  cascata=MontaCascata(ID_final,Ord_aflu)

  #Ordens
  SimuAcu=list()
  Aflutot=list()
  QGarantia=numeric()
  ord=sort(unique(cascata[,2]))
  for(i in ord){
    id_or=cascata[cascata[,2]==i,1]
    for(id in id_or){
      #print(id)
      tmp=which(cascata[,1]==id)
      if(i>1){
        Aflutot[[tmp]]=AfluInc[[as.character(id)]]
        mon=Montantes(id,Ord_aflu)
        for(m in mon){
          tmp2=which(cascata[,1]==m)
          Aflu_m=Aflutot[[tmp2]]
          SimuAcu_m=SimuAcu[[tmp2]]
          Aflu_m[as.character(SimuAcu_m$Datas)]=SimuAcu_m$Ver_m3s

          Aflutot[[tmp]]=Aflutot[[tmp]]+Aflu_m
        }

      }else{
        Aflutot[[tmp]]=AfluInc[[as.character(id)]]
      }

      Acude=LerAcude(nID = id, DadosRes, DadosCAV)
      if(id==194){
        print("Castanhao simulado com:")
        print(" capacidade operacional = 4785,92 hm3 (cota 101 m - volume de espera de cerca de 2000 hm3)")
        print(" volume mínimo = 117,04 hm3 (cota 65 m)")
        Acude$Geometria$Vol_Max_Operacional=4785.92
        Acude$Geometria$Vol_Min=117.04
      }
      SerieAfluEvapRet=data.frame(Datas=as.Date(names(Aflutot[[tmp]])),
                                  Aflu=Aflutot[[tmp]],
                                  Evap=EvapSerie[[as.character(id)]])

      #Calcula QGarantia (Q90, Q95,...) com a série completa de afluencias
      QGarantia[tmp]=AcheRetirada(Acude, Garantia=Garantia,SerieAfluEvapRet,Voliniperc=vecVoliniperc[as.character(id)],onlyR = T, garantia_anual=garantia_anual)# já sai em m³/s

      SerieAfluEvapRet$Ret=rep(QGarantia[tmp],nrow(SerieAfluEvapRet))
      SimuAcu[[tmp]]=SimulaAcudeMensal(Acude,SerieAfluEvapRet,vecVoliniperc[as.character(id)])

    }

  }
  names(SimuAcu)=cascata[,1]
  names(Aflutot)=cascata[,1]
  names(QGarantia)=cascata[,1]


  if(returnSim){
    return(SimuAcu)
  }else{
    return(list(AfluInc=AfluInc[names(Aflutot)],Aflutot=Aflutot,SimuAcu=SimuAcu, Q90_m3_s=QGarantia))
  }

}

#' @title Simulação de Múltiplas Redes de Reservatórios
#' @description Realiza simulações em redes de reservatórios para diferentes níveis de garantia especificados.
#' @param vec_ID_final Vetor com os IDs dos reservatórios finais na rede.
#' @param vecGarantia Vetor com os níveis de garantia a serem simulados (valores entre 0 e 1). Padrão: `c(0.9, 0.95, 0.98)`.
#' @param AfluInc Matriz ou lista representando as séries de incrementos de afluência para os reservatórios.
#' @param EvapSerie Série temporal de taxas de evaporação (em mm/mês).
#' @param Ord_aflu Matriz com duas colunas: IDs dos reservatórios e seus jusantes.
#' @param DadosRes Lista ou estrutura contendo informações dos reservatórios (ex.: volumes máximos, mínimos, operacionais).
#' @param DadosCAV Dados relacionados às curvas de área-volume para os reservatórios.
#' @param vecVoliniperc Vetor com os percentuais de volume inicial para os reservatórios.
#' @return Lista de listas de simulações (`LLSimul`). Cada sublista corresponde a um nível de garantia e contém os resultados das simulações para cada reservatório da rede.
#' @export
#' @details
#' Para cada nível de garantia especificado em `vecGarantia`, a função realiza as seguintes operações:
#' - Itera pelos reservatórios finais fornecidos em `vec_ID_final`.
#' - Chama a função `ReservoirNetworkSimulation` para realizar a simulação da rede associada ao reservatório final com o nível de garantia especificado.
#' - Consolida os resultados em uma lista organizada por IDs dos reservatórios e níveis de garantia.
#' @seealso `ReservoirNetworkSimulation`
#' @examples
#' \dontrun{
#' vec_ID_final <- c(1, 2, 3)
#' vecGarantia <- c(0.9, 0.95)
#' AfluInc <- matrix(runif(100), nrow=10)
#' EvapSerie <- runif(10, 5, 15)
#' Ord_aflu <- matrix(c(1, 0, 2, 1, 3, 1), ncol=2, byrow=TRUE)
#' DadosRes <- list(Vol_Max = c(100, 200, 300), Vol_Min = c(10, 20, 30))
#' DadosCAV <- list()
#' vecVoliniperc <- c(0.5, 0.7, 0.6)
#' result <- MultiplReservoirNetworkSimulation(vec_ID_final, vecGarantia,
#'                                             AfluInc, EvapSerie, Ord_aflu,
#'                                             DadosRes, DadosCAV, vecVoliniperc)
#' }
MultiplReservoirNetworkSimulation <- function(vec_ID_final, vecGarantia = c(0.9, 0.95, 0.98),
                                              AfluInc, EvapSerie, Ord_aflu,
                                              DadosRes, DadosCAV, vecVoliniperc, garantia_anual=FALSE) {
  LLSimul <- lapply(X = vecGarantia, FUN = function(garan) {
    LSimul <- list()
    n_list0 <- 0
    for (id in vec_ID_final) {
      LSimuladd <- ReservoirNetworkSimulation(ID_final = id, Garantia = garan,
                                              AfluInc, EvapSerie, Ord_aflu,
                                              DadosRes, DadosCAV, vecVoliniperc, returnSim = TRUE, garantia_anual=garantia_anual)
      n_list <- n_list0 + length(LSimuladd)
      LSimul[(n_list0 + 1):n_list] <- LSimuladd
      n_list0 <- n_list
    }
    names(LSimul) <- FUNC_resRede_selec(Ord_aflu, vec_ID_final)
    LSimul <- LSimul[order(as.numeric(names(LSimul)))]
    return(LSimul)
  })
  names(LLSimul) <- paste0("Q", vecGarantia * 100)
  return(LLSimul)
}



#OBS: função requer metadados do arquivo FUNC_OperacaoReservatorios.R
#' Title
#'
#' @param AfluInc Lista nomeada(indice doas açudes) com a série de afluÇencias incrementais
#' (nomeadas com as datas). Todas as séries com o memso comprimento e correpontende às séries de Evap
#' @param EvapSerie Lista nomeada (indice doas açudes na mesma ordem que em AfluInc) com a série de
#'  evaporação dos lagos (nomeadas com as datas). Todas as séries com o memso comprimento e correpontende
#'  às séries de Evap
#' @param Ord_aflu Matriz com a primeira coluna referente aos índices dos açudes e com a segunda coluna
#' igual ao indice do reservatório imediatamente à jusante. Caso não haja reservatório a jusante o valor
#' deve ser 0.
#' @param ID
#'
#' @return Vazão regularizada
#' @export
QGarantia_withoutNet<-function(AfluInc,EvapSerie,Ord_aflu,ID,vecVoliniperc,Garantia=0.9,garantia_anual=FALSE){
  res=MontaCascata(ID,Ord_aflu)[,1]
  datas=names(AfluInc[[1]])
  Aflutot=sapply(1:length(datas),function(d){
    sum(sapply(as.character(res),function(r){AfluInc[[r]][d]}))
  })

  SerieAfluEvap=data.frame(Datas=as.Date(datas),
                              Aflu=Aflutot,
                              Evap=EvapSerie[[as.character(ID)]])
  Acude=LerAcude(ID, DadosRes, DadosCAV)
  if(ID==194){
    print("Castanhao simulado com:")
    print(" capacidade operacional = 4785,92 hm3 (cota 101 m - volume de espera de cerca de 2000 hm3)")
    print(" volume mínimo = 117,04 hm3 (cota 65 m)")
    Acude$Geometria$Vol_Max_Operacional=4785.92
    Acude$Geometria$Vol_Min=117.04
  }
  QGarantia=AcheRetirada(Acude, Garantia=Garantia,SerieAfluEvap,Voliniperc=vecVoliniperc[as.character(id)],onlyR = T,garantia_anual=garantia_anual)
  return(QGarantia)
}





