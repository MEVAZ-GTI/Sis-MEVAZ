#' Funcao para criar um objeto com os parametros constantes para rodar o SMAP
#'
#' @param SAT Capacidade maxima do solo (400 < SAT <5000)
#' @param PES Parametro de Escoamento (0.1 < PES < 10)
#' @param CREC Parametro de Recarga (0 < CREC < 70)
#' @param K Constante (1 < K < 6)
#'
#' @return param List contendo esses parametros nomeados
#' @export
SMAPMes.param <- function(SAT, PES, CREC, K){

  if(is.na(SAT) | is.na(PES) | is.na(CREC) | is.na(K)){
    stop("Valor invalido para um dos parametros SAT, PES, CREC ou K")
  }

  #Teste dos limites dos par?metros
  if((SAT > 5000) | (SAT < 400)){
    stop("SAT fora dos limites (400 < SAT <5000)")
  }

  if((PES > 10) | (PES < 0.1)){
    stop("PES fora dos limites (0.1 < PES < 10)")
  }

  if((CREC > 70) | (CREC < 0)){
    stop("CREC fora dos limites (0 < CREC < 70)")
  }

  if((K > 6) | (K < 1)){
    stop("K fora dos limites (1 < K < 6)")
  }

  param <- list(SAT,PES,CREC,K)
  names(param) <- c("SAT","PES","CREC","K")

  return(param)
}

#' Funcao para criar objeto com os valores iniciais para rodar o SMAP
#'
#' @param Area Area da bacia (km2)
#' @param EbInic Vazao Basica Inicial em m3/s (0.95 da vazao minima), default = 0
#' @param TuInic Teor de Umidade inicial, default = 0.3
#'
#' @return List contendo esses parametros
#' @export
SMAPMes.inic <- function(Area, EbInic, TuInic){
  Ebd <- 0
  Tud <- 0.3

  if(missing(Area)){
    stop("Sem valor para area")
  }

  if(!missing(EbInic)){
    Ebd <- EbInic
  }

  if(!missing(TuInic)){
    Tud <- TuInic
  }

  inic <- list(Area, Ebd, Tud)
  names(inic) <- c("Area", "EbInic", "TuInic")
  return(inic)
}

#' Funcao para o calculo do SMAP mensal
#'
#' @param param Objeto param obtido pela funcao SMAP.param
#' @param inic  Objeto inic obtido pela funcao SMAP.inic
#' @param ETo Valor de ETo do mes
#' @param P Valor de precipitacao do mes
#' @param mRbind Matriz com iteracoes passadas, onde os resultados do mes serao adicionados,
#' contendo os valores de Rsolo e Rsub da itera??o anterio
#' @param dataM [Opcional] Nomea a linha da matriz de saida com a data fornecida
#' @param mComp [Opcional] Altera a matriz de saida, adicionando os resultados das variaveis utilizadas
#' no calculo
#'
#' @return Matriz com a vaz?o calculada e par?metros Rsolo e Rsub necess?rios para proxima iteracao
#' @export
SMAPMes.rodadaMensal <- function(param, inic, ETo, P, mRbind, dataM, mComp){
  SAT <- param$SAT
  PES <- param$PES
  CREC <- param$CREC
  K <- param$K

  Ks <- 0.5^(1/K)

  Area <- inic$Area
  EbInic <- inic$EbInic
  TuInic <- inic$TuInic

  #Se nao for fornecido nenhuma matriz no mRBind calcular os valores iniciais de Rsolo e Rsub
  if(missing(mRbind)){
    RsoloInic <- SAT*TuInic
    RsubInic <- (EbInic/(1-Ks))*2630/Area
  }else{
    tmp <- nrow(mRbind)
    RsoloInic <- mRbind[tmp,2]
    RsubInic <- mRbind[tmp,3]
  }

  #Calculo das variaveis utilizadas baseadas no RSoloInic
  Tuo <- RsoloInic/SAT
  Dsolo <- 0.5*( P -P*(Tuo^PES) -ETo*Tuo -RsoloInic*(CREC/100)*(Tuo^4) )
  Tu <-(RsoloInic + Dsolo)/SAT

  if(Tu > 1 ){
    Tu <- 1
  }else{
    if(Tu <  0){
      Tu <- 0
    }
  }

  Es <- P*(Tu^PES)
  Er <- ETo*Tu
  Rec <- RsoloInic*(CREC/100)*(Tu^4)
  Rsolo <- RsoloInic + P - Es - Er -Rec

  if(Rsolo > SAT){
    Es <- Es + Rsolo - SAT
    Rsolo <- SAT
  }else{
    if(Rsolo < 0){
      Rsolo <- 0
    }
  }

  #Calculo das variaveis utilizadas baseadas no RSubInic
  Eb <- RsubInic*(1-Ks)
  Rsub <- RsubInic -Eb+Rec

  #Calculo da vazao
  Qcalc <- (Es+Eb)*(Area)/2630

  #Matriz de Saida
  mCompD <- F

  if(!missing(mComp)){
    mCompD <- mComp
  }

  if(mCompD){
    matrizSaida <- matrix(nrow = 1, ncol = 10)
    colnames(matrizSaida) <- c("Qcalc","Rsolo","Rsub","Tuo","Dsolo","Tu","Es","Er","Rec","Eb")
    matrizSaida[1,1]<-Qcalc
    matrizSaida[1,2]<-Rsolo
    matrizSaida[1,3]<-Rsub
    matrizSaida[1,4]<-Tuo
    matrizSaida[1,5]<-Dsolo
    matrizSaida[1,6]<-Tu
    matrizSaida[1,7]<-Es
    matrizSaida[1,8]<-Er
    matrizSaida[1,9]<-Rec
    matrizSaida[1,10]<-Eb
  }else{
    matrizSaida <- matrix(nrow = 1, ncol = 3)
    colnames(matrizSaida) <- c("Qcalc","Rsolo","Rsub")
    matrizSaida[1,1]<-Qcalc
    matrizSaida[1,2]<-Rsolo
    matrizSaida[1,3]<-Rsub
  }

  if(!missing(dataM)){
    row.names(matrizSaida)<-dataM
  }

  #Adiciona a saida dessa iteracao como linha da matriz fornecida
  if(!missing(mRbind)){

    if(ncol(matrizSaida) != ncol(mRbind)){
      stop("Matrizes de tamanhos diferentes, alterar mRbind ou mComp")
    }

    matrizSaida <- rbind(mRbind,matrizSaida)
  }

  return(matrizSaida)
}

#' Funcao para calcular uma serie de vazoes mensais (m3s)
#'
#' @param param Objeto do tipo SMAP.param
#' @param inic Objeto do tipo SMAP.inic
#' @param SerieETo Serie mensal de ETo (mm)
#' @param SerieP Serie Mensal de P (mm)
#' @param dataM Serie com as datas das observacoes
#'
#' @return Vetor com as series de vaz?es
#' @export
SMAPMes.serieMensalQ <- function(param, inic, SerieETo, SerieP, dataM){
  resultados <- SMAPMes.rodadaMensal(param = param,inic = inic,ETo = SerieETo[1],
                                  P = SerieP[1],dataM = dataM[1])
  for(i in 2:length(SerieP)){
    tmp <- nrow(resultados)
    resultados <- SMAPMes.rodadaMensal(param = param, inic = inic,ETo = SerieETo[i], P= SerieP[i],dataM=dataM[i],
                                    mRbind = resultados)
  }
  return(resultados[,1])
}

#' Serie de vazoes a partir de entrada vetorial
#'
#' @param v vetor com os parametros do SMAP
#' @param inic Objeto do tipo SMAP.inic
#' @param SerieETo Serie mensal de ETo (mm)
#' @param SerieP Serie Mensal de P (mm)
#' @param dataM Serie com as datas das observacoes
#'
#' @return Vetor com as series de vazoes
#' @export
SMAPMes.serieMensalQvet <- function(v, inic, SerieETo, SerieP, dataM){
  param <- SMAPMes.param(SAT = v[1], PES = v[2], CREC= v[3], K=v[4])
  Qcalc <- SMAPMes.serieMensalQ(param = param,inic = inic,SerieETo = SerieETo,SerieP = SerieP,dataM = dataM)
  return(Qcalc)
}


#' @title Geração de Séries de Vazão com o Modelo SMAP
#' @description Calcula as séries de vazão incremental para uma lista de bacias utilizando o modelo SMAP.
#' @param matParam Data frame ou matriz com os parâmetros do modelo SMAP (SAT, PES, CREC, K) para cada bacia.
#' Cada linha corresponde a uma bacia, e as colunas devem ser nomeadas como "SAT", "PES", "CREC" e "K".
#' @param vecArea Vetor nomeado com as áreas de drenagem das bacias (em km²). Os nomes devem corresponder aos IDs das bacias.
#' @param lPrec Lista com as séries temporais de precipitação média mensal (em mm) para cada bacia.
#' Os nomes da lista devem corresponder aos IDs das bacias.
#' @param lETo_med Lista com os valores médios mensais de evapotranspiração potencial (ETO) para cada bacia.
#' Cada elemento deve ser um vetor numérico de 12 valores (um para cada mês).
#' @param ids Vetor com os identificadores das bacias a serem processadas.
#' @return Lista de séries de vazões incrementais para cada bacia, onde cada elemento da lista corresponde ao ID da bacia.
#' @export
#' @details
#' A função aplica o modelo SMAP para calcular as séries de vazão incremental a partir de precipitação e evapotranspiração.
#'
#' Para cada bacia:
#' 1. Inicializa os parâmetros do modelo com a função `SMAPMes.inic`.
#' 2. Define os parâmetros específicos da bacia com a função `SMAPMes.param`.
#' 3. Calcula a série de vazão incremental com a função `SMAPMes.serieMensalQ`.
#'
#' As funções auxiliares utilizadas são assumidas como implementações do modelo SMAP:
#' - `SMAPMes.inic`: Inicializa as condições iniciais do modelo.
#' - `SMAPMes.param`: Define os parâmetros do modelo SMAP.
#' - `SMAPMes.serieMensalQ`: Calcula a série mensal de vazão incremental.
#'
#' A área de drenagem (`vecArea`) é utilizada para ajustar as condições iniciais do modelo.
#' @seealso `SMAPMes.inic`, `SMAPMes.param`, `SMAPMes.serieMensalQ`
#' @examples
#' \dontrun{
#' # Exemplo de parâmetros
#' matParam <- data.frame(SAT = c(500, 600), PES = c(0.5, 0.7),
#'                        CREC = c(0, 0), K = c(3, 4))
#' rownames(matParam) <- c("b1", "b2")
#' vecArea <- c(b1 = 50, b2 = 70)
#'
#' # Precipitação e ETO médias
#' lPrec <- list(b1 = runif(12, 50, 100), b2 = runif(12, 60, 120))
#' lETo_med <- list(b1 = runif(12, 3, 5), b2 = runif(12, 4, 6))
#'
#' # IDs das bacias
#' ids <- c("b1", "b2")
#'
#' # Calcular séries de vazões
#' lvaz <- SMAP.listaBac(matParam, vecArea, lPrec, lETo_med, ids)
#' }
SMAP.listaBac <- function(matParam, vecArea, lPrec, lETo_med, ids) {
  ids <- as.character(ids)
  lvaz <- lapply(ids, function(id) {
    P <- lPrec[[id]]
    ETO <- sapply(month(names(P)), function(m) { lETo_med[[id]][m] })

    inic <- SMAPMes.inic(Area = vecArea[id], EbInic = 0, TuInic = 0.3)
    param <- SMAPMes.param(SAT = matParam[id, "SAT"],
                           PES = matParam[id, "PES"],
                           CREC = matParam[id, "CREC"],
                           K = matParam[id, "K"])

    vaz <- SMAPMes.serieMensalQ(param, inic, ETO, P, names(P))
    return(vaz)
  })
  names(lvaz) <- ids
  return(lvaz)
}

