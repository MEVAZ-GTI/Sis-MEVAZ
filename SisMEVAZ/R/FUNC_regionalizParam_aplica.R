

# Função para encontrar as estações mais próximas usando KNN
#' Encontra as estações mais próximas de acordo com características selecionadas
#'
#' Esta função utiliza o método K-Nearest Neighbors (KNN) para identificar as cinco
#' estações mais próximas em termos de características selecionadas e ponderadas.
#'
#' @param CaracEst Data.frame com as características das estações.
#' @param CaracAcu Data.frame com as características acumuladas.
#' @param carac_compara Vetor com os nomes das características a serem usadas na comparação.
#' @param pesos_carac Vetor de pesos para ponderação das características.
#' @param n_viz Número de vizinhos (default = 5)
#'
#' @return Lista com as estações mais próximas para cada entrada em CaracAcu.
#' @export
#'
#' @examples
#' CaracEst <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
#' CaracAcu <- data.frame(A = rnorm(3), B = rnorm(3), C = rnorm(3))
#' carac_compara <- c("A", "B", "C")
#' pesos_carac <- c(A = 1, B = 1, C = 1)
#' FUNC_estproxKNN(CaracEst, CaracAcu, carac_compara, pesos_carac)
FUNC_estproxKNN <- function(CaracEst, CaracAcu, carac_compara, pesos_carac, n_viz=5) {

  # Verifica se as características estão presentes em CaracEst
  if(any(!is.element(carac_compara, colnames(CaracEst)))){
    stop("Característica não presente em CaracEst (verificar nomes das colunas)")
  }

  # Verifica se as características estão presentes em CaracAcu
  if(any(!is.element(carac_compara, colnames(CaracAcu)))){
    stop("Característica não presente em CaracAcu (verificar nomes das colunas)")
  }

  # Verifica se todas as características possuem um peso atribuído
  if(any(!is.element(carac_compara, names(pesos_carac)))){
    stop("Característia sem peso atribuído (verificar nomes dos elementos do vetor de pesos)")
  }

  # Seleciona características usadas na comparação entre as estações
  CaracEst <- CaracEst[, carac_compara]
  CaracAcu <- CaracAcu[, carac_compara]

  # Reduz as variáveis para que tenham média 0 e desvio padrão 1
  CaracEstRed <- apply(CaracEst, 2, function(c) { (c - mean(c)) / sd(c) })
  CaracAcuRed <- sapply(1:ncol(CaracAcu), function(j) { (CaracAcu[, j] - mean(CaracEst[, j])) / sd(CaracEst[, j]) })
  colnames(CaracAcuRed) <- carac_compara

  # Aplica os pesos às características
  CaracEstPond <- t(apply(CaracEstRed, 1, function(x) { x * abs(pesos_carac[carac_compara]) }))
  CaracAcuPond <- t(apply(CaracAcuRed, 1, function(x) { x * abs(pesos_carac[carac_compara]) }))

  # Calcula as estações mais próximas para cada entrada em CaracAcu
  est_prox_ac <- lapply(1:nrow(CaracAcuPond), function(i) {
    x <- CaracAcuPond[i, ]
    vdist <- apply(CaracEstPond, 1, function(y) { dist(rbind(x, y)) })
    return(names(vdist)[order(vdist)[1:n_viz]])  # Retorna os nomes das cinco estações mais próximas
  })

  names(est_prox_ac) <- rownames(CaracAcu)  # Atribui os nomes das estações acumuladas
  return(est_prox_ac)  # Retorna a lista de estações mais próximas
}


# Função para calcular as distâncias para as estações mais próximas usando KNN
#' Calcula as distâncias das cinco estações mais próximas de acordo com características selecionadas
#'
#' Esta função utiliza o método K-Nearest Neighbors (KNN) para calcular as distâncias
#' das cinco estações mais próximas em termos de características selecionadas e ponderadas.
#'
#' @param CaracEst Data.frame com as características das estações.
#' @param CaracAcu Data.frame com as características acumuladas.
#' @param carac_compara Vetor com os nomes das características a serem usadas na comparação.
#' @param pesos_carac Vetor de pesos para ponderação das características.
#' @param n_viz Número de vizinhos (default = 5)
#'
#' @return Lista com as distâncias das cinco estações mais próximas para cada entrada em CaracAcu.
#' @export
#'
#' @examples
#' CaracEst <- data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10))
#' CaracAcu <- data.frame(A = rnorm(3), B = rnorm(3), C = rnorm(3))
#' carac_compara <- c("A", "B", "C")
#' pesos_carac <- c(A = 1, B = 1, C = 1)
#' FUNC_distKNN(CaracEst, CaracAcu, carac_compara, pesos_carac)
FUNC_distKNN <- function(CaracEst, CaracAcu, carac_compara, pesos_carac, n_viz=5) {

  # Verifica se as características estão presentes em CaracEst
  if(any(!is.element(carac_compara, colnames(CaracEst)))){
    stop("Característica não presente em CaracEst (verificar nomes das colunas)")
  }

  # Verifica se as características estão presentes em CaracAcu
  if(any(!is.element(carac_compara, colnames(CaracAcu)))){
    stop("Característica não presente em CaracAcu (verificar nomes das colunas)")
  }

  # Verifica se todas as características possuem um peso atribuído
  if(any(!is.element(carac_compara, names(pesos_carac)))){
    stop("Característia sem peso atribuído (verificar nomes dos elementos do vetor de pesos)")
  }

  # Seleciona características usadas na comparação entre as estações
  CaracEst <- CaracEst[, carac_compara]
  CaracAcu <- CaracAcu[, carac_compara]

  # Reduz as variáveis para que tenham média 0 e desvio padrão 1
  CaracEstRed <- apply(CaracEst, 2, function(c) { (c - mean(c)) / sd(c) })
  CaracAcuRed <- sapply(1:ncol(CaracAcu), function(j) { (CaracAcu[, j] - mean(CaracEst[, j])) / sd(CaracEst[, j]) })
  colnames(CaracAcuRed) <- carac_compara

  # Aplica os pesos às características
  CaracEstPond <- t(apply(CaracEstRed, 1, function(x) { x * abs(pesos_carac[carac_compara]) }))
  CaracAcuPond <- t(apply(CaracAcuRed, 1, function(x) { x * abs(pesos_carac[carac_compara]) }))

  # Calcula as distâncias para as estações mais próximas para cada entrada em CaracAcu
  est_prox_dist_ac <- lapply(1:nrow(CaracAcuPond), function(i) {
    x <- CaracAcuPond[i, ]
    vdist <- apply(CaracEstPond, 1, function(y) { dist(rbind(x, y)) })
    return(sort(vdist)[1:n_viz])  # Retorna as distâncias das cinco estações mais próximas
  })

  names(est_prox_dist_ac) <- rownames(CaracAcu)  # Atribui os nomes das estações acumuladas
  return(est_prox_dist_ac)  # Retorna a lista de distâncias para as estações mais próximas
}


#' @title Obtém Parâmetros KNN
#' @description Seleciona os parâmetros das estações mais próximas para o método KNN.
#' @param est_prox Vetor com os índices ou identificadores das estações mais próximas.
#' @param ParamEstFlu Data frame contendo os parâmetros das estações fluviométricas.
#' @return Data frame com os parâmetros das estações selecionadas.
#' @export
FUNC_paramKNN <- function(est_prox, ParamEstFlu) {
  ParamKNN <- sapply(est_prox, function(est) {
    ParamEstFlu[est, ]
  })
  ParamKNN <- t(ParamKNN) # Transforma para formato de tabela
  return(ParamKNN)
}

#' @title Calcula Parâmetros por Modelo Linear
#' @description Calcula os parâmetros para modelagem hidrológica usando características do solo e dados de precipitação.
#' @param Carac Data frame com características das estações (e.g., "Cristalino", "CAD_mm").
#' @param Prec_med Vetor com a precipitação média anual das estações.
#' @return Data frame com os parâmetros calculados: SAT, PES, CREC e K.
#' @export
FUNC_paramML <- function(Carac, Prec_med) {
  ids <- rownames(Carac) # Obtém os identificadores das estações
  ParamML <- sapply(ids, function(id) {
    # Calcula parâmetros baseados nas características e precipitação
    SAT <- 3213.4 - 22.9 * Carac[id, "Cristalino"] * 100
    PES <- -0.888 + 0.0034 * Prec_med[id] + 0.041 * Carac[id, "CAD_mm"]
    CREC <- 0
    K <- 3

    # Define limites para os parâmetros calculados
    if (SAT < 400) { SAT <- 400 }
    if (SAT > 5000) { SAT <- 5000 }
    if (PES < 0.1) { PES <- 0.1 }
    if (PES > 10) { PES <- 10 }

    return(c(SAT, PES, CREC, K))
  })

  ParamML <- t(ParamML) # Transforma para formato de tabela
  colnames(ParamML) <- c("SAT", "PES", "CREC", "K") # Define nomes das colunas
  return(ParamML)
}
