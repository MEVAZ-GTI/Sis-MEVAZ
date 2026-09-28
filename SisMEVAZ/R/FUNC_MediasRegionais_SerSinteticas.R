#' Calcular Fragmentos Mensais de Séries de Vazão
#'
#' Essa função calcula os fragmentos mensais de séries de vazão, que são as proporções médias da vazão mensal em relação à vazão anual média.
#'
#' @param LVaz Lista de séries temporais de vazão (`numeric`), onde cada elemento representa uma série de dados mensais de vazão.
#' @return Uma matriz onde cada linha representa uma série de vazão e cada coluna, o fragmento médio para cada mês (valores normalizados em relação à vazão anual média).
#' @import lubridate
#' @export
FUNC_fragmentos <- function(LVaz) {

  # Calcula a soma anual da vazão para cada série
  Vaz_somaano <- Agg_anual(LVaz, func_agg = sum)

  # Calcula a vazão média anual para cada série
  Vaz_somaano_med <- sapply(Vaz_somaano, mean)

  # Inicializa a matriz para armazenar os fragmentos mensais
  Matfrag <- numeric()

  # Para cada série de vazão
  for (i in 1:length(LVaz)) {
    serie <- LVaz[[i]]

    # Calcula os fragmentos mensais normalizados pela vazão anual média
    vecFrag <- sapply(1:12, function(m) {
      mean(serie[month(as.Date(names(serie))) == m])
    }) / Vaz_somaano_med[i]

    # Adiciona os fragmentos à matriz
    Matfrag <- rbind(Matfrag, vecFrag)
  }

  # Define os nomes das linhas da matriz como os nomes das séries de entrada
  rownames(Matfrag) <- names(LVaz)

  return(Matfrag)
}

#' Gerar Série Sintética de Vazão
#'
#' Essa função gera uma série sintética de vazão mensal com base em parâmetros fornecidos e fragmentos mensais.
#'
#' @param vecFrag Vetor de fragmentos mensais, representando as proporções médias da vazão mensal em relação à vazão anual média.
#' @param Precmed Precipitação média anual (em mm).
#' @param Area Área da bacia hidrográfica (em km²).
#' @param CE Coeficiente de escoamento (adimensional).
#' @param CV Coeficiente de variação da vazão anual (adimensional).
#' @param dat_ini Data de início da série (formato "yyyy-mm-dd").
#' @param dat_fin Data de término da série (formato "yyyy-mm-dd").
#' @return Um vetor numérico com a série sintética de vazão mensal, onde os nomes do vetor são as datas correspondentes.
#' @export
FUNC_SerSintecia <- function(vecFrag, Precmed, Area, CE, CV, dat_ini, dat_fin) {

  # Gera as datas mensais e anuais dentro do intervalo especificado
  Datas <- seq(as.Date(dat_ini), as.Date(dat_fin), by = "months")
  Datas_an <- seq(as.Date(dat_ini), as.Date(dat_fin), by = "years")

  # Determina os meses correspondentes às datas
  meses <- month(Datas)

  # Calcula a média anual da vazão com base nos parâmetros fornecidos
  media <- Precmed * CE * Area * (1000 / (3600 * 24 * 365)) # Conversão para m³/s

  # Calcula os parâmetros da distribuição gamma para geração da série
  sd2 <- (CV * media)^2
  scale <- sd2 / media
  shape <- media / scale

  # Gera a série de vazão anual usando a distribuição gamma
  set.seed(1)
  vaz_anual <- rgamma(n = length(Datas_an), shape = shape, scale = scale)

  # Expande a vazão anual para uma série mensal usando os fragmentos
  vaz_menal <- unlist(lapply(vaz_anual, function(v) {
    12 * v * vecFrag
  }))

  # Define os nomes do vetor como as datas correspondentes
  names(vaz_menal) <- Datas

  return(vaz_menal)
}
