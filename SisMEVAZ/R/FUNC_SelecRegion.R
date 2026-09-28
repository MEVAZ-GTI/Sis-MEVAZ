
#' @title Seleção de Regionalização de Vazões para Bacias
#' @description Combina diferentes métodos de regionalização para a geração de séries de vazões incrementais de bacias hidrográficas.
#' @param AfluInc_KNN Lista contendo as séries de vazões incrementais geradas pelo método KNN (K-Nearest Neighbors).
#' @param AfluInc_ML Lista contendo as séries de vazões incrementais geradas pelo método ML (Machine Learning).
#' @param Aflu_CalLocal Lista contendo as séries de vazões geradas por simulação hidrológica com calibração local (dados do Balanço Reverso).
#' @param ModRegion Vetor indicando o modelo de regionalização selecionado para cada bacia. Pode ser "KNN", "ML", "Multimodelo" ou "Médias Regionais".
#' @param RegHidro Vetor indicando a região hidrográfica associada a cada bacia.
#' @param Prec_med Vetor com os valores médios de precipitação para cada bacia (em mm/mês).
#' @param Areas Vetor com as áreas de drenagem das bacias (em km²).
#' @param datMedReg Data frame contendo dados regionais médios, incluindo coeficiente de escoamento (CE) e coeficiente de variação (CV).
#' Deve conter as colunas `ID`, `CE` e `CV`, onde `ID` corresponde aos identificadores das bacias.
#' @return Lista `AfluInc` contendo as séries de vazões incrementais geradas para cada bacia, considerando o modelo de regionalização selecionado.
#' @export
#' @details
#' A função verifica a compatibilidade das séries de entrada `AfluInc_KNN` e `AfluInc_ML` e processa as bacias conforme o modelo de regionalização especificado:
#' - **KNN**: Utiliza diretamente a série de `AfluInc_KNN`.
#' - **ML**: Utiliza diretamente a série de `AfluInc_ML`.
#' - **Multimodelo**: Calcula a média das séries de `AfluInc_KNN` e `AfluInc_ML`.
#' - **Calibração Local (Balanço Reverso)**: Utiliza diretamente a série de `Aflu_CalLocal`.
#' - **Médias Regionais**: Gera uma série sintética baseada nos dados regionais médios (`datMedReg`) e nas características da bacia.
#'
#' Bacias com regionalização "Médias Regionais" requerem informações adicionais, como coeficientes de escoamento e variação, além de considerar as médias regionais das vazões incrementais de outras bacias da mesma região hidrográfica.
#'
#' Subfunções utilizadas:
#' - `FUNC_fragmentos`: Gera fragmentos baseados nas séries de vazões incrementais.
#' - `FUNC_SerSintecia`: Gera séries sintéticas de vazões incrementais com base em fragmentos e dados regionais.
#' @seealso `FUNC_fragmentos`, `FUNC_SerSintecia`
#' @examples
#' \dontrun{
#' AfluInc_KNN <- list(b1 = runif(12), b2 = runif(12))
#' AfluInc_ML <- list(b1 = runif(12), b2 = runif(12))
#' ModRegion <- c(b1 = "KNN", b2 = "Médias Regionais")
#' RegHidro <- c(b1 = "RH1", b2 = "RH1")
#' Prec_med <- c(b1 = 100, b2 = 120)
#' Areas <- c(b1 = 50, b2 = 60)
#' datMedReg <- data.frame(ID = c("b2"), CE = c(0.5), CV = c(0.2))
#' AfluInc <- FUNC_SelecRegion(AfluInc_KNN, AfluInc_ML, ModRegion, RegHidro,
#'                             Prec_med, Areas, datMedReg)
#' }
FUNC_SelecRegion <- function(AfluInc_KNN, AfluInc_ML, Aflu_CalLocal, ModRegion, RegHidro,
                             Prec_med, Areas, datMedReg) {
  if (length(AfluInc_KNN) != length(AfluInc_ML)) {
    stop("AfluInc_KNN e AfluInc_ML têm bacias de reservatórios diferentes")
  } else if (any(names(AfluInc_KNN) != names(AfluInc_ML))) {
    stop("AfluInc_KNN e AfluInc_ML têm bacias de reservatórios diferentes")
  }

  ids <- names(AfluInc_KNN)
  AfluInc <- list()

  for (i in 1:length(ids)) {
    id_i <- as.character(ids[i])
    modreg <- ModRegion[id_i]
    if (!is.element(modreg, c("KNN", "ML", "Multimodelo","Calibração Local (Balanço Reverso)", "Médias Regionais"))) {
      stop("Verificar Modelo de Regionalização Selecionado")
    }
    if (modreg == "KNN") {
      AfluInc[[i]] <- AfluInc_KNN[[id_i]]
    }
    if (modreg == "ML") {
      AfluInc[[i]] <- AfluInc_ML[[id_i]]
    }
    if (modreg == "Multimodelo") {
      AfluInc[[i]] <- (AfluInc_KNN[[id_i]] + AfluInc_ML[[id_i]]) / 2
    }
    if (modreg == "Calibração Local (Balanço Reverso)") {
      AfluInc[[i]] <- Aflu_CalLocal[[id_i]]
    }
    if (modreg == "Médias Regionais") {
      AfluInc[[i]] <- numeric()
    }
  }

  names(AfluInc) <- ids
  ids_MR <- ids[which(sapply(AfluInc, length) == 0)]
  dat_ini <- head(names(AfluInc_KNN[[1]]), 1)
  dat_fin <- tail(names(AfluInc_KNN[[1]]), 1)

  if (length(ids_MR) > 0) {
    for (i in 1:length(ids_MR)) {
      RegHidro_i <- RegHidro[ids_MR[i]]
      Prec <- Prec_med[ids_MR[i]]
      Area <- Areas[ids_MR[i]]
      CE <- datMedReg$CE[datMedReg$ID == ids_MR[i]]
      CV <- datMedReg$CV[datMedReg$ID == ids_MR[i]]
      ids_reg <- names(RegHidro)[RegHidro == RegHidro_i]
      ids_reg <- ids_reg[!is.element(ids_reg, ids_MR)]
      matfrag <- FUNC_fragmentos(LVaz = AfluInc[ids_reg])
      frag_med <- apply(matfrag, 2, mean)
      AfluInc[[ids_MR[i]]] <- FUNC_SerSintecia(vecFrag = frag_med, Precmed = Prec,
                                               Area = Area, CE = CE, CV = CV,
                                               dat_ini = dat_ini, dat_fin = dat_fin)
    }
  }

  return(AfluInc)
}
