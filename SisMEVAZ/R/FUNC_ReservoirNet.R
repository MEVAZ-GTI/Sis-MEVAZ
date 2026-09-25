#' @title Simplifica Lista de Jusantes
#' @description Reorganiza a lista de jusantes considerando apenas os reservatórios predefinidos (com dados disponíveis).
#' @param Ord_aflu Matriz com duas colunas: a primeira contendo os IDs dos reservatórios e a segunda os IDs dos jusantes.
#' @param IDres Vetor de IDs dos reservatórios predefinidos.
#' @return Matriz simplificada com jusantes considerando apenas os reservatórios predefinidos.
#' @export
Simpl.Jusantes <- function(Ord_aflu, IDres) {
  IDret <- Ord_aflu[!is.element(Ord_aflu[, 1], IDres), 1]
  for (i in IDret) {
    pos <- Ord_aflu[Ord_aflu[, 1] == i, 2]
    Ord_aflu[Ord_aflu[, 2] == i, 2] <- pos
  }
  Ord_aflu <- Ord_aflu[is.element(Ord_aflu[, 1], IDres), ]
  if (length(Ord_aflu) == 2) {
    Ord_aflu <- matrix(Ord_aflu, 1, 2)
  }
  return(Ord_aflu)
}

#' @title Identifica Montantes Imediatos
#' @description Retorna os reservatórios imediatamente a montante de um dado reservatório.
#' @param nID ID do reservatório principal.
#' @param Ord_aflu Matriz com duas colunas: IDs dos reservatórios e seus jusantes.
#' @return Vetor de IDs dos reservatórios imediatamente a montante.
#' @export
Montantes <- function(nID, Ord_aflu) {
  Ord_aflu[is.na(Ord_aflu[, 2]), 2] <- 0
  reservs <- Ord_aflu[is.element(Ord_aflu[, 2], nID), 1]
  return(reservs)
}

#' @title Calcula Ordem de Operação N
#' @description Determina o vetor de reservatórios com ordem de operação imediatamente superior.
#' @param ReservOrdemN Vetor de IDs de reservatórios com ordem atual.
#' @param ReservOrdensMenores Vetor de IDs de reservatórios com ordens inferiores.
#' @param nID ID do reservatório principal.
#' @param Ord_aflu Matriz com duas colunas: IDs dos reservatórios e seus jusantes.
#' @return Vetor de IDs dos reservatórios com ordem de operação superior.
#' @export
AcheOrdemN <- function(ReservOrdemN, ReservOrdensMenores, nID, Ord_aflu) {
  vecMont <- Montantes(nID, Ord_aflu)
  if (is.element(FALSE, is.element(vecMont, ReservOrdensMenores))) {
    for (i in 1:length(vecMont)) {
      if (!is.element(vecMont[i], ReservOrdensMenores)) {
        ReservOrdemN <- AcheOrdemN(ReservOrdemN, ReservOrdensMenores, vecMont[i], Ord_aflu)
      }
    }
  } else {
    ReservOrdemN <- c(ReservOrdemN, nID)
  }
  return(ReservOrdemN)
}

#' @title Monta Cascata de Reservatórios
#' @description Gera a lista de reservatórios a montante e suas ordens de operação.
#' @param nID ID do reservatório principal.
#' @param Ord_aflu Matriz com duas colunas: IDs dos reservatórios e seus jusantes.
#' @return Matriz com os IDs dos reservatórios e suas respectivas ordens de operação.
#' @export
MontaCascata <- function(nID, Ord_aflu) {
  reservs <- integer(0)
  reservsSup <- AcheOrdemN(integer(0), integer(0), nID, Ord_aflu)
  ordens <- rep(1, length(reservsSup))
  reservs <- c(reservs, reservsSup)
  i <- 1
  while (is.element(FALSE, is.element(reservsSup, nID))) {
    i <- i + 1
    reservsSup <- AcheOrdemN(integer(0), reservs, nID, Ord_aflu)
    ordens <- c(ordens, rep(i, length(reservsSup)))
    reservs <- c(reservs, reservsSup)
  }
  cascata <- matrix(data = c(reservs, ordens), nrow = length(reservs), ncol = 2)
  return(cascata)
}

#' @title Seleciona Reservatórios Finais Modificados
#' @description Identifica reservatórios finais de rede que fazem parte de cascatas com reservatórios modificados.
#' @param Ord_aflu Matriz com duas colunas: IDs dos reservatórios e seus jusantes.
#' @param res_mod Vetor com IDs dos reservatórios modificados.
#' @return Vetor de IDs dos reservatórios finais selecionados.
#' @export
FUNC_resFimRede_selec <- function(Ord_aflu, res_mod) {
  res_final <- Ord_aflu[Ord_aflu[, 2] == 0, 1]
  sel_fim <- sapply(res_final, function(i) {
    cas <- MontaCascata(i, Ord_aflu)
    pertence <- any(is.element(res_mod, cas[, 1]))
    return(pertence)
  }) %>% which
  res_final_selec <- res_final[sel_fim]
  return(res_final_selec)
}

#' @title Seleciona Reservatórios de Rede
#' @description Retorna os IDs de todos os reservatórios que pertencem a cascatas selecionadas.
#' @param Ord_aflu Matriz com duas colunas: IDs dos reservatórios e seus jusantes.
#' @param resFimRede_selec Vetor com IDs dos reservatórios finais selecionados.
#' @return Vetor de IDs dos reservatórios que fazem parte das cascatas selecionadas.
#' @export
FUNC_resRede_selec <- function(Ord_aflu, resFimRede_selec) {
  Cas <- lapply(resFimRede_selec, MontaCascata, Ord_aflu = Ord_aflu)
  resRede_selec <- do.call(c, lapply(Cas, function(x) { x[, 1] }))
  return(resRede_selec)
}

