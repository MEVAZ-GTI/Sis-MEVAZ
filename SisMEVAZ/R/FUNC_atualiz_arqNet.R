
#' Atualiza Matriz de Características
#'
#' Atualiza a matriz de características de reservatórios com base em novos dados fornecidos.
#'
#' @param mat_tmp Data frame. Matriz com características dos novos reservatórios.
#' @param mat_consol Data frame. Matriz consolidada com características de reservatórios existentes.
#' @param res_tmp Vetor. IDs dos novos reservatórios.
#' @param res_consol Vetor. IDs dos reservatórios existentes.
#'
#' @return Data frame. Matriz consolidada atualizada com os novos dados.
#' @export
#'
#' @examples
#' # Exemplo de uso
#' nova_matriz <- FUNC_atualiz_mat(mat_tmp, mat_consol, res_tmp, res_consol)

FUNC_atualiz_mat <- function(mat_tmp, mat_consol, res_tmp, res_consol) {

  # verificação
  if (length(res_tmp) != nrow(mat_tmp)) {
    stop("Reservatórios em res_tmp não correspondem às linhas de mat_tmp")
  }
  if (length(res_consol) != nrow(mat_consol)) {
    stop("Reservatórios em res_consol não correspondem às linhas de mat_consol")
  }

  res_n <- res_tmp # reservatórios da rede-teste que tiveram suas características calculadas
  res_j <- res_n[is.element(res_n, res_consol)] # reservatórios que já percentiam a rede consolidada, mas têm suas características alteradas.

  # Atualiza matriz de caractísticas para a rede-teste
  if (length(res_j) > 0) {
    selec <- which(!is.element(res_consol, res_j))
    mat_consol <- mat_consol[selec, ]
    res_consol <- res_consol[selec]
  }

  mat_teste <- rbind(mat_consol, mat_tmp)

  # Reordena linhas da matriz de características de acordo com o ID
  ordem <- order(as.numeric(c(res_consol, res_tmp)))
  mat_teste <- mat_teste[ordem, ]
  return(mat_teste)
}

#' Atualiza Lista ou Vetor de Dados
#'
#' Atualiza uma lista ou vetor de dados com base em novos valores fornecidos.
#'
#' @param List_tmp Lista ou vetor. Dados dos novos reservatórios.
#' @param List_consol Lista ou vetor. Dados consolidados de reservatórios existentes.
#'
#' @return Lista ou vetor. Dados consolidados atualizados com os novos valores.
#' @export
#'
#' @examples
#' # Exemplo de uso
#' nova_lista <- FUNC_atualiz_lista_vec(List_tmp, List_consol)
#'
FUNC_atualiz_lista_vec <- function(List_tmp, List_consol) {
  res_n <- names(List_tmp) # reservatórios da rede-teste com dados novos ou atualizados
  List_teste <- List_consol # Atualiza lista/vetor
  List_teste[res_n] <- List_tmp
  ordem <- order(as.numeric(names(List_teste))) # Reordena elementos da lista/vetor características de acordo com o ID
  List_teste <- List_teste[ordem]
  return(List_teste)
}


