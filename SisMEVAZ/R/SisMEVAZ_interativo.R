#' Inicia a interface interativa do Sis-MEVAZ
#'
#' Abre no navegador a interface Shiny instalada junto com o pacote.
#'
#' @param diretorio_de_dados Diretório que contém a pasta `dados`. Por padrão,
#'   utiliza o diretório de trabalho atual.
#' @param navegador Se deve abrir automaticamente o navegador.
#' @param porta Porta local utilizada pelo servidor Shiny.
#' @param endereco Endereço em que o servidor escuta. O padrão é somente local.
#'
#' @return Retorna invisivelmente o resultado de [shiny::runApp()].
#' @export
SisMEVAZ_interativo <- function(diretorio_de_dados = getwd(),
                                navegador = TRUE,
                                porta = 8082,
                                endereco = "127.0.0.1") {
  dependencias <- c(
    "shiny", "bslib", "leaflet", "plotly", "DT", "shinyjs",
    "readxl", "stars", "whitebox"
  )
  ausentes <- dependencias[
    !vapply(dependencias, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(ausentes)) {
    stop(
      "A instalação do Sis-MEVAZ está incompleta. Pacotes ausentes: ",
      paste(ausentes, collapse = ", "),
      ". Execute novamente o instalador do Sis-MEVAZ.",
      call. = FALSE
    )
  }

  diretorio_de_dados <- normalizePath(
    diretorio_de_dados, winslash = "/", mustWork = TRUE
  )
  if (!dir.exists(file.path(diretorio_de_dados, "dados"))) {
    stop(
      "A pasta 'dados' não foi encontrada em: ", diretorio_de_dados,
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", package = "SisMEVAZ")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("A interface interativa não foi encontrada no pacote SisMEVAZ.",
         call. = FALSE)
  }

  base_anterior <- Sys.getenv("SISMEVAZ_BASE_DIR", unset = NA_character_)
  Sys.setenv(SISMEVAZ_BASE_DIR = diretorio_de_dados)
  on.exit({
    if (is.na(base_anterior)) {
      Sys.unsetenv("SISMEVAZ_BASE_DIR")
    } else {
      Sys.setenv(SISMEVAZ_BASE_DIR = base_anterior)
    }
  }, add = TRUE)

  shiny::runApp(
    appDir = app_dir,
    port = as.integer(porta),
    host = endereco,
    launch.browser = navegador
  )
}
