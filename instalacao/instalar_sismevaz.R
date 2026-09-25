## Instala o Sis-MEVAZ, a interface interativa e todas as dependências.
## Requer R 4.2.0 ou posterior. O instalador não depende de devtools.

versao_minima <- numeric_version("4.2.0")
versao_atual <- getRversion()
if (versao_atual < versao_minima) {
  stop(
    "O Sis-MEVAZ requer R 4.2.0 ou posterior.\n",
    "Versão encontrada: ", as.character(versao_atual), ".\n",
    "Atualize o R antes de continuar.",
    call. = FALSE
  )
}

argumentos <- commandArgs(trailingOnly = FALSE)
arquivo_arg <- grep("^--file=", argumentos, value = TRUE)
if (length(arquivo_arg)) {
  instalador_dir <- dirname(normalizePath(
    sub("^--file=", "", arquivo_arg[[1L]]), mustWork = TRUE
  ))
  projeto_dir <- normalizePath(file.path(instalador_dir, ".."), mustWork = TRUE)
} else {
  projeto_dir <- normalizePath(getwd(), mustWork = TRUE)
}

pkg_dir <- file.path(projeto_dir, "SisMEVAZ")
if (!dir.exists(pkg_dir)) {
  stop("Pasta 'SisMEVAZ' não encontrada em: ", projeto_dir,
       "\nConfira se o instalador permanece dentro da pasta 'instalacao'.")
}

repo <- "https://cloud.r-project.org"
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = repo)
}

cat("Instalando dependências externas (hydrobr e phylin)...\n")
if (!requireNamespace("hydrobr", quietly = TRUE)) {
  remotes::install_github("lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241", dependencies = "hard", upgrade = "never")
}
if (!requireNamespace("phylin", quietly = TRUE)) {
  install.packages("phylin", repos = repo)
}

cat("Instalando o pacote SisMEVAZ e a interface interativa...\n")
remotes::install_local(
  pkg_dir, dependencies = TRUE, upgrade = "never", force = TRUE
)

cat("Verificando o executável WhiteboxTools...\n")
bin_ok <- tryCatch(
  isTRUE(whitebox::check_whitebox_binary()),
  error = function(e) FALSE
)
if (!bin_ok) {
  whitebox::install_whitebox()
}
if (!isTRUE(tryCatch(whitebox::check_whitebox_binary(), error = function(e) FALSE))) {
  stop("Não foi possível instalar ou localizar o WhiteboxTools.", call. = FALSE)
}

necessarios <- c(
  "SisMEVAZ", "shiny", "bslib", "leaflet", "plotly", "DT", "shinyjs",
  "readxl", "stars", "whitebox", "hydrobr", "phylin"
)
ausentes <- necessarios[
  !vapply(necessarios, requireNamespace, logical(1), quietly = TRUE)
]
if (length(ausentes)) {
  stop("Instalação incompleta. Pacotes ausentes: ",
       paste(ausentes, collapse = ", "), call. = FALSE)
}
if (!nzchar(system.file("shiny", package = "SisMEVAZ"))) {
  stop("A interface interativa não foi incluída no pacote instalado.", call. = FALSE)
}

cat("\n=================================================\n")
cat("  Instalação concluída com sucesso!\n")
cat("  Use o atalho Sis-MEVAZ ou o iniciador para abrir o sistema.\n")
cat("=================================================\n")
