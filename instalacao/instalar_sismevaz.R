## ================================================================
## INSTALADOR DO SIS-MEVAZ
##
## Instala o pacote SisMEVAZ, a interface interativa e as
## dependências necessárias.
##
## O instalador não seleciona nem verifica manualmente a versão
## do R. A compatibilidade é declarada no DESCRIPTION do pacote.
## ================================================================


## ----------------------------------------------------------------
## 1. LOCALIZAR O PROJETO
## ----------------------------------------------------------------

argumentos <- commandArgs(trailingOnly = FALSE)

arquivo_arg <- grep("^--file=", argumentos, value = TRUE)

if (length(arquivo_arg)) {

  instalador <- normalizePath(
    sub("^--file=", "", arquivo_arg[[1L]]),
    mustWork = TRUE
  )

  instalador_dir <- dirname(instalador)
  projeto_dir <- normalizePath(
    file.path(instalador_dir, ".."),
    mustWork = TRUE
  )

} else {

  projeto_dir <- normalizePath(
    getwd(),
    mustWork = TRUE
  )
}


## ----------------------------------------------------------------
## 2. LOCALIZAR O PACOTE SisMEVAZ
## ----------------------------------------------------------------

pkg_dir <- file.path(projeto_dir, "SisMEVAZ")

if (!dir.exists(pkg_dir)) {
  stop(
    "Pasta 'SisMEVAZ' não encontrada em: ",
    projeto_dir,
    "\nConfira se o instalador está dentro da pasta 'instalacao'.",
    call. = FALSE
  )
}


## ----------------------------------------------------------------
## 3. REPOSITÓRIO CRAN
## ----------------------------------------------------------------

repo <- "https://cloud.r-project.org"


## ----------------------------------------------------------------
## 4. INSTALAR remotes
## ----------------------------------------------------------------

cat("\n")
cat("=================================================\n")
cat("  Instalador do Sis-MEVAZ\n")
cat("=================================================\n\n")

cat("Preparando o instalador...\n")

if (!requireNamespace("remotes", quietly = TRUE)) {

  install.packages(
    "remotes",
    repos = repo
  )
}


## ----------------------------------------------------------------
## 5. INSTALAR DEPENDÊNCIAS EXTERNAS
## ----------------------------------------------------------------

cat("\n")
cat("Instalando dependências externas...\n\n")


## hydrobr ---------------------------------------------------------

if (!requireNamespace("hydrobr", quietly = TRUE)) {

  cat("Instalando hydrobr...\n")

  remotes::install_github(
    "lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241",
    dependencies = "hard",
    upgrade = "never"
  )
}


## phylin ----------------------------------------------------------

if (!requireNamespace("phylin", quietly = TRUE)) {

  cat("Instalando phylin...\n")

  install.packages(
    "phylin",
    repos = repo
  )
}


## ----------------------------------------------------------------
## 6. INSTALAR O PACOTE SisMEVAZ
## ----------------------------------------------------------------

cat("\n")
cat("=================================================\n")
cat("  Instalando o pacote SisMEVAZ e a interface\n")
cat("=================================================\n\n")

remotes::install_local(
  pkg_dir,
  dependencies = NA,
  upgrade = "never",
  force = TRUE
)


## ----------------------------------------------------------------
## 7. VERIFICAR WHITEBOXTOOLS
## ----------------------------------------------------------------

cat("\n")
cat("Verificando o executável WhiteboxTools...\n")

bin_ok <- tryCatch(
  isTRUE(whitebox::check_whitebox_binary()),
  error = function(e) FALSE
)

if (!bin_ok) {

  cat("WhiteboxTools não encontrado. Instalando...\n")

  whitebox::install_whitebox()
}

bin_ok <- tryCatch(
  isTRUE(whitebox::check_whitebox_binary()),
  error = function(e) FALSE
)

if (!bin_ok) {

  stop(
    "Não foi possível instalar ou localizar o WhiteboxTools.",
    call. = FALSE
  )
}


## ----------------------------------------------------------------
## 8. VERIFICAÇÃO FINAL
## ----------------------------------------------------------------

cat("\n")
cat("Verificando a instalação...\n")

necessarios <- c(
  "SisMEVAZ",
  "shiny",
  "bslib",
  "leaflet",
  "plotly",
  "DT",
  "shinyjs",
  "readxl",
  "stars",
  "whitebox",
  "hydrobr",
  "phylin"
)

ausentes <- necessarios[
  !vapply(
    necessarios,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(ausentes)) {

  stop(
    "Instalação incompleta. Pacotes ausentes: ",
    paste(ausentes, collapse = ", "),
    call. = FALSE
  )
}


## ----------------------------------------------------------------
## 9. VERIFICAR INTERFACE SHINY
## ----------------------------------------------------------------

if (!nzchar(system.file("shiny", package = "SisMEVAZ"))) {

  stop(
    "A interface interativa não foi incluída no pacote instalado.",
    call. = FALSE
  )
}


## ----------------------------------------------------------------
## 10. SUCESSO
## ----------------------------------------------------------------

cat("\n")
cat("=================================================\n")
cat("  INSTALAÇÃO CONCLUÍDA COM SUCESSO!\n")
cat("=================================================\n")
cat("\n")
cat("O Sis-MEVAZ e todos os componentes necessários\n")
cat("foram instalados neste ambiente R.\n")
cat("\n")
cat("Use o atalho ou o iniciador do Sis-MEVAZ para\n")
cat("abrir a interface interativa.\n")
cat("\n")
