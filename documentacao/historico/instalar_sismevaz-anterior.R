## Instala o Sis-MEVAZ, a interface interativa e todas as dependências.
## Requer exatamente o R 4.5.3 homologado para esta versão do sistema.

versao_necessaria <- numeric_version("4.5.3")
versao_atual <- getRversion()
if (versao_atual != versao_necessaria) {
  stop(
    "O Sis-MEVAZ requer exatamente o R 4.5.3.\n",
    "Versão encontrada: ", as.character(versao_atual), ".\n",
    "Instale ou selecione o R 4.5.3 antes de continuar.",
    call. = FALSE
  )
}

pkg_dir <- file.path(getwd(), "SisMEVAZ")
if (!dir.exists(pkg_dir)) {
  stop("Pasta 'SisMEVAZ' não encontrada a partir de: ", getwd(),
       "\nExecute este instalador a partir da raiz do projeto.")
}

repo <- "https://cloud.r-project.org"
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = repo)
}

cat("Instalando dependências externas (hydrobr e phylin)...\n")
if (!requireNamespace("hydrobr", quietly = TRUE)) {
  remotes::install_github("lhmet/hydrobr", dependencies = "hard", upgrade = "never")
}
if (!requireNamespace("phylin", quietly = TRUE)) {
  remotes::install_url(
    "https://cran.r-project.org/src/contrib/Archive/phylin/phylin_1.0.tar.gz",
    dependencies = "hard", upgrade = "never"
  )
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
cat("  Use iniciar_sismevaz para abrir o sistema.\n")
cat("=================================================\n")
