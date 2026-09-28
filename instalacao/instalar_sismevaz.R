## ============================================================
## INSTALADOR DO SIS-MEVAZ
##
## Instala:
##   1. dependências obrigatórias do núcleo;
##   2. pacote SisMEVAZ;
##   3. interface interativa;
##   4. WhiteboxTools;
##   5. hydrobr, quando possível.
##
## O hydrobr é opcional.
## Se sua instalação falhar, o Sis-MEVAZ continua disponível
## para as demais funções.
## ============================================================


repo <- "https://cloud.r-project.org"


## ------------------------------------------------------------
## 1. Localizar o diretório do projeto
## ------------------------------------------------------------

argumentos <- commandArgs(trailingOnly = FALSE)

arquivo_arg <- grep("^--file=", argumentos, value = TRUE)

if (length(arquivo_arg)) {

  instalador_dir <- dirname(
    normalizePath(
      sub("^--file=", "", arquivo_arg[[1L]]),
      mustWork = TRUE
    )
  )

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


pkg_dir <- file.path(projeto_dir, "SisMEVAZ")


if (!dir.exists(pkg_dir)) {

  stop(
    paste0(
      "Pasta 'SisMEVAZ' não encontrada em:\n",
      projeto_dir,
      "\n\n",
      "Verifique se o instalador está dentro da pasta 'instalacao'."
    ),
    call. = FALSE
  )
}


cat("\n")
cat("============================================================\n")
cat("                 INSTALAÇÃO DO SIS-MEVAZ\n")
cat("============================================================\n")
cat("\n")

cat("R utilizado:\n")
cat(R.home())
cat("\n\n")

cat("Versão do R: ")
cat(as.character(getRversion()))
cat("\n\n")


## ------------------------------------------------------------
## 2. Função auxiliar para instalar pacotes
## ------------------------------------------------------------

instalar_cran <- function(pkg, obrigatorio = TRUE) {

  if (requireNamespace(pkg, quietly = TRUE)) {

    cat("[OK] ", pkg, " já está instalado.\n", sep = "")

    return(TRUE)
  }

  cat("[..] Instalando ", pkg, "...\n", sep = "")

  resultado <- tryCatch({

    install.packages(
      pkg,
      repos = repo,
      dependencies = TRUE
    )

    requireNamespace(
      pkg,
      quietly = TRUE
    )

  }, error = function(e) {

    cat(
      "[ERRO] Falha ao instalar ",
      pkg,
      ": ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })

  if (!resultado && obrigatorio) {

    stop(
      paste0(
        "\nNão foi possível instalar a dependência obrigatória '",
        pkg,
        "'.\n",
        "A instalação do Sis-MEVAZ não pode continuar."
      ),
      call. = FALSE
    )
  }

  resultado
}


## ------------------------------------------------------------
## 3. remotes
## ------------------------------------------------------------

if (!requireNamespace("remotes", quietly = TRUE)) {

  cat("Instalando remotes...\n")

  instalar_cran(
    "remotes",
    obrigatorio = TRUE
  )
}


## ------------------------------------------------------------
## 4. Dependências obrigatórias externas
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("        DEPENDÊNCIAS OBRIGATÓRIAS DO SIS-MEVAZ\n")
cat("============================================================\n")
cat("\n")


## phylin é obrigatório porque está em Imports
instalar_cran(
  "phylin",
  obrigatorio = TRUE
)


## ------------------------------------------------------------
## 5. Instalar o núcleo do Sis-MEVAZ
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("             INSTALANDO O NÚCLEO DO SIS-MEVAZ\n")
cat("============================================================\n")
cat("\n")


cat("Instalando o pacote SisMEVAZ e suas dependências obrigatórias...\n")
cat("O módulo hydrobr não será exigido nesta etapa.\n\n")


resultado_sismevaz <- tryCatch({

  remotes::install_local(
    path = pkg_dir,
    dependencies = NA,
    upgrade = "never",
    force = TRUE
  )

  TRUE

}, error = function(e) {

  cat("\n")
  cat("[ERRO] Falha na instalação do SisMEVAZ.\n")
  cat(conditionMessage(e))
  cat("\n")

  FALSE
})


if (!resultado_sismevaz) {

  stop(
    paste0(
      "\nA instalação do núcleo do Sis-MEVAZ não foi concluída.\n\n",
      "O pacote hydrobr é opcional e não é responsável por esta etapa."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 6. Verificar WhiteboxTools
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("                VERIFICANDO WHITEBOXTOOLS\n")
cat("============================================================\n")
cat("\n")


if (!requireNamespace("whitebox", quietly = TRUE)) {

  cat("O pacote whitebox não foi encontrado.\n")
  cat("Instalando whitebox...\n")

  resultado_whitebox <- tryCatch({

    install.packages(
      "whitebox",
      repos = repo,
      dependencies = TRUE
    )

    TRUE

  }, error = function(e) {

    cat(
      "[ERRO] Não foi possível instalar whitebox: ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })

} else {

  resultado_whitebox <- TRUE
}


if (!resultado_whitebox) {

  stop(
    "Não foi possível instalar o pacote whitebox.",
    call. = FALSE
  )
}


bin_ok <- tryCatch(
  isTRUE(
    whitebox::check_whitebox_binary()
  ),
  error = function(e) FALSE
)


if (!bin_ok) {

  cat("WhiteboxTools não encontrado. Instalando o executável...\n")

  tryCatch(
    whitebox::install_whitebox(),
    error = function(e) {
      cat(
        "[AVISO] Falha na instalação do WhiteboxTools: ",
        conditionMessage(e),
        "\n",
        sep = ""
      )
    }
  )
}


bin_ok <- tryCatch(
  isTRUE(
    whitebox::check_whitebox_binary()
  ),
  error = function(e) FALSE
)


if (!bin_ok) {

  stop(
    "Não foi possível instalar ou localizar o WhiteboxTools.",
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 7. Tentar instalar hydrobr
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("          INSTALANDO MÓDULO DE PLUVIOMETRIA\n")
cat("============================================================\n")
cat("\n")

cat(
  "O pacote 'hydrobr' é opcional.\n",
  "Ele é utilizado para atualização/obtenção dos dados ",
  "pluviométricos.\n\n",
  sep = ""
)


hydrobr_ok <- FALSE


if (requireNamespace("hydrobr", quietly = TRUE)) {

  hydrobr_ok <- TRUE

  cat("[OK] hydrobr já está instalado.\n")

} else {

  cat("Tentando instalar hydrobr...\n\n")

  hydrobr_ok <- tryCatch({

    remotes::install_github(
      "lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241",
      dependencies = "hard",
      upgrade = "never"
    )

    requireNamespace(
      "hydrobr",
      quietly = TRUE
    )

  }, error = function(e) {

    cat("\n")
    cat("[AVISO] Não foi possível instalar hydrobr.\n")
    cat(
      "Motivo: ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })
}


## ------------------------------------------------------------
## 8. Verificação final
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("                  VERIFICAÇÃO FINAL\n")
cat("============================================================\n")
cat("\n")


necessarios <- c(
  "SisMEVAZ",
  "phylin",
  "raster",
  "sf",
  "lwgeom",
  "dplyr",
  "lubridate",
  "openxlsx",
  "RPostgreSQL",
  "xts",
  "whitebox"
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
    paste0(
      "Instalação incompleta do núcleo.\n",
      "Pacotes ausentes: ",
      paste(ausentes, collapse = ", ")
    ),
    call. = FALSE
  )
}


if (!nzchar(system.file(
  "shiny",
  package = "SisMEVAZ"
))) {

  stop(
    "A interface interativa não foi incluída no pacote instalado.",
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 9. Resultado
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("             INSTALAÇÃO DO SIS-MEVAZ CONCLUÍDA\n")
cat("============================================================\n")
cat("\n")

cat("[OK] Núcleo do Sis-MEVAZ: instalado\n")
cat("[OK] Interface interativa: instalada\n")
cat("[OK] WhiteboxTools: instalado\n")


if (hydrobr_ok) {

  cat("[OK] hydrobr: instalado\n")

  cat(
    "\nA atualização da pluviometria está disponível.\n"
  )

} else {

  cat("[AVISO] hydrobr: NÃO instalado\n")

  cat(
    "\n",
    "A instalação principal foi concluída, mas o módulo ",
    "de atualização da pluviometria não está disponível.\n\n",
    "A função de atualização da pluviometria, incluindo a cadeia ",
    "associada a SisMEVAZ_atualiz_Pluviometria_redeConsol, ",
    "não poderá ser utilizada enquanto o hydrobr não estiver instalado.\n\n",
    "As demais funcionalidades do Sis-MEVAZ, incluindo a ",
    "simulação e os procedimentos relacionados à inclusão de ",
    "novos reservatórios, permanecem disponíveis.\n",
    sep = ""
  )
}


cat("\n")
cat("Instalação finalizada.\n")## ================================================================
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
