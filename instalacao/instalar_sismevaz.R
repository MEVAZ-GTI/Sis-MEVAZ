## ============================================================
## INSTALADOR DO SIS-MEVAZ
## ============================================================
##
## Instala:
##   - dependências do núcleo
##   - dependências da interface
##   - terra/raster
##   - hydrobr
##   - phylin
##   - pacote SisMEVAZ
##   - WhiteboxTools
##
## Não seleciona nem exige uma versão específica do R.
## No Windows, prioriza pacotes binários para evitar compilação
## desnecessária de pacotes espaciais como terra.
## ============================================================


cat("\n")
cat("============================================================\n")
cat("                 INSTALAÇÃO DO SIS-MEVAZ\n")
cat("============================================================\n\n")


## ------------------------------------------------------------
## LOCALIZAR O PROJETO
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
      "Confira se o instalador está dentro da pasta 'instalacao'."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## CONFIGURAÇÃO DO REPOSITÓRIO
## ------------------------------------------------------------

repo <- "https://cloud.r-project.org"

is_windows <- identical(.Platform$OS.type, "windows")

if (is_windows) {
  pkg_type <- "binary"
} else {
  pkg_type <- getOption("pkgType")
}


cat("R utilizado: ", R.version.string, "\n", sep = "")
cat("Sistema: ", R.version$platform, "\n", sep = "")

if (is_windows) {
  cat("Modo de instalação no Windows: BINÁRIO\n")
} else {
  cat("Modo de instalação: ", pkg_type, "\n", sep = "")
}

cat("\n")


## ------------------------------------------------------------
## FUNÇÃO AUXILIAR
## ------------------------------------------------------------

instalar_cran <- function(pkgs,
                           obrigatorios = TRUE,
                           tipo = pkg_type) {

  pkgs <- unique(pkgs)

  ausentes <- pkgs[
    !vapply(
      pkgs,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

  if (!length(ausentes)) {

    cat(
      "[OK] Pacotes já instalados: ",
      paste(pkgs, collapse = ", "),
      "\n",
      sep = ""
    )

    return(invisible(TRUE))
  }

  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Instalando pacotes:\n")
  cat(paste(ausentes, collapse = ", "), "\n")
  cat("------------------------------------------------------------\n")

  resultado <- tryCatch({

    utils::install.packages(
      ausentes,
      repos = repo,
      dependencies = TRUE,
      type = tipo
    )

    TRUE

  }, error = function(e) {

    cat(
      "\n[ERRO] Falha na instalação dos pacotes:\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


  ainda_ausentes <- ausentes[
    !vapply(
      ausentes,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]


  if (length(ainda_ausentes)) {

    mensagem <- paste0(
      "Não foi possível instalar os seguintes pacotes:\n",
      paste(ainda_ausentes, collapse = ", ")
    )

    if (obrigatorios) {
      stop(mensagem, call. = FALSE)
    } else {
      warning(mensagem, call. = FALSE)
      return(invisible(FALSE))
    }
  }

  cat(
    "[OK] Dependências instaladas: ",
    paste(ausentes, collapse = ", "),
    "\n",
    sep = ""
  )

  invisible(TRUE)
}


## ------------------------------------------------------------
## 1. REMOTES
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("1. Preparando ferramenta de instalação\n")
cat("============================================================\n")

instalar_cran(
  "remotes",
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 2. TERRA
## ------------------------------------------------------------
##
## Esta etapa é deliberadamente feita ANTES do hydrobr.
##
## No Windows usamos explicitamente o binário correspondente
## ao R em execução.
##
## Isso evita que install.packages/remotes escolha uma versão
## fonte mais nova e tente compilá-la com Rtools.
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("2. Instalando terra\n")
cat("============================================================\n")


if (!requireNamespace("terra", quietly = TRUE)) {

  if (is_windows) {

    cat(
      "Instalando terra como pacote binário para o R atual...\n"
    )

    terra_ok <- tryCatch({

      utils::install.packages(
        "terra",
        repos = repo,
        dependencies = TRUE,
        type = "binary"
      )

      requireNamespace(
        "terra",
        quietly = TRUE
      )

    }, error = function(e) {

      cat(
        "\n[ERRO] Não foi possível instalar terra como binário.\n",
        conditionMessage(e),
        "\n",
        sep = ""
      )

      FALSE
    })

  } else {

    cat(
      "Instalando terra normalmente no sistema operacional...\n"
    )

    terra_ok <- tryCatch({

      utils::install.packages(
        "terra",
        repos = repo,
        dependencies = TRUE
      )

      requireNamespace(
        "terra",
        quietly = TRUE
      )

    }, error = function(e) {

      cat(
        "\n[ERRO] Não foi possível instalar terra.\n",
        conditionMessage(e),
        "\n",
        sep = ""
      )

      FALSE
    })
  }

} else {

  terra_ok <- TRUE

  cat("[OK] terra já está instalado.\n")
}


if (!isTRUE(terra_ok)) {

  stop(
    paste0(
      "\n",
      "============================================================\n",
      "INSTALAÇÃO INTERROMPIDA\n",
      "============================================================\n\n",
      "O pacote 'terra' não pôde ser instalado.\n\n",
      "O Sis-MEVAZ utiliza o pacote 'raster' no cálculo de\n",
      "características fisiográficas das novas bacias.\n\n",
      "Por isso, não é seguro continuar sem uma instalação\n",
      "funcional do terra.\n"
    ),
    call. = FALSE
  )
}


cat(
  "[OK] terra instalado: ",
  as.character(packageVersion("terra")),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 3. DEPENDÊNCIAS ESPACIAIS E DO NÚCLEO
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("3. Instalando dependências do núcleo\n")
cat("============================================================\n")


dependencias_nucleo <- c(
  "dplyr",
  "lubridate",
  "lwgeom",
  "openxlsx",
  "raster",
  "RPostgreSQL",
  "sf",
  "xts"
)


instalar_cran(
  dependencias_nucleo,
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 4. DEPENDÊNCIAS DA INTERFACE
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("4. Instalando dependências da interface\n")
cat("============================================================\n")


dependencias_interface <- c(
  "bslib",
  "DT",
  "leaflet",
  "plotly",
  "readxl",
  "shiny",
  "shinyjs",
  "stars",
  "whitebox"
)


instalar_cran(
  dependencias_interface,
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 5. GEOBR
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("5. Instalando geobr\n")
cat("============================================================\n")

instalar_cran(
  "geobr",
  obrigatorios = TRUE
)

cat(
  "[OK] geobr instalado: ",
  as.character(packageVersion("geobr")),
  "\n",
  sep = ""
)


cat("\n============================================================\n")
cat("6. HYDROBR\n")
cat("============================================================\n")

hydrobr_ok <- FALSE

tryCatch({

  cat("Instalando hydrobr a partir do GitHub...\n")

  remotes::install_github(
    "lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241",
    dependencies = "hard",
    upgrade = "never"
  )

  hydrobr_ok <- requireNamespace("hydrobr", quietly = TRUE)

}, error = function(e) {

  cat("\n")
  cat("[AVISO] Não foi possível instalar o hydrobr.\n")
  cat("[AVISO] A instalação do Sis-MEVAZ continuará sem o hydrobr.\n")
  cat("[AVISO] Motivo: ", conditionMessage(e), "\n", sep = "")

})

if (hydrobr_ok) {
  cat("[OK] hydrobr instalado com sucesso.\n")
} else {
  cat("[AVISO] hydrobr não está disponível nesta instalação.\n")
}




## ------------------------------------------------------------
## 7. PHYLIN
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("6. Instalando phylin\n")
cat("============================================================\n")


instalar_cran(
  "phylin",
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 8. INSTALAR O PACOTE SISMEVAZ
## ------------------------------------------------------------
##
## IMPORTANTE:
##
## As dependências já foram instaladas explicitamente acima.
##
## Por isso usamos dependencies = FALSE aqui.
##
## Isso impede que remotes volte a resolver toda a árvore de
## dependências e tente substituir um pacote binário funcional
## por uma versão fonte mais recente.
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("7. Instalando o pacote SisMEVAZ\n")
cat("============================================================\n")


remotes::install_local(
  path = pkg_dir,
  dependencies = FALSE,
  upgrade = "never",
  force = TRUE
)


## ------------------------------------------------------------
## . VERIFICAR WHITEBOXTOOLS
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("8. Verificando WhiteboxTools\n")
cat("============================================================\n")


whitebox_ok <- tryCatch(
  isTRUE(
    whitebox::check_whitebox_binary()
  ),
  error = function(e) FALSE
)


if (!whitebox_ok) {

  cat(
    "WhiteboxTools não encontrado. Instalando...\n"
  )

  tryCatch({

    whitebox::install_whitebox()

  }, error = function(e) {

    stop(
      paste0(
        "Não foi possível instalar o WhiteboxTools.\n\n",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
}


whitebox_ok <- tryCatch(
  isTRUE(
    whitebox::check_whitebox_binary()
  ),
  error = function(e) FALSE
)


if (!whitebox_ok) {

  stop(
    "Não foi possível instalar ou localizar o WhiteboxTools.",
    call. = FALSE
  )
}


cat("[OK] WhiteboxTools disponível.\n")


## ------------------------------------------------------------
## 10. VERIFICAÇÃO FINAL
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("9. Verificação final\n")
cat("============================================================\n")


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
  "raster",
  "terra",
  "sf",
  "lwgeom",
  "dplyr",
  "lubridate",
  "openxlsx",
  "RPostgreSQL",
  "xts",
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
    paste0(
      "Instalação incompleta.\n\n",
      "Pacotes ausentes:\n",
      paste(ausentes, collapse = ", ")
    ),
    call. = FALSE
  )
}


if (!nzchar(system.file("shiny", package = "SisMEVAZ"))) {

  stop(
    "A interface interativa não foi incluída no pacote instalado.",
    call. = FALSE
  )
}

if (!hydrobr_ok) {
  cat("\n")
  cat("[AVISO] O Sis-MEVAZ foi instalado sem o pacote hydrobr.\n")
  cat("[AVISO] Funcionalidades que dependem do hydrobr não estarão disponíveis.\n")
}

## ------------------------------------------------------------
## SUCESSO
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("              INSTALAÇÃO CONCLUÍDA COM SUCESSO\n")
cat("============================================================\n\n")

cat("R:             ", R.version.string, "\n", sep = "")
cat(
  "terra:         ",
  as.character(packageVersion("terra")),
  "\n",
  sep = ""
)
cat(
  "raster:        ",
  as.character(packageVersion("raster")),
  "\n",
  sep = ""
)
cat("hydrobr:       OK\n")
cat("phylin:        OK\n")
cat("SisMEVAZ:      OK\n")
cat("WhiteboxTools: OK\n")
cat("Interface:     OK\n\n")

cat("O Sis-MEVAZ está pronto para utilização.\n\n")
