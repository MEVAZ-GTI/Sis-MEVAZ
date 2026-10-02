## ============================================================
## INSTALADOR DO SIS-MEVAZ
## ============================================================
##
## Instala:
##   - remotes
##   - terra
##   - dependências do núcleo
##   - dependências da interface
##   - geobr
##   - hydrobr (opcional)
##   - phylin
##   - SisMEVAZ
##   - WhiteboxTools
##
## PRINCÍPIOS:
##
##   1. Não escolhe uma versão específica do R.
##   2. Usa o Rscript que chamou este arquivo.
##   3. Usa uma biblioteca própria do usuário.
##   4. No Windows, prioriza binários.
##   5. Não usa sudo nem exige biblioteca do sistema.
##   6. Não altera permanentemente .libPaths().
##   7. hydrobr é opcional.
##   8. A instalação do SisMEVAZ é validada no final.
##
## ============================================================


## ------------------------------------------------------------
## 0. CABEÇALHO
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("                 INSTALAÇÃO DO SIS-MEVAZ\n")
cat("============================================================\n\n")


## ------------------------------------------------------------
## 1. LOCALIZAR O PROJETO
## ------------------------------------------------------------

argumentos <- commandArgs(trailingOnly = FALSE)

arquivo_arg <- grep(
  "^--file=",
  argumentos,
  value = TRUE
)

if (length(arquivo_arg)) {

  instalador_arquivo <- normalizePath(
    sub("^--file=", "", arquivo_arg[[1L]]),
    mustWork = TRUE
  )

  instalador_dir <- dirname(
    instalador_arquivo
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


pkg_dir <- file.path(
  projeto_dir,
  "SisMEVAZ"
)


if (!dir.exists(pkg_dir)) {

  stop(
    paste0(
      "Pasta 'SisMEVAZ' não encontrada.\n\n",
      "Diretório procurado:\n",
      projeto_dir,
      "\n\n",
      "Verifique se o instalador está dentro da pasta ",
      "'instalacao'."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 2. INFORMAÇÕES DO R
## ------------------------------------------------------------

is_windows <- identical(
  .Platform$OS.type,
  "windows"
)

r_major <- R.version$major

r_minor <- sub(
  "\\..*$",
  "",
  R.version$minor
)

r_minor_version <- paste(
  r_major,
  r_minor,
  sep = "."
)


cat(
  "R utilizado: ",
  R.version.string,
  "\n",
  sep = ""
)

cat(
  "Plataforma: ",
  R.version$platform,
  "\n",
  sep = ""
)

cat(
  "R minor: ",
  r_minor_version,
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 3. REPOSITÓRIO
## ------------------------------------------------------------

repo <- "https://cloud.r-project.org"


## ------------------------------------------------------------
## 4. TIPO DE PACOTE
## ------------------------------------------------------------
##
## No Windows queremos explicitamente os binários.
##
## Em Linux/macOS utilizamos a configuração do próprio R.
##
## Não existe aqui nenhuma escolha de versão do R.
## ------------------------------------------------------------

if (is_windows) {

  pkg_type <- "binary"

} else {

  pkg_type <- getOption(
    "pkgType",
    default = "source"
  )
}


if (is_windows) {

  cat("Tipo de pacote: BINÁRIO\n")

} else {

  cat(
    "Tipo de pacote: ",
    pkg_type,
    "\n",
    sep = ""
  )
}


## ------------------------------------------------------------
## 5. BIBLIOTECA DO USUÁRIO
## ------------------------------------------------------------
##
## Não instalamos em:
##
##   /usr/local/lib/R/site-library
##
## nem em:
##
##   C:/Program Files/R/...
##
## ------------------------------------------------------------

if (is_windows) {

  lib_instalacao <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "R",
    "win-library",
    r_minor_version
  )

} else if (identical(.Platform$OS.type, "unix") &&
           Sys.info()[["sysname"]] == "Darwin") {

  lib_instalacao <- file.path(
    path.expand("~/Library/R"),
    paste0(
      R.version$platform,
      "-library"
    ),
    r_minor_version
  )

} else {

  lib_instalacao <- file.path(
    path.expand("~/R"),
    paste0(
      R.version$platform,
      "-library"
    ),
    r_minor_version
  )
}


if (!dir.exists(lib_instalacao)) {

  dir.create(
    lib_instalacao,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


if (!dir.exists(lib_instalacao)) {

  stop(
    paste0(
      "Não foi possível criar a biblioteca do usuário:\n",
      lib_instalacao
    ),
    call. = FALSE
  )
}


## Testar escrita

arquivo_teste <- file.path(
  lib_instalacao,
  paste0(
    ".sismevaz_write_test_",
    Sys.getpid()
  )
)


teste_escrita <- tryCatch({

  file.create(
    arquivo_teste,
    showWarnings = FALSE
  )

}, error = function(e) {

  FALSE
})


if (file.exists(arquivo_teste)) {
  unlink(
    arquivo_teste,
    force = TRUE
  )
}


if (!isTRUE(teste_escrita)) {

  stop(
    paste0(
      "A biblioteca do usuário não permite gravação:\n",
      lib_instalacao,
      "\n\n",
      "O instalador não utilizará sudo nem modificará ",
      "bibliotecas do sistema."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 6. USAR A BIBLIOTECA NESTA EXECUÇÃO
## ------------------------------------------------------------

.libPaths(
  c(
    lib_instalacao,
    .libPaths()
  )
)


cat("\n")
cat("Biblioteca utilizada pelo instalador:\n")
cat(
  "  ",
  lib_instalacao,
  "\n",
  sep = ""
)

cat("\n")
cat("Bibliotecas R atualmente utilizadas:\n")

for (lib in .libPaths()) {

  cat(
    "  ",
    lib,
    "\n",
    sep = ""
  )
}


## ------------------------------------------------------------
## 7. FUNÇÕES AUXILIARES
## ------------------------------------------------------------


pacote_instalado <- function(pkg) {

  requireNamespace(
    pkg,
    quietly = TRUE
  )
}


versao_pacote <- function(pkg) {

  if (!pacote_instalado(pkg)) {
    return(NULL)
  }

  as.character(
    packageVersion(pkg)
  )
}


versao_satisfaz <- function(pkg, requisito = NULL) {

  if (!pacote_instalado(pkg)) {
    return(FALSE)
  }

  if (is.null(requisito) ||
      !nzchar(requisito)) {

    return(TRUE)
  }

  atual <- packageVersion(pkg)

  tryCatch({

    compare <- utils::compareVersion(
      as.character(atual),
      requisito
    )

    compare >= 0

  }, error = function(e) {

    FALSE
  })
}


## ------------------------------------------------------------
## 8. INSTALAÇÃO CRAN
## ------------------------------------------------------------
##
## A função NÃO contém mapeamento de versões do R.
##
## Exemplo:
##
##   R 4.2 -> alguma versão
##   R 4.3 -> outra versão
##
## NÃO existe.
##
## O próprio repositório é consultado para o R em execução.
##
## ------------------------------------------------------------

instalar_cran <- function(
    pkgs,
    obrigatorios = TRUE,
    requisitos = NULL) {

  pkgs <- unique(pkgs)

  if (!length(pkgs)) {

    return(
      invisible(TRUE)
    )
  }


  ## ----------------------------------------------------------
  ## Determinar quais precisam ser instalados
  ## ----------------------------------------------------------

  precisam <- character()

  for (pkg in pkgs) {

    requisito <- NULL

    if (!is.null(requisitos) &&
        pkg %in% names(requisitos)) {

      requisito <- requisitos[[pkg]]
    }

    if (!pacote_instalado(pkg)) {

      precisam <- c(
        precisam,
        pkg
      )

      next
    }


    if (!versao_satisfaz(pkg, requisito)) {

      precisam <- c(
        precisam,
        pkg
      )

      cat(
        "[INFO] ",
        pkg,
        " instalado em versão ",
        versao_pacote(pkg),
        " não satisfaz ",
        requisito,
        ".\n",
        sep = ""
      )

    } else {

      cat(
        "[OK] ",
        pkg,
        ": ",
        versao_pacote(pkg),
        "\n",
        sep = ""
      )
    }
  }


  precisam <- unique(precisam)


  if (!length(precisam)) {

    return(
      invisible(TRUE)
    )
  }


  ## ----------------------------------------------------------
  ## Instalar
  ## ----------------------------------------------------------

  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Instalando pacotes:\n")
  cat(
    "  ",
    paste(precisam, collapse = ", "),
    "\n",
    sep = ""
  )
  cat("------------------------------------------------------------\n")


  resultado <- tryCatch({

    utils::install.packages(
      precisam,
      lib = lib_instalacao,
      repos = repo,
      dependencies = TRUE,
      type = pkg_type
    )

    TRUE

  }, error = function(e) {

    cat("\n")
    cat(
      "[ERRO] Falha durante install.packages():\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


  ## ----------------------------------------------------------
  ## Verificação
  ## ----------------------------------------------------------

  problemas <- character()

  for (pkg in precisam) {

    requisito <- NULL

    if (!is.null(requisitos) &&
        pkg %in% names(requisitos)) {

      requisito <- requisitos[[pkg]]
    }


    if (!pacote_instalado(pkg)) {

      problemas <- c(
        problemas,
        paste0(
          pkg,
          " não foi instalado"
        )
      )

      next
    }


    if (!versao_satisfaz(pkg, requisito)) {

      problemas <- c(
        problemas,
        paste0(
          pkg,
          " ",
          versao_pacote(pkg),
          " < ",
          requisito
        )
      )
    }
  }


  if (length(problemas)) {

    mensagem <- paste0(
      "Não foi possível satisfazer as dependências:\n",
      paste(
        paste0("  - ", problemas),
        collapse = "\n"
      )
    )

    if (obrigatorios) {

      stop(
        mensagem,
        call. = FALSE
      )

    } else {

      warning(
        mensagem,
        call. = FALSE
      )

      return(
        invisible(FALSE)
      )
    }
  }


  cat("\n")

  for (pkg in precisam) {

    cat(
      "[OK] ",
      pkg,
      ": ",
      versao_pacote(pkg),
      "\n",
      sep = ""
    )
  }


  invisible(
    isTRUE(resultado)
  )
}


## ------------------------------------------------------------
## 9. REMOTES
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
## 10. TERRA
## ------------------------------------------------------------
##
## terra é usado diretamente pela instalação e por dependências
## espaciais.
##
## No Windows:
##     binário correspondente ao R atual.
##
## Em Unix:
##     tipo definido pelo próprio R.
##
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("2. Instalando terra\n")
cat("============================================================\n")


if (pacote_instalado("terra")) {

  cat(
    "[OK] terra: ",
    versao_pacote("terra"),
    "\n",
    sep = ""
  )

} else {

  terra_ok <- tryCatch({

    utils::install.packages(
      "terra",
      lib = lib_instalacao,
      repos = repo,
      dependencies = TRUE,
      type = pkg_type
    )

    TRUE

  }, error = function(e) {

    cat(
      "\n[ERRO] Falha na instalação do terra:\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


  if (!terra_ok ||
      !pacote_instalado("terra")) {

    stop(
      paste0(
        "O pacote 'terra' não pôde ser instalado.\n\n",
        "R utilizado: ",
        R.version.string,
        "\n",
        "Plataforma: ",
        R.version$platform
      ),
      call. = FALSE
    )
  }


  cat(
    "[OK] terra: ",
    versao_pacote("terra"),
    "\n",
    sep = ""
  )
}


## ------------------------------------------------------------
## 11. DEPENDÊNCIAS DO NÚCLEO
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


## IMPORTANTE:
##
## Não colocamos aqui versões específicas do R.
##
## Se uma versão mínima for realmente necessária ao código,
## ela deve ser declarada no DESCRIPTION.
##
## O instalador então tenta satisfazer essa exigência.
##

instalar_cran(
  dependencias_nucleo,
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 12. DEPENDÊNCIAS DA INTERFACE
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
## 13. GEOBR
## ------------------------------------------------------------
##
## NÃO fazemos:
##
##   R 4.2 -> geobr 1.x
##   R 4.3 -> geobr 1.x
##   R 4.4 -> geobr 2.x
##
## O R/CRAN deve fornecer a versão disponível para o R atual.
##
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
  "[OK] geobr: ",
  versao_pacote("geobr"),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 14. HYDROBR
## ------------------------------------------------------------
##
## hydrobr é OPCIONAL.
##
## Se não puder ser instalado, o restante da instalação continua.
##
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("6. Instalando hydrobr (opcional)\n")
cat("============================================================\n")


hydrobr_ok <- FALSE


if (pacote_instalado("hydrobr")) {

  hydrobr_ok <- TRUE

  cat(
    "[OK] hydrobr já disponível: ",
    versao_pacote("hydrobr"),
    "\n",
    sep = ""
  )

} else {

  hydrobr_ok <- tryCatch({

    cat(
      "Tentando instalar hydrobr a partir do GitHub...\n"
    )

    remotes::install_github(
      "lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241",
      lib = lib_instalacao,
      dependencies = "hard",
      upgrade = "never",
      force = FALSE
    )

    requireNamespace(
      "hydrobr",
      quietly = TRUE
    )

  }, error = function(e) {

    cat("\n")
    cat(
      "[AVISO] Não foi possível instalar o hydrobr.\n"
    )

    cat(
      "[AVISO] O instalador continuará sem o hydrobr.\n"
    )

    cat(
      "[AVISO] Motivo: ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


  if (isTRUE(hydrobr_ok)) {

    cat(
      "[OK] hydrobr instalado: ",
      versao_pacote("hydrobr"),
      "\n",
      sep = ""
    )

  } else {

    cat(
      "[AVISO] hydrobr não está disponível.\n"
    )
  }
}


## ------------------------------------------------------------
## 15. PHYLIN
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("7. Instalando phylin\n")
cat("============================================================\n")


instalar_cran(
  "phylin",
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 16. INSTALAR SISMEVAZ
## ------------------------------------------------------------
##
## As dependências obrigatórias já foram tratadas acima.
##
## Usamos dependencies = FALSE para impedir que o remotes
## volte a tentar resolver toda a árvore de dependências e,
## especialmente no Windows, tente trocar um binário já funcional
## por um source mais novo.
##
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("8. Instalando o pacote SisMEVAZ\n")
cat("============================================================\n")


if (!requireNamespace("remotes", quietly = TRUE)) {

  stop(
    "O pacote 'remotes' não está disponível.",
    call. = FALSE
  )
}


sismevaz_instalado <- tryCatch({

  remotes::install_local(
    path = pkg_dir,
    lib = lib_instalacao,
    dependencies = FALSE,
    upgrade = "never",
    force = TRUE,
    build = TRUE,
    build_opts = c(
      "--no-manual",
      "--no-resave-data",
      "--no-build-vignettes"
    )
  )

  TRUE

}, error = function(e) {

  cat("\n")
  cat(
    "[ERRO] Falha ao instalar SisMEVAZ:\n",
    conditionMessage(e),
    "\n",
    sep = ""
  )

  FALSE
})


if (!sismevaz_instalado ||
    !requireNamespace("SisMEVAZ", quietly = TRUE)) {

  stop(
    paste0(
      "O pacote SisMEVAZ não foi instalado corretamente.\n\n",
      "Verifique também o DESCRIPTION e o NAMESPACE."
    ),
    call. = FALSE
  )
}


cat(
  "[OK] SisMEVAZ: ",
  versao_pacote("SisMEVAZ"),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 17. WHITEBOXTOOLS
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("9. Verificando WhiteboxTools\n")
cat("============================================================\n")


whitebox_ok <- tryCatch(

  isTRUE(
    whitebox::check_whitebox_binary()
  ),

  error = function(e) {

    FALSE
  }
)


if (!whitebox_ok) {

  cat(
    "WhiteboxTools não encontrado.\n"
  )

  cat(
    "Tentando instalar...\n"
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

  error = function(e) {

    FALSE
  }
)


if (!whitebox_ok) {

  stop(
    "Não foi possível instalar ou localizar o WhiteboxTools.",
    call. = FALSE
  )
}


cat(
  "[OK] WhiteboxTools disponível.\n"
)


## ------------------------------------------------------------
## 18. VERIFICAÇÃO FINAL
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("10. Verificação final\n")
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
  "phylin",
  "geobr"
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
      paste(
        paste0(
          "  - ",
          ausentes
        ),
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 19. VERIFICAR VERSÃO INSTALADA DO SISMEVAZ
## ------------------------------------------------------------

sismevaz_version <- tryCatch(

  as.character(
    packageVersion("SisMEVAZ")
  ),

  error = function(e) {

    NA_character_
  }
)


if (is.na(sismevaz_version)) {

  stop(
    "Não foi possível determinar a versão instalada do SisMEVAZ.",
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 20. VERIFICAR INTERFACE SHINY
## ------------------------------------------------------------

interface_dir <- system.file(
  "shiny",
  package = "SisMEVAZ"
)


if (!nzchar(interface_dir)) {

  stop(
    paste0(
      "O pacote SisMEVAZ foi instalado, mas a interface Shiny ",
      "não foi encontrada dentro do pacote instalado.\n\n",
      "Verifique a pasta 'inst/shiny' do pacote."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 21. RESULTADO
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("              INSTALAÇÃO CONCLUÍDA COM SUCESSO\n")
cat("============================================================\n\n")


cat(
  "R:             ",
  R.version.string,
  "\n",
  sep = ""
)

cat(
  "Biblioteca:    ",
  lib_instalacao,
  "\n",
  sep = ""
)

cat(
  "terra:         ",
  versao_pacote("terra"),
  "\n",
  sep = ""
)

cat(
  "raster:        ",
  versao_pacote("raster"),
  "\n",
  sep = ""
)

cat(
  "sf:            ",
  versao_pacote("sf"),
  "\n",
  sep = ""
)

cat(
  "lwgeom:        ",
  versao_pacote("lwgeom"),
  "\n",
  sep = ""
)

cat(
  "geobr:         ",
  versao_pacote("geobr"),
  "\n",
  sep = ""
)

cat(
  "phylin:        ",
  versao_pacote("phylin"),
  "\n",
  sep = ""
)

cat(
  "SisMEVAZ:      ",
  sismevaz_version,
  "\n",
  sep = ""
)

if (hydrobr_ok) {

  cat(
    "hydrobr:       ",
    versao_pacote("hydrobr"),
    "\n",
    sep = ""
  )

} else {

  cat(
    "hydrobr:       NÃO INSTALADO (opcional)\n"
  )
}

cat(
  "WhiteboxTools: OK\n"
)

cat(
  "Interface:     OK\n"
)

cat("\n")

if (!hydrobr_ok) {

  cat(
    "[AVISO] O Sis-MEVAZ foi instalado sem o hydrobr.\n"
  )

  cat(
    "[AVISO] Funções que dependem diretamente do hydrobr ",
    "não estarão disponíveis.\n"
  )

  cat("\n")
}


cat(
  "O Sis-MEVAZ está pronto para utilização.\n"
)

cat("\n")
