## ============================================================
## INSTALADOR DO SIS-MEVAZ
## ============================================================
##
## Instala:
##   - dependências do núcleo
##   - dependências da interface
##   - terra
##   - raster
##   - geobr
##   - hydrobr (opcional)
##   - phylin
##   - pacote SisMEVAZ
##   - WhiteboxTools
##
## PRINCÍPIOS:
##
##   - não exige uma versão específica do R;
##   - usa o Rscript que chamou este arquivo;
##   - não exige privilégios administrativos;
##   - instala em biblioteca pessoal do usuário;
##   - não depende da biblioteca de um projeto renv;
##   - verifica versões mínimas das dependências;
##   - verifica efetivamente se SisMEVAZ foi instalado;
##   - usa binários no Windows quando disponíveis;
##   - adapta geobr ao R em execução;
##   - hydrobr é opcional.
##
## ============================================================


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

  arquivo_instalador <- sub(
    "^--file=",
    "",
    arquivo_arg[[1L]]
  )

  instalador_dir <- dirname(
    normalizePath(
      arquivo_instalador,
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


pkg_dir <- file.path(
  projeto_dir,
  "SisMEVAZ"
)


if (!dir.exists(pkg_dir)) {

  stop(
    paste0(
      "Pasta 'SisMEVAZ' não encontrada em:\n",
      projeto_dir,
      "\n\n",
      "Confira se o instalador está dentro da pasta ",
      "'instalacao'."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 2. CONFIGURAÇÃO BÁSICA
## ------------------------------------------------------------

repo <- "https://cloud.r-project.org"

is_windows <- identical(
  .Platform$OS.type,
  "windows"
)

is_macos <- identical(
  Sys.info()[["sysname"]],
  "Darwin"
)


if (is_windows) {

  pkg_type <- "binary"

} else {

  pkg_type <- getOption(
    "pkgType",
    default = "source"
  )
}


cat(
  "R utilizado: ",
  R.version.string,
  "\n",
  sep = ""
)

cat(
  "R.home(): ",
  R.home(),
  "\n",
  sep = ""
)

cat(
  "Sistema: ",
  R.version$platform,
  "\n",
  sep = ""
)

cat(
  "Versão R curta: ",
  paste(
    R.version$major,
    R.version$minor,
    sep = "."
  ),
  "\n",
  sep = ""
)

if (is_windows) {

  cat(
    "Modo de instalação no Windows: BINÁRIO\n"
  )

} else {

  cat(
    "Modo de instalação: ",
    pkg_type,
    "\n",
    sep = ""
  )
}


## ------------------------------------------------------------
## 3. BIBLIOTECA DO USUÁRIO
## ------------------------------------------------------------
##
## Não usamos cegamente R_LIBS_USER porque o ambiente pode
## estar dentro de um projeto renv.
##
## A biblioteca abaixo é específica para o R em execução.
##
## Windows:
##   %LOCALAPPDATA%/R/win-library/x.y
##
## Unix:
##   ~/R/<platform>-library/x.y
##
## macOS:
##   ~/Library/R/<machine>/x.y/library
##
## ------------------------------------------------------------

versao_r_curta <- paste(
  R.version$major,
  sub(
    "^([0-9]+).*",
    "\\1",
    R.version$minor
  ),
  sep = "."
)


if (is_windows) {

  local_appdata <- Sys.getenv(
    "LOCALAPPDATA"
  )

  if (!nzchar(local_appdata)) {

    local_appdata <- file.path(
      path.expand("~"),
      "AppData",
      "Local"
    )
  }

  lib_instalacao <- file.path(
    local_appdata,
    "R",
    "win-library",
    versao_r_curta
  )

} else if (is_macos) {

  maquina <- Sys.info()[["machine"]]

  lib_instalacao <- file.path(
    path.expand("~"),
    "Library",
    "R",
    maquina,
    versao_r_curta,
    "library"
  )

} else {

  lib_instalacao <- file.path(
    path.expand("~"),
    "R",
    paste0(
      R.version$platform,
      "-library"
    ),
    versao_r_curta
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
      "Não foi possível criar a biblioteca pessoal do usuário:\n",
      lib_instalacao
    ),
    call. = FALSE
  )
}


## Testar escrita na biblioteca.

arquivo_teste <- file.path(
  lib_instalacao,
  paste0(
    ".teste_escrita_",
    Sys.getpid()
  )
)


teste_escrita <- tryCatch({

  writeLines(
    "teste",
    arquivo_teste
  )

  file.exists(
    arquivo_teste
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
      "A biblioteca pessoal não é gravável:\n",
      lib_instalacao,
      "\n\n",
      "O instalador não tentará modificar bibliotecas ",
      "do sistema."
    ),
    call. = FALSE
  )
}


## Colocar nossa biblioteca na frente do .libPaths()
## apenas nesta execução do instalador.

.libPaths(
  unique(
    c(
      lib_instalacao,
      .libPaths()
    )
  )
)


cat("\n")
cat("Biblioteca de instalação:\n")
cat("  ", lib_instalacao, "\n", sep = "")

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
## 4. FUNÇÕES AUXILIARES
## ------------------------------------------------------------


versao_instalada <- function(pkg) {

  if (!requireNamespace(
    pkg,
    quietly = TRUE
  )) {

    return(NULL)
  }

  tryCatch(
    packageVersion(pkg),
    error = function(e) NULL
  )
}


pacote_compativel <- function(
    pkg,
    versao_minima = NULL) {

  versao <- versao_instalada(pkg)

  if (is.null(versao)) {
    return(FALSE)
  }

  if (is.null(versao_minima)) {
    return(TRUE)
  }

  versao >= package_version(
    versao_minima
  )
}


instalar_cran <- function(
    pkgs,
    versoes_minimas = NULL,
    obrigatorios = TRUE,
    tipo = pkg_type) {

  pkgs <- unique(pkgs)

  precisa_instalar <- vapply(
    pkgs,
    function(pkg) {

      versao_minima <- NULL

      if (!is.null(versoes_minimas) &&
          pkg %in% names(versoes_minimas)) {

        versao_minima <- versoes_minimas[[pkg]]
      }

      !pacote_compativel(
        pkg,
        versao_minima
      )
    },
    logical(1)
  )

  instalar <- pkgs[
    precisa_instalar
  ]

  if (!length(instalar)) {

    cat(
      "[OK] Pacotes já instalados e compatíveis: ",
      paste(pkgs, collapse = ", "),
      "\n",
      sep = ""
    )

    return(invisible(TRUE))
  }


  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Instalando/atualizando pacotes:\n")
  cat(
    paste(instalar, collapse = ", "),
    "\n"
  )

  cat("Biblioteca destino:\n")
  cat(
    lib_instalacao,
    "\n"
  )

  cat("------------------------------------------------------------\n")


  resultado <- tryCatch({

    utils::install.packages(
      instalar,
      lib = lib_instalacao,
      repos = repo,
      dependencies = TRUE,
      type = tipo
    )

    TRUE

  }, error = function(e) {

    cat(
      "\n[ERRO] Falha na instalação/atualização:\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


  problemas <- character(0)


  for (pkg in instalar) {

    if (!requireNamespace(
      pkg,
      quietly = TRUE
    )) {

      problemas <- c(
        problemas,
        paste0(
          pkg,
          " não está disponível"
        )
      )

      next
    }


    if (!is.null(versoes_minimas) &&
        pkg %in% names(versoes_minimas)) {

      atual <- packageVersion(pkg)

      minima <- package_version(
        versoes_minimas[[pkg]]
      )

      if (atual < minima) {

        problemas <- c(
          problemas,
          paste0(
            pkg,
            " ",
            as.character(atual),
            " < ",
            as.character(minima)
          )
        )
      }
    }
  }


  if (length(problemas)) {

    mensagem <- paste0(
      "Não foi possível instalar/atualizar corretamente:\n",
      paste(
        problemas,
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


  cat(
    "[OK] Pacotes instalados/atualizados: ",
    paste(instalar, collapse = ", "),
    "\n",
    sep = ""
  )

  invisible(TRUE)
}


## ------------------------------------------------------------
## 5. REMOTES
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
## 6. TERRA
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("2. Instalando terra\n")
cat("============================================================\n")


terra_ok <- pacote_compativel(
  "terra"
)


if (!terra_ok) {

  cat(
    "Instalando terra para o R em execução...\n"
  )


  terra_ok <- tryCatch({

    utils::install.packages(
      "terra",
      lib = lib_instalacao,
      repos = repo,
      dependencies = TRUE,
      type = pkg_type
    )

    requireNamespace(
      "terra",
      quietly = TRUE
    )

  }, error = function(e) {

    cat(
      "\n[ERRO] Falha na instalação do terra:\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })
}


if (!isTRUE(terra_ok)) {

  stop(
    paste0(
      "O pacote 'terra' não pôde ser instalado ",
      "para o R em execução.\n\n",
      "R: ",
      R.version.string,
      "\n",
      "Sistema: ",
      R.version$platform
    ),
    call. = FALSE
  )
}


cat(
  "[OK] terra: ",
  as.character(
    packageVersion("terra")
  ),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 7. DEPENDÊNCIAS DO NÚCLEO
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


versoes_minimas_nucleo <- c(
  dplyr       = "1.1.4",
  lubridate   = "1.9.2",
  lwgeom      = "0.2-13",
  openxlsx    = "4.2.5.2",
  raster      = "3.6-26",
  RPostgreSQL = "0.7-5",
  sf          = "1.0-14",
  xts         = "0.13.1"
)


instalar_cran(
  dependencias_nucleo,
  versoes_minimas = versoes_minimas_nucleo,
  obrigatorios = TRUE
)


## ------------------------------------------------------------
## 8. DEPENDÊNCIAS DA INTERFACE
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
## 9. GEOBR
## ------------------------------------------------------------
##
## A versão atual do geobr passou a exigir R mais recente.
##
## Para R >= 4.4:
##   usa CRAN normalmente.
##
## Para R < 4.4:
##   usa a versão arquivada 1.9.1.
##
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("5. Instalando geobr\n")
cat("============================================================\n")


versao_r_num <- getRversion()


if (versao_r_num >= "4.4.0") {

  cat(
    "R >= 4.4.0: instalando geobr do CRAN.\n"
  )


  geobr_ok <- tryCatch({

    instalar_cran(
      "geobr",
      obrigatorios = TRUE
    )

    requireNamespace(
      "geobr",
      quietly = TRUE
    )

  }, error = function(e) {

    cat(
      "[ERRO] ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


} else {

  cat(
    "R < 4.4.0: usando geobr 1.9.1 do arquivo do CRAN.\n"
  )


  geobr_ok <- tryCatch({

    remotes::install_version(
      "geobr",
      version = "1.9.1",
      repos = repo,
      lib = lib_instalacao,
      dependencies = TRUE,
      upgrade = "never",
      type = pkg_type
    )

    requireNamespace(
      "geobr",
      quietly = TRUE
    )

  }, error = function(e) {

    cat(
      "\n[ERRO] Falha na instalação do geobr 1.9.1:\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })
}


if (!isTRUE(geobr_ok)) {

  stop(
    paste0(
      "O pacote 'geobr' não pôde ser instalado.\n\n",
      "R utilizado: ",
      R.version.string
    ),
    call. = FALSE
  )
}


cat(
  "[OK] geobr: ",
  as.character(
    packageVersion("geobr")
  ),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 10. HYDROBR
## ------------------------------------------------------------
##
## hydrobr é OPCIONAL.
##
## Se falhar, a instalação continua.
##
## IMPORTANTE:
## Para que isso funcione de verdade, hydrobr também deve estar
## em Suggests, e NÃO em Imports, no DESCRIPTION do SisMEVAZ.
##
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("6. Instalando hydrobr (opcional)\n")
cat("============================================================\n")


hydrobr_ok <- requireNamespace(
  "hydrobr",
  quietly = TRUE
)


if (!hydrobr_ok) {

  cat(
    "Tentando instalar hydrobr a partir do GitHub...\n"
  )


  hydrobr_ok <- tryCatch({

    remotes::install_github(
      "lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241",
      lib = lib_instalacao,
      dependencies = "hard",
      upgrade = "never"
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
      "[AVISO] A instalação do SisMEVAZ continuará.\n"
    )
    cat(
      "[AVISO] Motivo: ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })
}


if (hydrobr_ok) {

  cat(
    "[OK] hydrobr: ",
    as.character(
      packageVersion("hydrobr")
    ),
    "\n",
    sep = ""
  )

} else {

  cat(
    "[AVISO] hydrobr não está disponível.\n"
  )
}


## ------------------------------------------------------------
## 11. PHYLIN
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
## 12. INSTALAR SISMEVAZ
## ------------------------------------------------------------
##
## As dependências já foram tratadas explicitamente.
##
## Portanto:
##
##   dependencies = FALSE
##
## Isso evita uma nova resolução automática de toda a árvore
## de dependências.
##
## Depois da instalação, verificamos obrigatoriamente se o
## namespace do SisMEVAZ pode ser carregado.
##
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("8. Instalando o pacote SisMEVAZ\n")
cat("============================================================\n")


sismevaz_ok <- tryCatch({

  remotes::install_local(
    path = pkg_dir,
    lib = lib_instalacao,
    dependencies = FALSE,
    upgrade = "never",
    force = TRUE
  )


  ## Não confiar somente no retorno de install_local().
  ## O teste abaixo é obrigatório.

  requireNamespace(
    "SisMEVAZ",
    quietly = TRUE
  )

}, error = function(e) {

  cat(
    "\n[ERRO] Falha na instalação do SisMEVAZ:\n",
    conditionMessage(e),
    "\n",
    sep = ""
  )

  FALSE
})


if (!isTRUE(sismevaz_ok)) {

  stop(
    paste0(
      "O pacote SisMEVAZ não foi instalado corretamente.\n\n",
      "Verifique as mensagens apresentadas acima.\n\n",
      "Biblioteca utilizada:\n",
      lib_instalacao
    ),
    call. = FALSE
  )
}


cat(
  "[OK] SisMEVAZ instalado e carregável.\n"
)


## ------------------------------------------------------------
## 13. VERIFICAR WHITEBOXTOOLS
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
    "WhiteboxTools não encontrado. Instalando...\n"
  )


  whitebox_install_ok <- tryCatch({

    whitebox::install_whitebox()

    TRUE

  }, error = function(e) {

    cat(
      "\n[ERRO] Falha na instalação do WhiteboxTools:\n",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    FALSE
  })


  if (!isTRUE(whitebox_install_ok)) {

    stop(
      "Não foi possível instalar o WhiteboxTools.",
      call. = FALSE
    )
  }
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
    "Não foi possível localizar o WhiteboxTools após a instalação.",
    call. = FALSE
  )
}


cat(
  "[OK] WhiteboxTools disponível.\n"
)


## ------------------------------------------------------------
## 14. VERIFICAÇÃO FINAL
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
        ausentes,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 15. VERIFICAR A INTERFACE
## ------------------------------------------------------------

interface_dir <- system.file(
  "shiny",
  package = "SisMEVAZ"
)


if (!nzchar(interface_dir) ||
    !dir.exists(interface_dir)) {

  stop(
    paste0(
      "A interface Shiny não foi encontrada no pacote ",
      "SisMEVAZ instalado."
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 16. VERIFICAR VERSÕES CRÍTICAS
## ------------------------------------------------------------

versoes_finais <- c(
  dplyr       = "1.1.4",
  lubridate   = "1.9.2",
  lwgeom      = "0.2-13",
  openxlsx    = "4.2.5.2",
  raster      = "3.6-26",
  RPostgreSQL = "0.7-5",
  sf          = "1.0-14",
  xts         = "0.13.1"
)


problemas_finais <- character(0)


for (pkg in names(versoes_finais)) {

  atual <- versao_instalada(pkg)

  minima <- package_version(
    versoes_finais[[pkg]]
  )


  if (is.null(atual)) {

    problemas_finais <- c(
      problemas_finais,
      paste0(
        pkg,
        " não instalado"
      )
    )

  } else if (atual < minima) {

    problemas_finais <- c(
      problemas_finais,
      paste0(
        pkg,
        " ",
        as.character(atual),
        " < ",
        as.character(minima)
      )
    )
  }
}


if (length(problemas_finais)) {

  stop(
    paste0(
      "Foram encontradas dependências incompatíveis:\n",
      paste(
        problemas_finais,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 17. RESULTADO
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
  as.character(
    packageVersion("terra")
  ),
  "\n",
  sep = ""
)

cat(
  "raster:        ",
  as.character(
    packageVersion("raster")
  ),
  "\n",
  sep = ""
)

cat(
  "geobr:         ",
  as.character(
    packageVersion("geobr")
  ),
  "\n",
  sep = ""
)

cat(
  "phylin:        ",
  as.character(
    packageVersion("phylin")
  ),
  "\n",
  sep = ""
)

cat(
  "SisMEVAZ:      ",
  as.character(
    packageVersion("SisMEVAZ")
  ),
  "\n",
  sep = ""
)

cat(
  "WhiteboxTools: OK\n"
)

cat(
  "Interface:     OK\n"
)


if (hydrobr_ok) {

  cat(
    "hydrobr:       OK\n"
  )

} else {

  cat(
    "hydrobr:       OPCIONAL / NÃO INSTALADO\n"
  )
}


cat("\n")


if (!hydrobr_ok) {

  cat(
    "[AVISO] O SisMEVAZ foi instalado sem o hydrobr.\n"
  )

  cat(
    "[AVISO] Download_estacoesPlu() requer o hydrobr.\n"
  )

  cat(
    "[AVISO] As demais funcionalidades instaladas permanecem disponíveis.\n"
  )

  cat("\n")
}


cat(
  "O Sis-MEVAZ está pronto para utilização.\n\n"
)

cat(
  "Biblioteca utilizada pelo instalador:\n",
  lib_instalacao,
  "\n\n",
  sep = ""
)

cat(
  "Você pode utilizar o pacote normalmente com:\n\n"
)

cat(
  "  library(SisMEVAZ)\n\n"
)

cat(
  "Para abrir a interface, utilize o launcher correspondente\n",
  "ao seu sistema operacional.\n\n",
  sep = ""
)
