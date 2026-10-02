## ============================================================
## INSTALADOR DO SIS-MEVAZ
## ============================================================
##
## Instala:
##   - dependências do núcleo
##   - dependências da interface
##   - terra/raster
##   - geobr
##   - hydrobr (opcional)
##   - phylin
##   - pacote SisMEVAZ
##   - WhiteboxTools
##
## Características:
##   - não exige uma versão específica do R
##   - detecta automaticamente a versão do R em execução
##   - utiliza biblioteca pessoal do usuário
##   - não exige sudo no Linux
##   - não altera permanentemente a configuração do R
##   - no Windows prioriza pacotes binários
##   - hydrobr é opcional
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
## CONFIGURAÇÃO DO SISTEMA
## ------------------------------------------------------------

repo <- "https://cloud.r-project.org"

is_windows <- identical(
  .Platform$OS.type,
  "windows"
)

if (is_windows) {

  pkg_type <- "binary"

} else {

  pkg_type <- getOption("pkgType")

}


cat(
  "R utilizado: ",
  R.version.string,
  "\n",
  sep = ""
)

cat(
  "R_HOME:      ",
  R.home(),
  "\n",
  sep = ""
)

cat(
  "Sistema:     ",
  R.version$platform,
  "\n",
  sep = ""
)

if (is_windows) {

  cat("Modo de instalação no Windows: BINÁRIO\n")

} else {

  cat(
    "Modo de instalação: ",
    pkg_type,
    "\n",
    sep = ""
  )

}

cat("\n")


## ------------------------------------------------------------
## CONFIGURAR BIBLIOTECA PESSOAL DO USUÁRIO
## ------------------------------------------------------------
##
## O instalador NÃO deve depender de:
##
##   /usr/local/lib/R/site-library
##
## nem exigir privilégios administrativos.
##
## Primeiro utiliza R_LIBS_USER, que é o mecanismo padrão
## do R para bibliotecas pessoais.
##
## Caso R_LIBS_USER esteja vazio, é criado um caminho
## equivalente ao padrão do R para a versão/plataforma
## atualmente em execução.
## ------------------------------------------------------------

configurar_biblioteca_usuario <- function() {

  lib_usuario <- Sys.getenv(
    "R_LIBS_USER",
    unset = ""
  )

  ## ----------------------------------------------------------
  ## Caso normal: R já forneceu R_LIBS_USER.
  ## ----------------------------------------------------------

  if (nzchar(lib_usuario)) {

    ## Em algumas configurações R_LIBS_USER pode conter
    ## múltiplos caminhos. Utilizamos o primeiro.
    separador <- .Platform$path.sep

    lib_usuario <- strsplit(
      lib_usuario,
      split = separador,
      fixed = TRUE
    )[[1L]][1L]

    lib_usuario <- path.expand(lib_usuario)

  } else {

    ## --------------------------------------------------------
    ## Fallback.
    ##
    ## R >= 4.2 normalmente já define R_LIBS_USER no startup.
    ## Este bloco existe para instalações incomuns nas quais
    ## essa variável esteja vazia.
    ## --------------------------------------------------------

    versao_r <- paste(
      R.version$major,
      R.version$minor,
      sep = "."
    )

    if (is_windows) {

      localappdata <- Sys.getenv(
        "LOCALAPPDATA",
        unset = ""
      )

      if (!nzchar(localappdata)) {
        localappdata <- path.expand("~")
      }

      ## R para Windows ARM utiliza uma biblioteca específica.
      if (grepl(
        "aarch64",
        R.version$platform,
        ignore.case = TRUE
      )) {

        pasta_plataforma <- "aarch64-library"

      } else {

        pasta_plataforma <- "win-library"

      }

      lib_usuario <- file.path(
        localappdata,
        "R",
        pasta_plataforma,
        versao_r
      )

    } else {

      lib_usuario <- file.path(
        path.expand("~"),
        "R",
        paste0(
          R.version$platform,
          "-library"
        ),
        versao_r
      )
    }
  }


  ## ----------------------------------------------------------
  ## Criar biblioteca se necessário.
  ## ----------------------------------------------------------

  if (!dir.exists(lib_usuario)) {

    ok_criacao <- tryCatch(

      dir.create(
        lib_usuario,
        recursive = TRUE,
        showWarnings = FALSE
      ),

      error = function(e) FALSE

    )

    if (!isTRUE(ok_criacao) &&
        !dir.exists(lib_usuario)) {

      stop(
        paste0(
          "Não foi possível criar a biblioteca pessoal do usuário:\n",
          lib_usuario,
          "\n\n",
          "O instalador não pode continuar sem uma biblioteca ",
          "gravável."
        ),
        call. = FALSE
      )
    }
  }


  ## ----------------------------------------------------------
  ## Verificar se realmente podemos escrever nela.
  ## ----------------------------------------------------------

  teste <- file.path(
    lib_usuario,
    ".sismevaz_write_test"
  )

  escrita_ok <- tryCatch({

    ok <- file.create(teste)

    if (file.exists(teste)) {
      unlink(teste)
    }

    isTRUE(ok)

  }, error = function(e) {

    FALSE

  })


  if (!isTRUE(escrita_ok)) {

    stop(
      paste0(
        "A biblioteca pessoal do usuário não possui ",
        "permissão de escrita:\n",
        lib_usuario,
        "\n\n",
        "O instalador não pode continuar."
      ),
      call. = FALSE
    )
  }


  ## ----------------------------------------------------------
  ## Colocar a biblioteca pessoal no início de .libPaths().
  ##
  ## Isto vale somente para esta execução do instalador.
  ## Não altera permanentemente a configuração do usuário.
  ## ----------------------------------------------------------

  .libPaths(
    unique(
      c(
        lib_usuario,
        .libPaths()
      )
    )
  )


  ## ----------------------------------------------------------
  ## Verificação final.
  ## ----------------------------------------------------------

  if (!lib_usuario %in% .libPaths()) {

    stop(
      paste0(
        "Não foi possível adicionar a biblioteca pessoal ",
        "à sessão atual do R:\n",
        lib_usuario
      ),
      call. = FALSE
    )
  }


  lib_usuario
}


lib_instalacao <- configurar_biblioteca_usuario()


cat(
  "Biblioteca de instalação: ",
  lib_instalacao,
  "\n",
  sep = ""
)

cat(
  "Bibliotecas R ativas:\n"
)

cat(
  paste(
    .libPaths(),
    collapse = "\n"
  ),
  "\n\n",
  sep = ""
)


## ------------------------------------------------------------
## FUNÇÃO AUXILIAR PARA INSTALAÇÃO DO CRAN
## ------------------------------------------------------------

instalar_cran <- function(
    pkgs,
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
  cat(
    paste(
      ausentes,
      collapse = ", "
    ),
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
      ausentes,
      lib = lib_instalacao,
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


  ## ----------------------------------------------------------
  ## Verificar individualmente quais pacotes ainda faltam.
  ## ----------------------------------------------------------

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
      paste(
        ainda_ausentes,
        collapse = ", "
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
    "[OK] Dependências instaladas: ",
    paste(
      ausentes,
      collapse = ", "
    ),
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
## Esta etapa é feita antes das demais dependências.
##
## Windows:
##   usa binário correspondente ao R em execução.
##
## Linux/macOS:
##   utiliza o tipo de pacote definido pelo R.
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("2. Instalando terra\n")
cat("============================================================\n")


if (!requireNamespace(
  "terra",
  quietly = TRUE
)) {

  if (is_windows) {

    cat(
      "Instalando terra como pacote binário para o R atual...\n"
    )


    terra_ok <- tryCatch({

      utils::install.packages(
        "terra",
        lib = lib_instalacao,
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
        "\n[ERRO] Não foi possível instalar terra ",
        "como binário.\n",
        conditionMessage(e),
        "\n",
        sep = ""
      )

      FALSE
    })


  } else {

    cat(
      "Instalando terra para o sistema operacional...\n"
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

  cat(
    "[OK] terra já está instalado.\n"
  )
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
  as.character(
    packageVersion("terra")
  ),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 3. DEPENDÊNCIAS DO NÚCLEO
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


cat("\n")
cat("============================================================\n")
cat("5. Instalando geobr\n")
cat("============================================================\n")

## ------------------------------------------------------------
## O geobr atual pode exigir uma versão de R mais nova.
##
## Para manter o instalador compatível com versões antigas
## do R, usamos automaticamente uma versão anterior do geobr
## quando necessário.
## ------------------------------------------------------------

geobr_ok <- FALSE

if (requireNamespace("geobr", quietly = TRUE)) {

  geobr_ok <- TRUE

  cat(
    "[OK] geobr já está instalado: ",
    as.character(packageVersion("geobr")),
    "\n",
    sep = ""
  )

} else {

  versao_r <- getRversion()

  if (versao_r >= "4.4.0") {

    cat(
      "R >= 4.4.0 detectado.\n"
    )

    cat(
      "Instalando a versão atual do geobr...\n"
    )

    geobr_ok <- tryCatch({

      utils::install.packages(
        "geobr",
        lib = lib_instalacao,
        repos = repo,
        dependencies = TRUE,
        type = pkg_type
      )

      requireNamespace(
        "geobr",
        quietly = TRUE
      )

    }, error = function(e) {

      cat(
        "\n[ERRO] Falha na instalação do geobr:\n",
        conditionMessage(e),
        "\n",
        sep = ""
      )

      FALSE
    })

  } else {

    cat(
      "R ",
      as.character(versao_r),
      " detectado.\n",
      sep = ""
    )

    cat(
      "A versão atual do geobr requer R >= 4.4.0.\n"
    )

    cat(
      "Instalando automaticamente geobr 1.9.1,\n"
    )

    cat(
      "compatível com versões anteriores do R...\n"
    )

    geobr_url <- paste0(
      "https://cran.r-project.org/src/contrib/Archive/geobr/",
      "geobr_1.9.1.tar.gz"
    )

    geobr_ok <- tryCatch({

      remotes::install_url(
        geobr_url,
        lib = lib_instalacao,
        dependencies = TRUE,
        upgrade = "never"
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
}

if (!isTRUE(geobr_ok)) {

  stop(
    paste0(
      "Não foi possível instalar uma versão compatível ",
      "do pacote geobr para o R ",
      as.character(getRversion()),
      "."
    ),
    call. = FALSE
  )
}

cat(
  "[OK] geobr instalado: ",
  as.character(packageVersion("geobr")),
  "\n",
  sep = ""
)


## ------------------------------------------------------------
## 6. HYDROBR
## ------------------------------------------------------------
##
## hydrobr é OPCIONAL.
##
## Se a instalação falhar, o instalador continua.
##
## O pacote SisMEVAZ deve tratar as funções que dependem
## do hydrobr separadamente.
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("6. Instalando hydrobr (opcional)\n")
cat("============================================================\n")


hydrobr_ok <- FALSE


if (requireNamespace(
  "hydrobr",
  quietly = TRUE
)) {

  hydrobr_ok <- TRUE

  cat(
    "[OK] hydrobr já está instalado.\n"
  )

} else {

  tryCatch({

    cat(
      "Instalando hydrobr a partir do GitHub...\n"
    )


    remotes::install_github(
      "lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241",
      lib = lib_instalacao,
      dependencies = "hard",
      upgrade = "never"
    )


    hydrobr_ok <- requireNamespace(
      "hydrobr",
      quietly = TRUE
    )


  }, error = function(e) {

    cat("\n")
    cat(
      "[AVISO] Não foi possível instalar o hydrobr.\n"
    )

    cat(
      "[AVISO] A instalação do Sis-MEVAZ continuará ",
      "sem o hydrobr.\n",
      sep = ""
    )

    cat(
      "[AVISO] Motivo: ",
      conditionMessage(e),
      "\n",
      sep = ""
    )
  })
}


if (hydrobr_ok) {

  cat(
    "[OK] hydrobr disponível.\n"
  )

} else {

  cat(
    "[AVISO] hydrobr não está disponível nesta instalação.\n"
  )
}


## ------------------------------------------------------------
## 7. PHYLIN
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
## 8. INSTALAR O PACOTE SISMEVAZ
## ------------------------------------------------------------
##
## As dependências são instaladas explicitamente acima.
##
## Portanto:
##
##   dependencies = FALSE
##
## evita que remotes tente resolver novamente toda a árvore
## de dependências.
##
## A biblioteca destino é explicitamente informada.
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("8. Instalando o pacote SisMEVAZ\n")
cat("============================================================\n")


tryCatch({

  remotes::install_local(
    path = pkg_dir,
    lib = lib_instalacao,
    dependencies = FALSE,
    upgrade = "never",
    force = TRUE
  )

}, error = function(e) {

  stop(
    paste0(
      "Falha na instalação do pacote SisMEVAZ.\n\n",
      conditionMessage(e)
    ),
    call. = FALSE
  )
})


cat(
  "[OK] Pacote SisMEVAZ instalado.\n"
)


## ------------------------------------------------------------
## 9. VERIFICAR WHITEBOXTOOLS
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("9. Verificando WhiteboxTools\n")
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


cat(
  "[OK] WhiteboxTools disponível.\n"
)


## ------------------------------------------------------------
## 10. VERIFICAÇÃO FINAL
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
      paste(
        ausentes,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## VERIFICAR INTERFACE SHINY
## ------------------------------------------------------------

if (!nzchar(
  system.file(
    "shiny",
    package = "SisMEVAZ"
  )
)) {

  stop(
    "A interface interativa não foi incluída no pacote instalado.",
    call. = FALSE
  )
}


## ------------------------------------------------------------
## RELATÓRIO FINAL
## ------------------------------------------------------------

cat("\n")

if (!hydrobr_ok) {

  cat(
    "[AVISO] O Sis-MEVAZ foi instalado sem o pacote hydrobr.\n"
  )

  cat(
    "[AVISO] Funcionalidades que dependem do hydrobr ",
    "não estarão disponíveis.\n",
    sep = ""
  )
}


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
  "hydrobr:       ",
  if (hydrobr_ok) {
    "OK"
  } else {
    "não instalado"
  },
  "\n",
  sep = ""
)


cat(
  "phylin:        OK\n"
)

cat(
  "SisMEVAZ:      OK\n"
)

cat(
  "WhiteboxTools: OK\n"
)

cat(
  "Interface:     OK\n\n"
)


cat(
  "O Sis-MEVAZ está pronto para utilização.\n\n"
)
