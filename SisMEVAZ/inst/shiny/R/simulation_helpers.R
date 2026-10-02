# =====================================================================
# simulation_helpers.R - Fase 3
#
# Porte fiel de simula_rede_teste.R:
# aplica um patch defensivo em SisMEVAZ::FUNC_SelecRegion
# (alinhamento robusto de MODELO_REGIONALIZACAO por ID,
# com normalizacao de acentos/encoding)
#
# A simulacao e executada diretamente no processo do Shiny,
# sem subprocesso Rscript.
#
# O pacote SisMEVAZ permanece intacto:
# o patch e feito via assignInNamespace() em tempo de execucao.
# =====================================================================


# ---------------------------------------------------------------------
# Controle para aplicar o patch somente uma vez por sessao R
# ---------------------------------------------------------------------

.mevaz_selec_region_patched <- FALSE


# ---------------------------------------------------------------------
# Normaliza os valores de MODELO_REGIONALIZACAO
# ---------------------------------------------------------------------

normalize_model_values <- function(values) {

  normalized <- trimws(as.character(values))

  normalized <- iconv(
    normalized,
    from = "",
    to = "UTF-8",
    sub = ""
  )

  normalized[
    normalized %in% c(
      "Medias Regionais",
      "MÃ©dias Regionais",
      "M?dias Regionais"
    )
  ] <- "Médias Regionais"

  normalized
}


# ---------------------------------------------------------------------
# Normaliza IDs usados para fazer o alinhamento
# ---------------------------------------------------------------------

normalize_id_keys <- function(keys) {

  out <- trimws(as.character(keys))

  out <- sub(
    "\\.0+$",
    "",
    out
  )

  out
}


# ---------------------------------------------------------------------
# Aplica o patch em FUNC_SelecRegion
# ---------------------------------------------------------------------

patch_func_selec_region <- function() {

  # Se o patch ja foi aplicado nesta sessao, nao faz novamente.
  if (.mevaz_selec_region_patched) {
    return(invisible())
  }


  # ---------------------------------------------------------------
  # Arquivo usado como fallback para MODELO_REGIONALIZACAO
  # ---------------------------------------------------------------

  consol_reservoirs_file <- file.path(
    REDE_RES_CONS,
    "input",
    "Dados_Reservatorios.xlsx"
  )

  fallback_model_map <- NULL


  if (file.exists(consol_reservoirs_file)) {

    consol_df <- openxlsx::read.xlsx(
      consol_reservoirs_file,
      colNames = TRUE
    )

    if (
      all(
        c(
          "ID",
          "MODELO_REGIONALIZACAO"
        ) %in% names(consol_df)
      )
    ) {

      fallback_ids <- normalize_id_keys(
        consol_df$ID
      )

      fallback_models <- normalize_model_values(
        consol_df$MODELO_REGIONALIZACAO
      )

      fallback_model_map <- fallback_models

      names(fallback_model_map) <- fallback_ids
    }
  }


  # ---------------------------------------------------------------
  # Guarda a FUNC_SelecRegion original do pacote
  # ---------------------------------------------------------------

  orig_select_region <- getFromNamespace(
    "FUNC_SelecRegion",
    "SisMEVAZ"
  )


  # ---------------------------------------------------------------
  # Funcao que substituira FUNC_SelecRegion
  # ---------------------------------------------------------------

  patched_select_region <- function(
      AfluInc_KNN,
      AfluInc_ML,
      Aflu_CalLocal,
      ModRegion,
      RegHidro,
      Prec_med,
      Areas,
      datMedReg
  ) {

    # -------------------------------------------------------------
    # Normaliza MODELO_REGIONALIZACAO
    # -------------------------------------------------------------

    ModRegion <- normalize_model_values(
      ModRegion
    )

    ids <- names(
      AfluInc_KNN
    )


    # -------------------------------------------------------------
    # Alinha MODELO_REGIONALIZACAO pelos IDs
    # -------------------------------------------------------------

    id_keys <- normalize_id_keys(
      ids
    )

    mod_map <- ModRegion

    mod_names <- names(
      ModRegion
    )


    if (
      is.null(mod_names) ||
      all(
        is.na(mod_names) |
        trimws(as.character(mod_names)) == ""
      )
    ) {

      if (
        length(ModRegion) == length(ids)
      ) {

        mod_names <- ids

      } else {

        mod_names <- as.character(
          seq_along(ModRegion)
        )
      }
    }


    names(mod_map) <- normalize_id_keys(
      mod_names
    )


    aligned_mod <- unname(
      mod_map[id_keys]
    )

    names(aligned_mod) <- ids


    # -------------------------------------------------------------
    # Usa Dados_Reservatorios.xlsx como fallback
    # -------------------------------------------------------------

    if (!is.null(fallback_model_map)) {

      need_fallback <-
        is.na(aligned_mod) |
        aligned_mod == ""

      if (any(need_fallback)) {

        fallback_values <- unname(
          fallback_model_map[
            id_keys[need_fallback]
          ]
        )

        aligned_mod[need_fallback] <-
          fallback_values
      }
    }


    # -------------------------------------------------------------
    # Verifica se existem modelos faltantes
    # -------------------------------------------------------------

    missing_ids <- ids[
      is.na(aligned_mod) |
      aligned_mod == ""
    ]


    if (length(missing_ids) > 0) {

      stop(
        sprintf(
          paste0(
            "MODELO_REGIONALIZACAO ausente para IDs: %s"
          ),
          paste(
            utils::head(
              missing_ids,
              20
            ),
            collapse = ", "
          )
        )
      )
    }


    # -------------------------------------------------------------
    # Modelos permitidos na interface interativa
    #
    # IMPORTANTE:
    # Calibracao Local NAO e permitida pela interface.
    # -------------------------------------------------------------

    allowed <- c(
      "KNN",
      "ML",
      "Multimodelo",
      "Médias Regionais"
    )


    invalid_values <- unique(
      aligned_mod[
        !(aligned_mod %in% allowed)
      ]
    )


    if (length(invalid_values) > 0) {

      stop(
        sprintf(
          "MODELO_REGIONALIZACAO invalido: %s",
          paste(
            invalid_values,
            collapse = ", "
          )
        )
      )
    }


    # -------------------------------------------------------------
    # Chama a FUNC_SelecRegion original
    #
    # Aflu_CalLocal e encaminhado para a nova assinatura.
    # -------------------------------------------------------------

    orig_select_region(
      AfluInc_KNN = AfluInc_KNN,
      AfluInc_ML = AfluInc_ML,
      Aflu_CalLocal = Aflu_CalLocal,
      ModRegion = aligned_mod,
      RegHidro = RegHidro,
      Prec_med = Prec_med,
      Areas = Areas,
      datMedReg = datMedReg
    )
  }


  # ---------------------------------------------------------------
  # Substitui FUNC_SelecRegion dentro do namespace do SisMEVAZ
  # ---------------------------------------------------------------

  assignInNamespace(
    "FUNC_SelecRegion",
    patched_select_region,
    ns = "SisMEVAZ"
  )


  # Marca o patch como aplicado
  .mevaz_selec_region_patched <<- TRUE

  invisible()
}


# ---------------------------------------------------------------------
# Executa a simulacao
#
# Chama:
#   1. SisMEVAZ_redeTeste
#   2. SisMEVAZ_print_Sintese
#   3. SisMEVAZ_print_Series
# ---------------------------------------------------------------------

run_simulation <- function(
    dados_dir = file.path(
      BASE_DIR,
      "dados"
    )
) {

  # Primeiro aplica o patch.
  patch_func_selec_region()


  # Executa a simulacao principal.
  SisMEVAZ::SisMEVAZ_redeTeste(
    dados_dir
  )


  # Gera a sintese.
  SisMEVAZ::SisMEVAZ_print_Sintese(
    dados_dir
  )


  # Gera as series.
  #
  # Mantemos este trecho tolerante a falha,
  # como no comportamento anterior.
  tryCatch(

    SisMEVAZ::SisMEVAZ_print_Series(
      dados_dir
    ),

    error = function(e) {

      message(
        "Aviso: SisMEVAZ_print_Series falhou: ",
        conditionMessage(e)
      )
    }
  )


  invisible(TRUE)
}
