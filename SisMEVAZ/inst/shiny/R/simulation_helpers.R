# =====================================================================
# simulation_helpers.R  -  Fase 3
# Porte fiel de simula_rede_teste.R: aplica o mesmo patch defensivo em
# SisMEVAZ::FUNC_SelecRegion (alinhamento robusto de MODELO_REGIONALIZACAO
# por ID, com normalizacao de acentos/encoding) e chama SisMEVAZ_redeTeste
# diretamente no processo do Shiny (sem subprocess Rscript).
#
# NOTA: o pacote SisMEVAZ permanece intacto - o patch e feito via
# assignInNamespace() em tempo de execucao, exatamente como o script
# original ja fazia ao ser chamado via Rscript pelo app Python.
# =====================================================================

.mevaz_selec_region_patched <- FALSE

normalize_model_values <- function(values) {
  normalized <- trimws(as.character(values))
  normalized <- iconv(normalized, from = "", to = "UTF-8", sub = "")
  normalized[normalized %in% c("Medias Regionais", "MÃ©dias Regionais", "M?dias Regionais")] <- "Médias Regionais"
  normalized
}

normalize_id_keys <- function(keys) {
  out <- trimws(as.character(keys))
  out <- sub("\\.0+$", "", out)
  out
}

# ---- patch_func_selec_region: aplica o patch uma unica vez por sessao R ----
patch_func_selec_region <- function() {
  if (.mevaz_selec_region_patched) return(invisible())

  consol_reservoirs_file <- file.path(REDE_RES_CONS, "input", "Dados_Reservatorios.xlsx")
  fallback_model_map <- NULL
  if (file.exists(consol_reservoirs_file)) {
    consol_df <- openxlsx::read.xlsx(consol_reservoirs_file, colNames = TRUE)
    if (all(c("ID", "MODELO_REGIONALIZACAO") %in% names(consol_df))) {
      fallback_ids <- normalize_id_keys(consol_df$ID)
      fallback_models <- normalize_model_values(consol_df$MODELO_REGIONALIZACAO)
      fallback_model_map <- fallback_models
      names(fallback_model_map) <- fallback_ids
    }
  }

  orig_select_region <- getFromNamespace("FUNC_SelecRegion", "SisMEVAZ")

  patched_select_region <- function(AfluInc_KNN, AfluInc_ML, ModRegion, RegHidro, Prec_med, Areas, datMedReg) {
    ModRegion <- normalize_model_values(ModRegion)
    ids <- names(AfluInc_KNN)

    id_keys <- normalize_id_keys(ids)
    mod_map <- ModRegion
    mod_names <- names(ModRegion)
    if (is.null(mod_names) || all(is.na(mod_names) | trimws(as.character(mod_names)) == "")) {
      if (length(ModRegion) == length(ids)) {
        mod_names <- ids
      } else {
        mod_names <- as.character(seq_along(ModRegion))
      }
    }
    names(mod_map) <- normalize_id_keys(mod_names)
    aligned_mod <- unname(mod_map[id_keys])
    names(aligned_mod) <- ids

    if (!is.null(fallback_model_map)) {
      need_fallback <- is.na(aligned_mod) | aligned_mod == ""
      if (any(need_fallback)) {
        fallback_values <- unname(fallback_model_map[id_keys[need_fallback]])
        aligned_mod[need_fallback] <- fallback_values
      }
    }

    missing_ids <- ids[is.na(aligned_mod) | aligned_mod == ""]
    if (length(missing_ids) > 0) {
      stop(sprintf("MODELO_REGIONALIZACAO ausente para IDs: %s",
                   paste(utils::head(missing_ids, 20), collapse = ", ")))
    }

    allowed <- c("KNN", "ML", "Multimodelo", "Médias Regionais")
    invalid_values <- unique(aligned_mod[!(aligned_mod %in% allowed)])
    if (length(invalid_values) > 0) {
      stop(sprintf("MODELO_REGIONALIZACAO invalido: %s", paste(invalid_values, collapse = ", ")))
    }

    orig_select_region(AfluInc_KNN, AfluInc_ML, aligned_mod, RegHidro, Prec_med, Areas, datMedReg)
  }

  assignInNamespace("FUNC_SelecRegion", patched_select_region, ns = "SisMEVAZ")
  .mevaz_selec_region_patched <<- TRUE
  invisible()
}

# ---- run_simulation: chama SisMEVAZ_redeTeste + print_Sintese + print_Series ----
# Identico a simula_rede_teste.R, mas em processo (sem Rscript subprocess).
# Levanta erro (stop) se SisMEVAZ_redeTeste ou SisMEVAZ_print_Sintese falharem;
# SisMEVAZ_print_Series segue tolerante a falha (so registra aviso), como no original.
run_simulation <- function(dados_dir = file.path(BASE_DIR, "dados")) {
  patch_func_selec_region()
  SisMEVAZ::SisMEVAZ_redeTeste(dados_dir)
  SisMEVAZ::SisMEVAZ_print_Sintese(dados_dir)
  tryCatch(
    SisMEVAZ::SisMEVAZ_print_Series(dados_dir),
    error = function(e) message("Aviso: SisMEVAZ_print_Series falhou: ", conditionMessage(e))
  )
  invisible(TRUE)
}
