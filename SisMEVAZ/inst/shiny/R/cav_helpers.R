# =====================================================================
# cav_helpers.R  -  Fase 1
# Porte fiel da logica do CAV da versao Python:
#   ensure_spillway_cota_in_cav (sem interpolacao),
#   backfill_cota_vert_in_dados_novos, save_cav (grava so o .xlsx).
#
# Depende de: REDE_RES_TEST (global.R)
# =====================================================================

# ---- np.isclose(a, b): |a-b| <= atol + rtol*|b| (atol=1e-9, rtol=1e-5) ----
.isclose <- function(a, b, atol = 1e-9, rtol = 1e-5) {
  abs(a - b) <= (atol + rtol * abs(b))
}

# ---- ensure_spillway_cota_in_cav: erro se a cota nao consta (nao interpola) ----
ensure_spillway_cota_in_cav <- function(df, cota_sang) {
  if (is.null(cota_sang) || (length(cota_sang) == 1 && is.na(cota_sang))) {
    stop("Informe a Cota do Sangrador antes de salvar o CAV.")
  }
  cota_sang <- as.numeric(cota_sang)
  if (!all(c("COTA", "AREA_KM2", "VOLUME_M3") %in% names(df))) {
    stop("Tabela CAV incompleta (faltam colunas COTA/AREA_KM2/VOLUME_M3).")
  }
  cota_vals <- suppressWarnings(as.numeric(df$COTA))
  cota_vals <- cota_vals[!is.na(cota_vals)]
  if (!any(.isclose(cota_vals, cota_sang))) {
    stop(sprintf(paste0(
      "Nao e possivel inserir a Cota do Sangrador (%g m): essa cota nao consta ",
      "na tabela CAV. Adicione uma linha com essa cota exata (Cota/Area/Volume) ",
      "antes de salvar; o sistema nao interpola esses valores."), cota_sang))
  }
  invisible(TRUE)
}

# ---- backfill_cota_vert_in_dados_novos: preenche COTA_VERT_M no arquivo transposto ----
backfill_cota_vert_in_dados_novos <- function(codigo, cota_sang) {
  if (is.null(cota_sang) || identical(trimws(as.character(cota_sang)), "")) return(invisible())
  path <- file.path(REDE_RES_TEST, "input", "Dados_Novos_Reservatorios.xlsx")
  if (!file.exists(path)) return(invisible())
  df <- tryCatch(openxlsx::read.xlsx(path, colNames = TRUE), error = function(e) NULL)
  if (is.null(df) || ncol(df) < 2) return(invisible())

  first_col <- names(df)[1]
  idx <- which(trimws(as.character(df[[first_col]])) == "COTA_VERT_M")
  if (length(idx) == 0) return(invisible())
  idx <- idx[1]

  target_col <- NULL
  for (col in names(df)[-1]) {
    if (identical(safe_to_int(col), safe_to_int(codigo))) { target_col <- col; break }
  }
  if (is.null(target_col)) return(invisible())

  df[[target_col]][idx] <- as.numeric(cota_sang)
  tryCatch(openxlsx::write.xlsx(df, path), error = function(e) NULL)
  invisible()
}

# ---- save_cav: valida e grava CAVs_Novos_Reservatorios.xlsx (sem .csv) ----
# table_data: data.frame/list com colunas COTA, AREA_KM2, VOLUME_M3 (e opc. ID)
# Levanta erro (stop) se a cota do sangrador nao constar. Retorna msg de sucesso.
save_cav <- function(codigo, cota_sang, table_data) {
  df <- as.data.frame(table_data, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("COTA", "AREA_KM2", "VOLUME_M3") %in% names(df))) {
    stop("Tabela CAV incompleta (faltam colunas COTA/AREA_KM2/VOLUME_M3).")
  }
  df$COTA      <- suppressWarnings(as.numeric(df$COTA))
  df$AREA_KM2  <- suppressWarnings(as.numeric(df$AREA_KM2))
  df$VOLUME_M3 <- suppressWarnings(as.numeric(df$VOLUME_M3))
  df$ID        <- as.integer(codigo)
  df <- df[!is.na(df$COTA) & !is.na(df$AREA_KM2) & !is.na(df$VOLUME_M3), , drop = FALSE]
  df <- df[order(df$COTA), c("ID", "COTA", "AREA_KM2", "VOLUME_M3"), drop = FALSE]

  ensure_spillway_cota_in_cav(df, cota_sang)  # stop() se cota ausente

  out <- file.path(REDE_RES_TEST, "input", "CAVs_Novos_Reservatorios.xlsx")
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  openxlsx::write.xlsx(df, out)

  backfill_cota_vert_in_dados_novos(codigo, cota_sang)
  "Dados salvos com sucesso!"
}
