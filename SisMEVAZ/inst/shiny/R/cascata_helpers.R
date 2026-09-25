# =====================================================================
# cascata_helpers.R  -  Fase 1
# Porte fiel de fcascata (construcao recursiva da cascata) da versao Python.
# A geracao de Dados_Novos_Reservatorios.xlsx (que usa fcascata) sera portada
# junto com o bloco de tracado.
# =====================================================================

# tabela_cas: data.frame com colunas id_sagreh, id_ac_jus, Lat, Long
# Retorna data.frame com colunas idto, idfrom, latto, lonto, latfrom, lonfrom
fcascata <- function(x, tabela_cas, df = NULL) {
  if (is.null(df)) {
    df <- data.frame(idto = numeric(0), idfrom = numeric(0),
                     latto = numeric(0), lonto = numeric(0),
                     latfrom = numeric(0), lonfrom = numeric(0))
  }

  eq <- function(col, val) !is.na(col) & col == val

  # 1) conexao a jusante de x (para incluir o acude a jusante do novo)
  df_down <- tabela_cas[eq(tabela_cas$id_sagreh, x), , drop = FALSE]
  if (nrow(df_down) > 0) {
    downstream_node <- df_down$id_ac_jus[1]
    if (!is.na(downstream_node)) {
      df_down_info <- tabela_cas[eq(tabela_cas$id_sagreh, downstream_node), , drop = FALSE]
      if (nrow(df_down_info) > 0) {
        df <- rbind(df, data.frame(
          idto = downstream_node, idfrom = x,
          latto = df_down_info$Lat[1], lonto = df_down_info$Long[1],
          latfrom = df_down$Lat[1], lonfrom = df_down$Long[1]
        ))
      }
    }
  }

  # 2) montantes de x (recursao)
  df_filt <- tabela_cas[eq(tabela_cas$id_ac_jus, x), , drop = FALSE]
  if (nrow(df_filt) > 0) {
    df_x <- tabela_cas[eq(tabela_cas$id_sagreh, x), , drop = FALSE]
    latto <- df_x$Lat[1]; lonto <- df_x$Long[1]
    for (i in seq_len(nrow(df_filt))) {
      df_y <- df_filt[i, ]
      df <- rbind(df, data.frame(
        idto = x, idfrom = df_y$id_sagreh,
        latto = latto, lonto = lonto,
        latfrom = df_y$Lat, lonfrom = df_y$Long
      ))
      ftmp <- fcascata(df_y$id_sagreh, tabela_cas, df)
      if (nrow(ftmp) > nrow(df)) df <- ftmp
    }
  }

  df
}

# ---- build_dados_novos ----
# Porte fiel do restante do bloco 'traca-cascata' do Python: usa fcascata para
# achar o reservatorio a jusante, atualiza ID_ACJUS na rede consolidada,
# aplica a correcao de area incremental do jusante, e grava o
# Dados_Novos_Reservatorios.xlsx TRANSPOSTO (variaveis em linhas, IDs em
# colunas), com a nova coluna do reservatorio inserida logo apos 'ID'.
#
# tabela_cas: data.frame (Tabela_atributos_acudes_Nova): id_sagreh, id_ac_jus, Lat, Long, acude
# reservoir_table: copia de load_consolidated_reservoirs() (mutavel nesta chamada)
# analysis: list(station_code, basin_name, drainage_name,
#                containing_sub_basin_area_sq_km, intersection_area_sq_km)
build_dados_novos <- function(cod_value, nomeac_value, cota_sang_value, mod_reg_value,
                              point_lon, point_lat, tabela_cas, reservoir_table, analysis,
                              standalone = FALSE) {

  # standalone = TRUE: o novo reservatorio nao tem jusante na rede consolidada
  # (nenhuma sub-bacia contem o ponto). Nesse caso a cascata e legitimamente
  # vazia - nao ha montante reconectado nem jusante - e o Dados_Novos deve
  # conter APENAS a coluna do novo reservatorio, com ID_ACJUS = 0 e sem
  # nenhuma subtracao de area incremental.
  cascata <- fcascata(cod_value, tabela_cas)
  if (nrow(cascata) > 0) {
    cascata$idto <- as.integer(cascata$idto)
    cascata$idfrom <- as.integer(cascata$idfrom)
    cascata <- unique(cascata)
    cascata_mevaz <- cascata[cascata$idto == cod_value | cascata$idfrom == cod_value, , drop = FALSE]
  } else {
    cascata_mevaz <- cascata
  }

  if (nrow(cascata_mevaz) == 0 && !standalone) {
    stop("Nao foi possivel tracar a cascata para o reservatorio selecionado. ",
         "Verifique conexoes montante-jusante (id_ac_jus/id_sagreh) em Tabela_atributos_acudes_Nova.xlsx.")
  }

  if (nrow(cascata_mevaz) == 0) {
    new_id_acjus <- 0
    reservoir_ids <- integer(0)
  } else {
    new_id_acjus <- cascata_mevaz$idto[1]
    reservoir_ids <- unique(c(cascata_mevaz$idto, cascata_mevaz$idfrom))
  }

  mask <- reservoir_table$ID %in% reservoir_ids
  reservoir_table$ID_ACJUS[mask] <- cod_value

  if (length(reservoir_ids) == 0) {
    new_id_acjus <- 0
  } else if (new_id_acjus == cod_value) {
    new_id_acjus <- 0
  } else {
    orig_row <- tabela_cas[!is.na(tabela_cas$id_sagreh) & tabela_cas$id_sagreh == new_id_acjus, , drop = FALSE]
    if (nrow(orig_row) > 0) {
      reservoir_table$ID_ACJUS[reservoir_table$ID == new_id_acjus] <- orig_row$id_ac_jus[1]
    }
  }

  required_columns <- c("ID", "ACUDE", "COTA_VERT_M", "MODELO_REGIONALIZACAO",
                        "COD_EST_EVAP", "LONG", "LAT", "ID_ACJUS", "REG_HIDRO",
                        "RIO_BARRADO", "AREA_BAC_TOT_KM2", "AREA_BAC_INC_KM2")
  existing_columns <- intersect(required_columns, names(reservoir_table))
  result_data <- reservoir_table[reservoir_table$ID %in% reservoir_ids, existing_columns, drop = FALSE]

  if ("MODELO_REGIONALIZACAO" %in% names(result_data)) {
    result_data$MODELO_REGIONALIZACAO <- vapply(result_data$MODELO_REGIONALIZACAO, function(v) {
      m <- normalize_modreg(v); if (is.na(m)) "KNN" else m
    }, character(1))
  }

  # Correcao da area incremental do reservatorio a jusante (recorte da bacia).
  if (!is.na(new_id_acjus) && new_id_acjus != 0 && "AREA_BAC_INC_KM2" %in% names(result_data)) {
    jus_mask <- result_data$ID == new_id_acjus
    if (any(jus_mask)) {
      area_nova_inc <- if (is.null(analysis$intersection_area_sq_km)) 0 else as.numeric(analysis$intersection_area_sq_km)
      area_jus_orig <- as.numeric(result_data$AREA_BAC_INC_KM2[jus_mask])
      result_data$AREA_BAC_INC_KM2[jus_mask] <- pmax(area_jus_orig - area_nova_inc, 0)
    }
  }

  # Transpoe: variaveis em linhas, reservatorios (IDs) em colunas.
  # IMPORTANTE: nao usar t() aqui - t.data.frame() converte tudo para
  # character quando ha colunas com tipos mistos (numerico + texto), perdendo
  # o tipo numerico das celulas. O pandas.transpose() do Python preserva o
  # tipo original de cada celula (dtype 'object').
  #
  # openxlsx nao lida bem com colunas-lista de tipos mistos (a deteccao
  # automatica de formato numerico chama is.nan() na coluna inteira, que
  # falha para uma lista). Como cada VARIAVEL (linha) e sempre do mesmo tipo
  # em todos os reservatorios (ex.: LONG e sempre numerico, ACUDE e sempre
  # texto), escrevemos linha a linha com vetores atomicos homogeneos -
  # equivalente ao resultado, sem o problema de tipo misto por coluna.
  ids <- as.character(result_data$ID)
  vars <- setdiff(names(result_data), "ID")
  col_ids <- c(as.character(cod_value), ids)  # nova coluna logo apos 'ID'

  numeric_vars <- c("COTA_VERT_M", "COD_EST_EVAP", "LONG", "LAT", "ID_ACJUS",
                    "AREA_BAC_TOT_KM2", "AREA_BAC_INC_KM2")

  normalized_mod_reg <- normalize_modreg(mod_reg_value)
  if (is.na(normalized_mod_reg)) normalized_mod_reg <- "KNN"
  station_code <- analysis$station_code
  new_reservoir_data <- list(
    ACUDE = nomeac_value,
    COTA_VERT_M = cota_sang_value,
    MODELO_REGIONALIZACAO = normalized_mod_reg,
    COD_EST_EVAP = if (!is.null(station_code) && !identical(station_code, "N/A") && !is.na(station_code)) station_code else "N/A",
    LONG = point_lon,
    LAT = point_lat,
    ID_ACJUS = new_id_acjus,
    REG_HIDRO = analysis$basin_name,
    RIO_BARRADO = analysis$drainage_name,
    AREA_BAC_TOT_KM2 = analysis$containing_sub_basin_area_sq_km,
    AREA_BAC_INC_KM2 = analysis$intersection_area_sq_km
  )

  # value_for: valor bruto (tipo original) de uma variavel/reservatorio.
  value_for <- function(var, col_id) {
    if (identical(col_id, as.character(cod_value))) {
      v <- new_reservoir_data[[var]]
      return(if (is.null(v)) NA else v)
    }
    idx <- which(ids == col_id)[1]
    result_data[[var]][idx]
  }

  output_file <- file.path(REDE_RES_TEST, "input", "Dados_Novos_Reservatorios.xlsx")
  out <- write_dados_novos_transposed(vars, col_ids, value_for, numeric_vars, output_file)

  list(transposed = out$transposed, output_file = out$output_file, cascata = cascata,
       cascata_mevaz = cascata_mevaz, new_id_acjus = new_id_acjus)
}
