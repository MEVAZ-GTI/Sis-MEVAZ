# =====================================================================
# io_helpers.R  -  Fase 1
# Porte fiel dos utilitarios nao-geometricos da versao Python:
#   normalize_modreg, safe_to_int, remove_shapefile_if_exists,
#   clear_directory_files, clear_previous_simulation_data,
#   prune_orphan_test_shapes, ensure_simulation_modreg_input.
#
# Depende de: REDE_RES_TEST, tabela_reservatorios_consolidada (global.R)
# =====================================================================

VALID_MODREG_VALUES <- c("KNN", "ML", "Multimodelo", "Médias Regionais")

# ---- normalize_modreg: rotulos canonicos do modelo (NA se invalido) ----
normalize_modreg <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_character_)
  if (length(value) > 1) value <- value[[1]]
  if (is.na(value)) return(NA_character_)
  txt <- trimws(as.character(value))
  if (identical(txt, "")) return(NA_character_)
  low <- tolower(txt)
  if (low == "knn") return("KNN")
  if (low == "ml") return("ML")
  if (low %in% c("multimodelo", "multi-modelo", "multi modelo")) return("Multimodelo")
  if (low %in% c("médias regionais", "medias regionais",
                 "media regional", "média regional")) return("Médias Regionais")
  NA_character_
}

# ---- safe_to_int: int(float(x)) do Python (trunca para zero), NA se falhar ----
safe_to_int <- function(value) {
  v <- suppressWarnings(as.numeric(value))
  if (length(v) == 0 || is.na(v)) return(NA_integer_)
  as.integer(trunc(v))
}

# ---- remove_shapefile_if_exists: apaga o .shp e seus arquivos irmaos ----
remove_shapefile_if_exists <- function(shp_path) {
  base <- sub("\\.shp$", "", shp_path)
  exts <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".sbn", ".sbx", ".shp.xml")
  for (ext in exts) {
    f <- paste0(base, ext)
    if (file.exists(f)) try(file.remove(f), silent = TRUE)
  }
}

# ---- write_shapefile_with_retry: grava um shapefile, com algumas tentativas
# em caso de arquivo bloqueado (equivalente ao write_shapefile_with_retry do
# Python, que lida com locks do Windows) ----
write_shapefile_with_retry <- function(sf_obj, shp_path, retries = 5, delay = 0.3) {
  dir.create(dirname(shp_path), showWarnings = FALSE, recursive = TRUE)
  last_err <- NULL
  for (attempt in seq_len(retries)) {
    remove_shapefile_if_exists(shp_path)
    ok <- tryCatch({
      sf::st_write(sf_obj, shp_path, quiet = TRUE, delete_layer = TRUE)
      TRUE
    }, error = function(e) { last_err <<- e; FALSE })
    if (isTRUE(ok)) return(invisible(shp_path))
    Sys.sleep(delay)
  }
  stop(sprintf("Falha ao escrever shapefile: %s (%s)", shp_path,
               if (!is.null(last_err)) conditionMessage(last_err) else ""))
}

# ---- write_dados_novos_transposed ----
# Escreve Dados_Novos_Reservatorios.xlsx no formato TRANSPOSTO (variaveis em
# linhas, reservatorios/IDs em colunas, nova coluna logo apos 'ID'), usado
# tanto pelo tracado (Opcao B, cascata_helpers.R) quanto pela Opcao A
# (importacao de shapefile).
#
# openxlsx nao lida bem com colunas-lista de tipos mistos com NA (a deteccao
# automatica de formato numerico chama is.nan() na coluna inteira). Como cada
# VARIAVEL (linha) e sempre do mesmo tipo em todos os reservatorios, escrevemos
# linha a linha com vetores atomicos homogeneos (numeric ou character).
#
# vars: nomes das variaveis (linhas), na ordem desejada.
# col_ids: vetor de character com os IDs das colunas, na ordem final desejada
#          (a coluna do novo reservatorio deve ja estar na posicao certa,
#          normalmente logo apos 'ID', i.e. col_ids[1] == as.character(cod_value)).
# value_for: function(var, col_id) -> valor bruto (tipo original, ou NA).
# numeric_vars: nomes de variaveis que devem ser escritas como numero.
write_dados_novos_transposed <- function(vars, col_ids, value_for, numeric_vars, output_file) {
  row_for <- function(var) {
    raw <- lapply(col_ids, function(cid) value_for(var, cid))
    if (var %in% numeric_vars) {
      suppressWarnings(vapply(raw, function(x) as.numeric(x), numeric(1)))
    } else {
      vapply(raw, function(x) if (is.na(x)) NA_character_ else as.character(x), character(1))
    }
  }

  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", x = matrix(c("ID", col_ids), nrow = 1),
                      startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Sheet1", x = matrix(vars, ncol = 1),
                      startRow = 2, startCol = 1, colNames = FALSE)

  row_values_by_var <- setNames(vector("list", length(vars)), vars)
  for (i in seq_along(vars)) {
    row_vals <- row_for(vars[i])
    row_values_by_var[[vars[i]]] <- row_vals
    openxlsx::writeData(wb, "Sheet1", x = matrix(row_vals, nrow = 1),
                        startRow = i + 1, startCol = 2, colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  transposed <- do.call(data.frame, c(
    list(ID = vars),
    setNames(lapply(seq_along(col_ids), function(j) {
      vapply(vars, function(v) as.character(row_values_by_var[[v]][j]), character(1))
    }), col_ids),
    list(check.names = FALSE, stringsAsFactors = FALSE)
  ))

  list(transposed = transposed, output_file = output_file)
}

# ---- clear_directory_files: remove todos os arquivos de uma pasta (nao recursivo) ----
clear_directory_files <- function(directory_path) {
  if (!dir.exists(directory_path)) return(invisible())
  files <- list.files(directory_path, full.names = TRUE)
  files <- files[!dir.exists(files)]
  for (f in files) try(file.remove(f), silent = TRUE)
}

# ---- clear_previous_simulation_data: limpa entradas/saidas de testes anteriores ----
clear_previous_simulation_data <- function() {
  test_input_files <- file.path(REDE_RES_TEST, "input", c(
    "Dados_Novos_Reservatorios.xlsx",
    "CAVs_Novos_Reservatorios.csv",
    "CAVs_Novos_Reservatorios.xlsx"
  ))
  for (p in test_input_files) if (file.exists(p)) try(file.remove(p), silent = TRUE)

  clear_directory_files(file.path(REDE_RES_TEST, "input", "shapes"))

  input_dir <- file.path(REDE_RES_TEST, "input")
  if (dir.exists(input_dir)) {
    up <- list.files(input_dir, pattern = "^CAV_.*_upload\\.csv$", full.names = TRUE)
    for (p in up) try(file.remove(p), silent = TRUE)
  }

  output_dir <- file.path(REDE_RES_TEST, "output")
  if (dir.exists(output_dir)) {
    files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
    for (p in files) try(file.remove(p), silent = TRUE)
  }

  local_shps <- c("single_point.shp", "bacia.shp", "bac_inc_selected.shp",
                  "intersection.shp", "difference.shp", "new_exut_acudes.shp")
  for (shp in local_shps) remove_shapefile_if_exists(work_shape_path(shp))

  for (p in c("Tabela_atributos_acudes_Nova.xlsx", "tab_cascata.txt")) {
    full <- work_shape_path(p)
    if (file.exists(full)) try(file.remove(full), silent = TRUE)
  }
  invisible()
}

# ---- prune_orphan_test_shapes: remove shapes sem entrada em Dados_Novos ----
prune_orphan_test_shapes <- function() {
  shapes_dir <- file.path(REDE_RES_TEST, "input", "shapes")
  dados_novos <- file.path(REDE_RES_TEST, "input", "Dados_Novos_Reservatorios.xlsx")
  if (!dir.exists(shapes_dir) || !file.exists(dados_novos)) return(invisible())

  df <- tryCatch(openxlsx::read.xlsx(dados_novos, colNames = TRUE),
                 error = function(e) NULL)
  if (is.null(df) || ncol(df) < 2) return(invisible())

  valid_ids <- unique(stats::na.omit(vapply(names(df)[-1], safe_to_int, integer(1))))

  for (name in list.files(shapes_dir)) {
    mm <- regmatches(name, regexec("^graus_id_sagreh_(\\d+)\\.shp$", name))[[1]]
    if (length(mm) < 2) next
    sid <- as.integer(mm[2])
    if (!(sid %in% valid_ids)) {
      remove_shapefile_if_exists(file.path(shapes_dir, sprintf("graus_id_sagreh_%d.shp", sid)))
    }
  }
  invisible()
}

# ---- ensure_simulation_modreg_input: garante MODELO_REGIONALIZACAO em todas as colunas ----
# Retorna list(ok = logico, message = texto). ok=FALSE bloqueia a simulacao.
ensure_simulation_modreg_input <- function() {
  input_file <- file.path(REDE_RES_TEST, "input", "Dados_Novos_Reservatorios.xlsx")
  if (!file.exists(input_file)) {
    return(list(ok = FALSE, message = paste0("Arquivo nao encontrado: ", input_file)))
  }
  df <- tryCatch(openxlsx::read.xlsx(input_file, colNames = TRUE),
                 error = function(e) e)
  if (inherits(df, "error")) {
    return(list(ok = FALSE, message = paste0("Nao foi possivel ler Dados_Novos_Reservatorios.xlsx: ",
                                             conditionMessage(df))))
  }
  if (nrow(df) == 0 || ncol(df) < 2) {
    return(list(ok = FALSE, message = "Dados_Novos_Reservatorios.xlsx vazio ou formato invalido."))
  }

  first_col <- names(df)[1]
  varnames <- trimws(as.character(df[[first_col]]))
  model_row <- which(varnames == "MODELO_REGIONALIZACAO")
  if (length(model_row) == 0) {
    return(list(ok = FALSE, message = paste0(
      "Linha MODELO_REGIONALIZACAO ausente em Dados_Novos_Reservatorios.xlsx. ",
      "Refaca o passo 'Trace a Cascata' para regenerar o arquivo.")))
  }
  model_row <- model_row[1]

  # fallback a partir da rede consolidada
  cons <- tabela_reservatorios_consolidada
  consolidated_modreg <- list()
  if (all(c("ID", "MODELO_REGIONALIZACAO") %in% names(cons))) {
    for (i in seq_len(nrow(cons))) {
      rid <- safe_to_int(cons$ID[i])
      if (is.na(rid)) next
      mod <- normalize_modreg(cons$MODELO_REGIONALIZACAO[i])
      if (!is.na(mod)) consolidated_modreg[[as.character(rid)]] <- mod
    }
  }

  filled_ids <- integer(0)
  unresolved_ids <- character(0)
  for (col in names(df)[-1]) {
    rid <- safe_to_int(col)
    current <- normalize_modreg(df[[col]][model_row])
    if (!is.na(current)) {
      df[[col]][model_row] <- current
      next
    }
    fallback <- if (!is.na(rid) && !is.null(consolidated_modreg[[as.character(rid)]]))
      consolidated_modreg[[as.character(rid)]] else NA_character_
    if (is.na(fallback)) fallback <- "KNN"
    df[[col]][model_row] <- fallback
    if (!is.na(rid)) filled_ids <- c(filled_ids, rid)
    if (is.na(normalize_modreg(df[[col]][model_row]))) {
      unresolved_ids <- c(unresolved_ids, if (!is.na(rid)) as.character(rid) else col)
    }
  }

  if (length(unresolved_ids) > 0) {
    return(list(ok = FALSE, message = paste0(
      "MODELO_REGIONALIZACAO ausente/invalido para IDs: ",
      paste(unresolved_ids, collapse = ", "))))
  }

  ok_write <- tryCatch({ openxlsx::write.xlsx(df, input_file); TRUE },
                       error = function(e) conditionMessage(e))
  if (!isTRUE(ok_write)) {
    return(list(ok = FALSE, message = paste0(
      "Falha ao atualizar MODELO_REGIONALIZACAO em Dados_Novos_Reservatorios.xlsx: ", ok_write)))
  }

  if (length(filled_ids) > 0) {
    return(list(ok = TRUE, message = paste0(
      "MODELO_REGIONALIZACAO preenchido automaticamente para IDs: ",
      paste(sort(unique(filled_ids)), collapse = ", "), ".")))
  }
  list(ok = TRUE, message = "")
}
