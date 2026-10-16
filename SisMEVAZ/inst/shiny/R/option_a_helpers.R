# =====================================================================
# option_a_helpers.R  -  Fase 1
# Porte fiel da Opcao A do Python (gerar_dados_option_a): o usuario importa
# o proprio shapefile da bacia (em vez de tracar automaticamente) e informa
# o ID do reservatorio a jusante; o sistema gera Dados_Novos_Reservatorios.xlsx
# a partir dos campos preenchidos + dos dados consolidados do reservatorio
# a jusante.
#
# Depende de: REDE_RES_TEST (global.R), calculate_area_km2 (geo_helpers.R),
# normalize_modreg, write_shapefile_with_retry, write_dados_novos_transposed
# (io_helpers.R), keep_largest_single_polygon (bacia_helpers.R).
# =====================================================================

# Operador de fallback (equivalente a "x if x is not None else y" do Python).
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- generate_dados_novos_option_a ----
# bacia_shp_path: caminho para o .shp importado pelo usuario (com .shx/.dbf/.prj
#                 no mesmo diretorio). Deve ja ter CRS definido (equivalente a
#                 checagem bacia_gdf.crs is None do Python).
# id_jusante: ID do reservatorio a jusante (sera validado/convertido p/ inteiro).
# analysis: list(station_code, basin_name, drainage_name, selected_point=list(lon,lat))
#           (mesma origem do analysis_results do Python) - opcional.
# lat_value/lon_value: usados como fallback se analysis$selected_point for NULL.
#
# Retorna list(area_km2, output_file, transposed) ou levanta erro (stop) nos
# mesmos casos de validacao do Python (mensagens equivalentes).
prepare_uploaded_basin <- function(upload) {
  if (is.null(upload) || !nrow(upload)) stop("Selecione os arquivos da bacia.")
  names <- basename(upload$name)
  if (any(names != upload$name) || anyDuplicated(tolower(names))) stop("Os nomes dos arquivos da bacia são inválidos ou repetidos.")
  shp <- which(grepl("\\.shp$", names, ignore.case = TRUE))
  if (length(shp) != 1L) stop("Selecione uma única bacia em formato shapefile.")
  base <- tolower(tools::file_path_sans_ext(names[shp]))
  if (!all(paste0(base, c(".shp", ".shx", ".dbf", ".prj")) %in% tolower(names))) {
    stop("Selecione os arquivos .shp, .shx, .dbf e .prj da mesma bacia.")
  }
  directory <- tempfile("mevaz_basin_")
  dir.create(directory)
  valid <- FALSE
  on.exit(if (!valid) unlink(directory, recursive = TRUE))
  if (!all(file.copy(upload$datapath, file.path(directory, names)))) stop("Não foi possível copiar os arquivos da bacia.")
  path <- file.path(directory, names[shp])
  geometry <- sf::st_read(path, quiet = TRUE)
  if (!nrow(geometry) || all(sf::st_is_empty(geometry))) stop("O shapefile da bacia está vazio.")
  if (is.na(sf::st_crs(geometry))) stop("O shapefile precisa de um sistema de coordenadas válido (.prj).")
  if (!all(as.character(sf::st_geometry_type(geometry)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("O shapefile deve conter polígonos da bacia.")
  if (!all(sf::st_is_valid(geometry) %in% TRUE)) stop("Os polígonos da bacia possuem geometria inválida.")
  geometry <- sf::st_transform(geometry, 4326)
  area_km2 <- calculate_area_km2(geometry)
  valid <- TRUE
  list(path = path, directory = directory, geometry = geometry, area_km2 = area_km2)
}

generate_dados_novos_option_a <- function(bacia_shp_path, cod_value, nomeac_value,
                                          mod_reg_value, id_jusante,
                                          analysis = NULL, lat_value = NULL, lon_value = NULL) {

  if (is.null(bacia_shp_path) || !file.exists(bacia_shp_path)) {
    stop("Importe o shapefile da bacia (.shp, .shx, .dbf, .prj) antes de gerar os dados.")
  }
  if (is.null(cod_value) || identical(trimws(as.character(cod_value)), "")) {
    stop("Código do reservatório ausente. Volte à etapa 1.")
  }
  if (is.null(id_jusante) || identical(trimws(as.character(id_jusante)), "")) {
    stop("Informe o ID do reservatório a jusante.")
  }
  id_jusante_int <- suppressWarnings(as.integer(as.numeric(id_jusante)))
  if (is.na(id_jusante_int)) {
    stop("ID do reservatório a jusante inválido (use um número inteiro).")
  }

  bacia_sf <- tryCatch(sf::st_read(bacia_shp_path, quiet = TRUE),
                       error = function(e) stop(paste0("Erro ao processar o shapefile: ", conditionMessage(e))))
  if (nrow(bacia_sf) == 0) {
    stop("O shapefile importado está vazio.")
  }
  if (is.na(sf::st_crs(bacia_sf))) {
    stop("O shapefile não tem sistema de coordenadas (.prj). Adicione o .prj e tente novamente.")
  }
  bacia_sf <- sf::st_transform(bacia_sf, 4326)

  shapes_dir <- file.path(REDE_RES_TEST, "input", "shapes")
  dir.create(shapes_dir, showWarnings = FALSE, recursive = TRUE)
  tmp_shp <- work_shape_path(sprintf("graus_id_sagreh_%s.shp", cod_value))
  write_shapefile_with_retry(bacia_sf, tmp_shp)
  keep_largest_single_polygon(tmp_shp, file.path(shapes_dir, sprintf("graus_id_sagreh_%s.shp", cod_value)))

  area_km2 <- calculate_area_km2(bacia_sf)

  analysis <- analysis %||% list()
  station_code <- analysis$station_code %||% "N/A"
  basin_name <- analysis$basin_name %||% "N/A"
  drainage_name <- analysis$drainage_name %||% "N/A"
  sel_pt <- analysis$selected_point
  if (!is.null(sel_pt)) {
    final_lat <- as.numeric(sel_pt$lat); final_lon <- as.numeric(sel_pt$lon)
  } else if (!is.null(lat_value) && !is.null(lon_value) &&
             !identical(lat_value, "") && !identical(lon_value, "")) {
    final_lat <- as.numeric(lat_value); final_lon <- as.numeric(lon_value)
  } else {
    final_lat <- NA_real_; final_lon <- NA_real_
  }

  normalized_mod_reg <- normalize_modreg(mod_reg_value)
  if (is.na(normalized_mod_reg)) normalized_mod_reg <- "KNN"

  new_reservoir_data <- list(
    ACUDE = nomeac_value,
    COTA_VERT_M = NA,
    MODELO_REGIONALIZACAO = normalized_mod_reg,
    COD_EST_EVAP = if (!is.null(station_code) && !is.na(station_code)) station_code else "N/A",
    LONG = final_lon,
    LAT = final_lat,
    ID_ACJUS = id_jusante_int,
    REG_HIDRO = basin_name,
    RIO_BARRADO = drainage_name,
    AREA_BAC_TOT_KM2 = area_km2,
    AREA_BAC_INC_KM2 = area_km2
  )

  required_columns <- c("ID", "ACUDE", "COTA_VERT_M", "MODELO_REGIONALIZACAO",
                        "COD_EST_EVAP", "LONG", "LAT", "ID_ACJUS", "REG_HIDRO",
                        "RIO_BARRADO", "AREA_BAC_TOT_KM2", "AREA_BAC_INC_KM2")
  numeric_vars <- c("COTA_VERT_M", "COD_EST_EVAP", "LONG", "LAT", "ID_ACJUS",
                    "AREA_BAC_TOT_KM2", "AREA_BAC_INC_KM2")

  reservoir_table <- load_consolidated_reservoirs()
  existing_columns <- intersect(required_columns, names(reservoir_table))
  downstream <- reservoir_table[reservoir_table$ID == id_jusante_int, existing_columns, drop = FALSE]
  if (nrow(downstream) == 0 && id_jusante_int != 0) {
    stop(sprintf("Reservatório a jusante com ID %d não encontrado na rede consolidada.", id_jusante_int))
  }
  if (nrow(downstream) > 0 && "MODELO_REGIONALIZACAO" %in% names(downstream)) {
    m <- normalize_modreg(downstream$MODELO_REGIONALIZACAO[1])
    downstream$MODELO_REGIONALIZACAO[1] <- if (is.na(m)) "KNN" else m
  }

  vars <- setdiff(existing_columns, "ID")
  col_ids <- if (id_jusante_int == 0) as.character(cod_value) else c(as.character(cod_value), as.character(id_jusante_int))

  value_for <- function(var, col_id) {
    if (identical(col_id, as.character(cod_value))) {
      v <- new_reservoir_data[[var]]
      return(if (is.null(v)) NA else v)
    }
    downstream[[var]][1]
  }

  output_file <- file.path(REDE_RES_TEST, "input", "Dados_Novos_Reservatorios.xlsx")
  out <- write_dados_novos_transposed(vars, col_ids, value_for, numeric_vars, output_file)

  list(area_km2 = area_km2, output_file = out$output_file, transposed = out$transposed)
}
