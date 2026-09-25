# =====================================================================
# bacia_helpers.R  -  Fase 1/3
# Porte fiel do nucleo geometrico do tracado da bacia (bloco run-r-script):
#   trace_bacia_direct (equivalente a traca_bacia_shp_comando.R, mas chamado
#   diretamente no processo do Shiny, sem subprocess Rscript),
#   keep_largest_single_polygon, e a selecao do sub-bacia + overlay
#   intersecao/diferenca com calculo de areas.
#
# NAO existe funcao equivalente no pacote SisMEVAZ para nenhuma destas duas
# etapas (tracado de bacia via whitebox, ou overlay com os sub-bacias
# consolidados) - e logica especifica do app interativo, como no Python.
#
# Depende de: REDE_RES_CONS, DADOS_DASH, WORK_SHAPES_DIR, work_shape_path,
# write_shapefile_with_retry, remove_shapefile_if_exists (global.R/io_helpers.R),
# calculate_area_km2 (geo_helpers.R)
# =====================================================================

# ---- trace_bacia_direct: traça a bacia a montante do ponto (whitebox) ----
# Replica exatamente traca_bacia_shp_comando.R (mesmas chamadas whitebox +
# raster + stars, na mesma ordem), mas em processo, sem Rscript subprocess.
# Retorna a bacia tracada como sf (tambem grava bacia.tif/bacia.shp em
# WORK_SHAPES_DIR, como o script original).
trace_bacia_direct <- function(point_lon, point_lat) {
  direcao_fluxo_tif <- file.path(DADOS_DASH, "direcao_fluxo_anadem_v1_24M_recortado.tif")
  if (!file.exists(direcao_fluxo_tif)) {
    stop("Arquivo de direcao de fluxo nao encontrado: ", direcao_fluxo_tif,
         "\nVerifique se a pasta 'dados' esta completa (Dados_fixos/Dados_dash).")
  }

  bin_ok <- tryCatch(isTRUE(whitebox::check_whitebox_binary()), error = function(e) FALSE)
  if (!bin_ok) {
    stop("O executavel WhiteboxTools nao esta instalado. Rode instalacao/windows/Instalar-SisMEVAZ.bat (Windows) ",
         "ou ./instalacao/linux/instalar_sismevaz.sh (Linux/macOS).")
  }

  # Ponto do exutorio, sem CRS definido (identico ao original_crs=None do Python
  # e ao ponto sem CRS gravado por traca_bacia_shp_comando.R).
  pt_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(point_lon, point_lat))))
  pt_path <- work_shape_path("single_point.shp")
  write_shapefile_with_retry(pt_sf, pt_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(WORK_SHAPES_DIR)

  whitebox::wbt_watershed(
    d8_pntr = direcao_fluxo_tif,
    pour_pts = "single_point.shp",
    output = "bacia.tif",
    wd = WORK_SHAPES_DIR,
    verbose_mode = FALSE
  )

  if (!file.exists("bacia.tif")) {
    stop("O WhiteboxTools nao gerou 'bacia.tif' em: ", WORK_SHAPES_DIR,
         "\nVerifique o arquivo de direcao de fluxo e se o ponto esta dentro da area coberta por ele.")
  }

  bacia_raster <- raster::raster("bacia.tif")
  bacia_shape <- stars::st_as_stars(bacia_raster) |> sf::st_as_sf(merge = TRUE)
  sf::st_write(bacia_shape, "bacia.shp", append = FALSE, quiet = TRUE)

  bacia_shape
}

# ---- keep_largest_single_polygon: mantem so o maior poligono (explode + max area) ----
keep_largest_single_polygon <- function(input_shp, output_shp) {
  g <- sf::st_read(input_shp, quiet = TRUE)
  g <- sf::st_cast(g, "MULTIPOLYGON", warn = FALSE)
  g <- suppressWarnings(sf::st_cast(g, "POLYGON", warn = FALSE))  # explode multipart
  areas <- as.numeric(sf::st_area(g))
  keep <- g[which.max(areas), , drop = FALSE]
  remove_shapefile_if_exists(output_shp)
  dir.create(dirname(output_shp), showWarnings = FALSE, recursive = TRUE)
  sf::st_write(keep, output_shp, quiet = TRUE, delete_layer = TRUE)
  invisible(output_shp)
}

# ---- select_containing_and_overlay ----
# bacia_sf: a bacia tracada (sf). point_lon/point_lat em EPSG:4326.
# Percorre os shapes das sub-bacias consolidadas, encontra a que intercepta o
# ponto, e calcula intersecao/diferenca com a bacia (mesma logica do Python).
# Retorna list(number, containing_sf, intersection_sf, difference_sf,
#              bacia_area, intersection_area, difference_area, containing_area,
#              rio_barrado, ac_jusante) ou NULL se nenhuma interceptar.
select_containing_and_overlay <- function(bacia_sf, point_lon, point_lat) {
  bacia_crs <- sf::st_crs(bacia_sf)
  pt <- sf::st_transform(sf::st_sfc(sf::st_point(c(point_lon, point_lat)), crs = 4326), bacia_crs)

  bacia_geom <- sf::st_union(sf::st_geometry(bacia_sf))
  bacia_area <- calculate_area_km2(bacia_sf)

  sub_dir <- file.path(REDE_RES_CONS, "input", "shapes")
  files <- list.files(sub_dir, pattern = "\\.shp$", full.names = TRUE)

  for (f in files) {
    sb <- sf::st_read(f, quiet = TRUE)
    sb <- sf::st_transform(sb, bacia_crs)

    inter_pt <- suppressMessages(sf::st_intersects(sb, pt, sparse = FALSE))
    if (!any(inter_pt)) next

    containing <- sb
    number <- as.integer(sub(".*_(\\d+)\\.shp$", "\\1", basename(f)))

    cg <- sf::st_make_valid(sf::st_union(sf::st_geometry(containing)))
    inter_geom <- suppressWarnings(sf::st_intersection(cg, bacia_geom))
    diff_geom  <- suppressWarnings(sf::st_difference(cg, bacia_geom))

    intersection_area <- if (length(inter_geom) == 0) 0 else calculate_area_km2(inter_geom)
    difference_area   <- if (length(diff_geom)  == 0) 0 else calculate_area_km2(diff_geom)
    containing_area   <- calculate_area_km2(containing)

    rio_barrado <- if ("rio_barrad" %in% names(containing)) containing[["rio_barrad"]][1] else NA
    ac_jusante  <- if ("acude" %in% names(containing))      containing[["acude"]][1]      else NA

    return(list(
      standalone = FALSE,
      number = number,
      containing_sf = containing,
      intersection_geom = inter_geom,
      difference_geom = diff_geom,
      bacia_area = bacia_area,
      intersection_area = intersection_area,
      difference_area = difference_area,
      containing_area = containing_area,
      rio_barrado = rio_barrado,
      ac_jusante = ac_jusante,
      sub_dir = sub_dir,
      sub_basin_files = files,
      bacia_geom = bacia_geom
    ))
  }

  # ---- Caso "sem jusante": nenhuma sub-bacia consolidada contem o ponto ----
  # Nao e um erro: significa apenas que o novo reservatorio nao tem reservatorio
  # a jusante na rede consolidada (cerca de 1/3 da area do estado nao e coberta
  # por nenhuma sub-bacia). Marca-se standalone = TRUE, number = 0 (ID_ACJUS = 0,
  # convencao da rede para "sem jusante") e difference_geom = NULL, para que
  # nenhum shapefile de jusante seja reescrito (nao ha jusante a recortar).
  #
  # ATENCAO - a bacia INCREMENTAL nao e a bacia tracada inteira. A bacia tracada
  # pode conter sub-bacias consolidadas (os reservatorios a MONTANTE): essas
  # areas ja pertencem a esses reservatorios e a sua agua chega ao novo
  # reservatorio pela cascata, nao pelo escoamento incremental. Contabiliza-las
  # de novo contaria a mesma chuva duas vezes.
  #
  #   bacia incremental = bacia tracada - uniao das sub-bacias consolidadas
  #                                        contidas nela
  #
  # E o mesmo criterio do caso normal, onde a intersecao com o sub-bacia
  # contenedor ja exclui automaticamente as sub-bacias de montante (sao
  # poligonos distintos). Verificado no Alto Jaguaribe: bacia 1542,78 km2
  # menos os montantes (17, 171, 111 = ~1364 km2) da ~178 km2, praticamente
  # a intersecao calculada de 176,50 km2.
  #
  # NOTA: a versao Python falha neste caso (containing_sub_basin nunca e
  # atribuida -> UnboundLocalError capturado pelo except generico). Desvio
  # intencional para permitir reservatorios sem jusante na rede consolidada.
  pedacos <- list()
  for (f in files) {
    sb <- sf::st_transform(sf::st_read(f, quiet = TRUE), bacia_crs)
    sbg <- sf::st_make_valid(sf::st_union(sf::st_geometry(sb)))
    ig <- suppressWarnings(sf::st_intersection(sbg, bacia_geom))
    if (length(ig) == 0 || all(sf::st_is_empty(ig))) next
    pedacos[[length(pedacos) + 1L]] <- ig
  }

  if (length(pedacos) == 0) {
    inc_geom <- bacia_geom              # nenhum montante: incremental = total
  } else {
    ocupado  <- sf::st_make_valid(sf::st_union(do.call(c, pedacos)))
    inc_geom <- suppressWarnings(sf::st_difference(bacia_geom, ocupado))
  }
  inc_area <- if (length(inc_geom) == 0 || all(sf::st_is_empty(inc_geom))) {
    0
  } else calculate_area_km2(inc_geom)

  list(
    standalone = TRUE,
    number = 0,
    containing_sf = NULL,
    intersection_geom = inc_geom,     # bacia INCREMENTAL (total - montantes)
    difference_geom = NULL,           # nao ha jusante para recortar
    bacia_area = bacia_area,          # bacia TOTAL a montante do ponto
    intersection_area = inc_area,
    difference_area = 0,
    containing_area = 0,
    rio_barrado = NA,
    ac_jusante = NA,
    sub_dir = sub_dir,
    sub_basin_files = files,
    bacia_geom = bacia_geom
  )
}

# ---- process_bacia_trace ----
# Porte fiel do restante do bloco 'run-r-script' do Python: cria as tabelas
# de atributos (intersecao/diferenca), grava os shapefiles (graus_id_sagreh_*),
# atualiza 'tabela' (Tabela_atributos_acudes_Nova) e 'new_exut_acudes', e roda
# o 2o loop que reconecta sub-bacias irmas ao novo reservatorio.
#
# ov: resultado de select_containing_and_overlay()
# tabela: data.frame com colunas acude/id_sagreh/reg_hidrog/rio_barrad/id_ac_jus/Long/Lat
#         (inicialmente uma copia de TABELA_ATRIBUTOS_BASE)
# geo_sf: sf de exutorios (inicialmente EXUT_ACUDES_SF)
#
# Retorna list(tabela = data.frame atualizado, geo_sf = sf atualizado,
#              intersection_area, containing_area, rio_barrado (nome do rio))
process_bacia_trace <- function(ov, cod_value, nomeac_value, point_lon, point_lat,
                                tabela, geo_sf, bacia_crs,
                                regiao_fallback = NULL, rio_barrado_fallback = NULL) {
  number <- ov$number
  standalone <- isTRUE(ov$standalone)

  # No caso standalone (sem jusante) nao ha sub-bacia contendo o ponto de onde
  # herdar a regiao hidrografica / rio barrado, entao usam-se os valores vindos
  # da analise do ponto (find_containing_basin / find_nearest_drainage).
  filtered <- tabela[!is.na(tabela$id_sagreh) & tabela$id_sagreh == number, , drop = FALSE]
  regiao <- if (standalone) {
    if (!is.null(regiao_fallback) && !is.na(regiao_fallback) &&
        nzchar(regiao_fallback) && !identical(regiao_fallback, "N/A")) {
      regiao_fallback
    } else "Sem bacia definida"
  } else if (nrow(filtered) > 0) filtered$reg_hidrog[1] else "Sem bacia definida"

  rio_barrad_val <- if (standalone) {
    if (!is.null(rio_barrado_fallback) && !is.na(rio_barrado_fallback) &&
        nzchar(as.character(rio_barrado_fallback))) rio_barrado_fallback else NA
  } else ov$rio_barrado

  mk_attr_sf <- function(geom, area_km2) {
    if (length(geom) == 0 || sf::st_is_empty(geom)) return(NULL)
    sf::st_sf(
      acude = nomeac_value, id_sagreh = cod_value, nome = nomeac_value,
      gerencia = "Gerência do Açude Novo", reg_hidrog = regiao,
      municipio = "Município do Açude Novo", ini_monitoro = "dd/mm/aaaa",
      ano_constr = 9999, rio_barrad = rio_barrad_val, area_km2 = area_km2,
      ac_jusante = ov$ac_jusante, id_ac_jusante = number,
      geometry = sf::st_sfc(geom, crs = bacia_crs)
    )
  }

  intersection_sf <- mk_attr_sf(ov$intersection_geom, ov$intersection_area)
  difference_sf   <- mk_attr_sf(ov$difference_geom,   ov$difference_area)

  shapes_dir <- file.path(REDE_RES_TEST, "input", "shapes")
  dir.create(shapes_dir, showWarnings = FALSE, recursive = TRUE)

  if (!is.null(intersection_sf)) {
    write_shapefile_with_retry(intersection_sf, work_shape_path("intersection.shp"))
    write_shapefile_with_retry(intersection_sf, work_shape_path(sprintf("graus_id_sagreh_%s.shp", cod_value)))
    keep_largest_single_polygon(
      work_shape_path(sprintf("graus_id_sagreh_%s.shp", cod_value)),
      file.path(shapes_dir, sprintf("graus_id_sagreh_%s.shp", cod_value))
    )
  }

  if (!is.null(difference_sf)) {
    write_shapefile_with_retry(difference_sf, work_shape_path("difference.shp"))

    # Redefine o shape da sub-bacia a jusante como a diferenca (bacia - novo reservatorio).
    novo_shape <- work_shape_path(sprintf("graus_id_sagreh_%d_novo.shp", number))
    novo_shape_cortado <- if (number == 0) {
      work_shape_path(sprintf("graus_id_sagreh_%d_novo_cortado.shp", number))
    } else {
      file.path(shapes_dir, sprintf("graus_id_sagreh_%d.shp", number))
    }
    write_shapefile_with_retry(difference_sf, novo_shape)
    keep_largest_single_polygon(novo_shape, novo_shape_cortado)
  }

  # ---- Atualiza 'tabela' com o novo reservatorio ----
  new_row <- data.frame(
    acude = nomeac_value, id_sagreh = cod_value, reg_hidrog = regiao,
    rio_barrad = "Rio Barrado",  # replica literal do Python (nao e a variavel rio_barrado)
    id_ac_jus = number, Long = point_lon, Lat = point_lat
  )
  for (col in setdiff(names(tabela), names(new_row))) new_row[[col]] <- NA
  new_row <- new_row[, names(tabela), drop = FALSE]
  tabela <- rbind(tabela, new_row)

  # ---- Atualiza 'geo_sf' (new_exut_acudes) com o ponto do novo reservatorio ----
  # Usa os nomes de coluna REAIS de geo_sf (sf pode sanitizar nomes com aspas,
  # ex.: '"' -> 'X.'), na mesma ordem posicional do schema do Python:
  # ['"', Acude, ID, Long, Lat, ID_jusante, Area_incr_, CN, CN_min, 'CN_max"'].
  attr_names <- setdiff(names(geo_sf), "geometry")
  stopifnot(length(attr_names) == 10)
  id_jusante_val <- if (is.character(geo_sf[[attr_names[6]]])) as.character(number) else number

  new_vals <- list(NA, nomeac_value, cod_value, point_lon, point_lat,
                   id_jusante_val, ov$intersection_area, 999.9, 999.9, "0.0")
  names(new_vals) <- attr_names
  pt_geom <- sf::st_sfc(sf::st_point(c(point_lon, point_lat)), crs = sf::st_crs(geo_sf))
  new_point_row <- do.call(sf::st_sf, c(new_vals, list(geometry = pt_geom)))
  geo_sf <- rbind(geo_sf, new_point_row)

  # ---- 2o loop: sub-bacias irmas que tambem interceptam a nova bacia ----
  # (skip via numero do arquivo em vez de igualdade geometrica exata: uma
  # sub-bacia e unicamente identificada pelo seu numero de arquivo).
  #
  # O loop roda TAMBEM em modo standalone (number = 0). Nesse caso a condicao
  # id_ac_jus == number seleciona os reservatorios que hoje NAO tem jusante
  # (ID_ACJUS == 0) e que passam a drenar para o novo - exatamente o analogo
  # do caso normal. Nao ha risco de reconectar os 53 reservatorios de exutorio
  # da rede: o filtro GEOMETRICO (area_inter_deg2 > 0.001) vem ANTES do teste
  # topologico, entao so as sub-bacias que realmente sobrepoem a nova bacia
  # sao candidatas.
  #
  # Reservatorios que ja drenam para outro reservatorio situado DENTRO da nova
  # bacia nao sao tocados (ex.: 60 -> 112): apenas o de jusante (112) e
  # religado ao novo, e 60 continua drenando para 112. Isso preserva a
  # topologia interna da cascata a montante.
  for (f in ov$sub_basin_files) {
    number1 <- as.integer(sub(".*_(\\d+)\\.shp$", "\\1", basename(f)))
    if (identical(number1, number)) next
    sb <- sf::st_transform(sf::st_read(f, quiet = TRUE), bacia_crs)
    inter_geom <- suppressWarnings(sf::st_intersection(sf::st_geometry(sb), ov$bacia_geom))
    # Area de intersecao PLANAR em graus^2 - identico ao gpd.overlay(...).area
    # do Python (linha 2915), que calcula a area cartesiana ignorando o CRS
    # geografico (EPSG:4326). st_area em lon/lat devolveria m^2 (geodesico), e o
    # limiar 0.001 do Python esta em graus^2 (~12 km^2). Se comparassemos m^2
    # contra 0.001, o limiar viraria ~0 e reconectaria QUALQUER sub-bacia que
    # apenas encosta na nova bacia - inflando a cascata e o Dados_Novos com
    # reservatorios que NAO sao vizinhos imediatos (bug observado no Alto
    # Jaguaribe). Removemos o CRS para obter a area planar em graus^2.
    area_inter_deg2 <- if (length(inter_geom) == 0) 0 else {
      sum(as.numeric(sf::st_area(sf::st_set_crs(inter_geom, NA))))
    }
    if (is.na(area_inter_deg2) || area_inter_deg2 <= 0.001) next

    matching <- tabela[!is.na(tabela$id_ac_jus) & !is.na(tabela$id_sagreh) &
                        tabela$id_ac_jus == number & tabela$id_sagreh == number1, , drop = FALSE]
    if (nrow(matching) == 0) next

    id_sagreh_value <- matching$id_sagreh[1]
    tabela$id_ac_jus[!is.na(tabela$id_sagreh) & tabela$id_sagreh == id_sagreh_value] <- cod_value
    geo_sf$ID_jusante[!is.na(geo_sf$ID) & geo_sf$ID == id_sagreh_value] <- cod_value
  }

  # Persiste em disco, igual ao Python (o proximo passo, Trace a Cascata, le
  # esses arquivos de volta). Mantido mesmo numa sessao Shiny com estado em
  # memoria, para preservar paridade de arquivos e permitir inspecao manual.
  openxlsx::write.xlsx(tabela, work_shape_path("Tabela_atributos_acudes_Nova.xlsx"))
  write_shapefile_with_retry(geo_sf, work_shape_path("new_exut_acudes.shp"))

  # NOTA: containing_sub_basin_area_sq_km no Python = bacia_area_sq_km (a area
  # da PROPRIA bacia tracada), NAO a area do sub-bacia existente que a contem
  # (essa e ov$containing_area, usada so para referencia/mapa).
  list(tabela = tabela, geo_sf = geo_sf,
       intersection_area = ov$intersection_area,
       bacia_area = ov$bacia_area,
       containing_area = ov$containing_area,
       rio_barrado_nome = ov$rio_barrado)
}
