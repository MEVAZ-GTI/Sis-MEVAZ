# =====================================================================
# geo_helpers.R  -  Fase 1
# Porte fiel da "analise do ponto" da versao Python (Etapa 3):
#   calculate_area, find_nearest_point, find_nearest_drainage,
#   find_containing_basin, find_nearest_weather_station, comprehensive.
#
# Depende de objetos carregados no global.R:
#   DRAINAGE_SF  (Rios_mSimpl, SIRGAS2000/UTM24S, coluna TOPONIMIA)
#   VETOR_COORDS (matriz Nx2 lon/lat: vertices do vetor_rios ANADEM)
#   BASINS_SF    (regioes hidrograficas, EPSG:4326, coluna BACIA)
#   STATIONS_DF  (Estacoes_Evaporacao: COD, LAT, LONG)
# =====================================================================

# ---- calculate_area: area em km2 (Albers equal-area centrada na geometria) ----
# Espelha o calculate_area(geom) do Python (proj=aea sobre os bounds).
calculate_area_km2 <- function(geom) {
  if (inherits(geom, "sf"))  geom <- sf::st_geometry(geom)
  if (inherits(geom, "sfg")) geom <- sf::st_sfc(geom, crs = 4326)
  if (is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326
  geom <- sf::st_transform(geom, 4326)
  # Uma projecao aea por geometria, centrada nos seus bounds, e somada
  # (identico ao Python: geometry.apply(calculate_area).sum()).
  tot <- 0
  for (i in seq_along(geom)) {
    g1 <- geom[i]
    b <- sf::st_bbox(g1)
    aea <- sprintf(
      "+proj=aea +lat_1=%.12f +lat_2=%.12f +lat_0=%.12f +lon_0=%.12f +datum=WGS84 +units=m +no_defs",
      b[["ymin"]], b[["ymax"]], (b[["ymin"]] + b[["ymax"]]) / 2, (b[["xmin"]] + b[["xmax"]]) / 2
    )
    tot <- tot + as.numeric(sf::st_area(sf::st_transform(g1, aea)))
  }
  tot / 1e6
}

# ---- find_nearest_point: ponto mais proximo SOBRE a rede de drenagem ----
# Diferente do Python original (que usa cKDTree so nos vertices do vetor_rios,
# reproduzido aqui ate a Fase 3): perto de confluencias, o vertice mais
# proximo podia cair no canal errado mesmo quando outro canal passava mais
# perto em termos de distancia real ao segmento. Aqui usamos a distancia
# geometrica verdadeira ao segmento (nao so aos vertices) via GEOS, o que e
# estritamente mais preciso e nao muda o resultado quando o ponto mais
# proximo ja coincide com um vertice (validado contra os casos da Fase 1).
# Retorna c(lon, lat, distance) com distance em metros (haversine).
find_nearest_point <- function(lon, lat) {
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = sf::st_crs(VETOR_RIOS_LL))
  lines <- sf::st_geometry(VETOR_RIOS_LL)
  idx <- suppressMessages(sf::st_nearest_feature(pt, lines))
  np_line <- suppressMessages(sf::st_nearest_points(pt, lines[idx]))
  coords <- sf::st_coordinates(np_line)
  # unname obrigatorio: coords[, "X"/"Y"] carrega o nome "X"/"Y", que se
  # propaga pela aritmetica do haversine e faria o elemento virar
  # "distance.Y" (entao np["distance"] devolveria NA).
  snapped_lon <- unname(coords[nrow(coords), "X"])
  snapped_lat <- unname(coords[nrow(coords), "Y"])
  c(lon = snapped_lon, lat = snapped_lat,
    distance = unname(haversine_distance_m(lon, lat, snapped_lon, snapped_lat)))
}

# ---- find_nearest_drainage: nome (TOPONIMIA) da rede dentro de max_distance (m) ----
find_nearest_drainage <- function(lon, lat, max_distance = 1000) {
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  pt <- sf::st_transform(pt, sf::st_crs(DRAINAGE_SF))
  idx <- sf::st_nearest_feature(pt, DRAINAGE_SF)
  if (length(idx) == 0) return(NULL)
  d <- as.numeric(sf::st_distance(pt, DRAINAGE_SF[idx, ]))
  if (d > max_distance) return(NULL)
  list(drainage_name = DRAINAGE_SF$TOPONIMIA[idx], distance = d)
}

# ---- find_containing_basin: regiao hidrografica que contem o ponto (BACIA) ----
find_containing_basin <- function(lon, lat) {
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  hit <- sf::st_contains(BASINS_SF, pt, sparse = FALSE)[, 1]
  if (!any(hit)) return(NULL)
  i <- which(hit)[1]
  list(basin_name = as.character(BASINS_SF$BACIA[i]))
}

# ---- haversine em metros (identico ao Python) ----
haversine_distance_m <- function(lon1, lat1, lon2, lat2) {
  r <- 6371000.0
  phi1 <- lat1 * pi / 180; phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180; dl <- (lon2 - lon1) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dl / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

# ---- find_nearest_weather_station: COD da estacao dentro de max_distance (m) ----
find_nearest_weather_station <- function(lon, lat, max_distance = 350000) {
  dx <- STATIONS_DF$LONG - lon
  dy <- STATIONS_DF$LAT - lat
  i <- which.min(dx * dx + dy * dy)
  dm <- haversine_distance_m(lon, lat, STATIONS_DF$LONG[i], STATIONS_DF$LAT[i])
  if (!is.null(max_distance) && dm > max_distance) return(NULL)
  list(station_code = STATIONS_DF$COD[i], latitude = STATIONS_DF$LAT[i], distance = dm)
}

# ---- comprehensive: junta drenagem + bacia + estacao (como no Python) ----
comprehensive_point_analysis <- function(lon, lat) {
  drn <- tryCatch(find_nearest_drainage(lon, lat, 1000),         error = function(e) NULL)
  sta <- tryCatch(find_nearest_weather_station(lon, lat, 350000), error = function(e) NULL)
  bas <- tryCatch(find_containing_basin(lon, lat),               error = function(e) NULL)
  list(
    station_code  = if (!is.null(sta$station_code))  sta$station_code  else "N/A",
    basin_name    = if (!is.null(bas$basin_name))    bas$basin_name    else "N/A",
    drainage_name = if (!is.null(drn$drainage_name)) drn$drainage_name else "N/A"
  )
}
