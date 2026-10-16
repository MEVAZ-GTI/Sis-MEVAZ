river_name_available <- function(names) {
  names <- trimws(as.character(names))
  !is.na(names) & nzchar(names) & !toupper(names) %in% c("0", "NA", "N/A", "S/N", "SEM NOME", "-")
}

# Usa os nomes da propria geometria SRH, sem transferi-los para a malha ANADEM.
# Agrupar por nome reduz o numero de objetos interativos enviados ao mapa.
named_river_layer <- function(drainage) {
  keep <- river_name_available(drainage$TOPONIMIA) & !sf::st_is_empty(drainage)
  named <- drainage[keep, "TOPONIMIA", drop = FALSE]
  if (!nrow(named)) return(sf::st_sf(river_name = character(), geometry = sf::st_sfc(crs = 4326)))
  display <- sf::st_transform(named, 31984)
  display <- sf::st_simplify(display, dTolerance = 25)
  groups <- split(sf::st_geometry(display), trimws(named$TOPONIMIA))
  geometry <- lapply(groups, function(lines) sf::st_combine(lines)[[1]])
  display <- sf::st_sf(river_name = names(groups), geometry = sf::st_sfc(geometry, crs = 31984))
  sf::st_transform(display, 4326)
}

summary_name <- function(value) {
  if (length(value) != 1L || !river_name_available(value)) return("Não identificado na base")
  as.character(value)
}

summary_area <- function(value) {
  if (length(value) != 1L || !is.finite(value)) return("Não calculada")
  paste0(formatC(value, format = "f", digits = 2, decimal.mark = ",", big.mark = "."), " km²")
}
