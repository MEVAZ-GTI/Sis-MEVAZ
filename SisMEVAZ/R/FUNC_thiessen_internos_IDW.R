# Função para calcular os pesos de Thiessen a partir de uma shape
#' Calcula os pesos de Thiessen para pontos dentro de uma shape
#'
#' Esta função calcula os pesos de Thiessen para um conjunto de pontos dentro de uma
#' área delimitada por uma shape (polígono). Os pesos são usados para determinar
#' a contribuição relativa de cada ponto em relação à área total.
#'
#' @param pontos Data.frame com as coordenadas $x e $y e identificador do ponto/posto $id.
#' @param shp_aq Spatial Feature (lida com `read_sf`) ou caminho para o arquivo com a shape.
#'
#' @return Vetor de pesos, onde cada peso corresponde à fração da área de Thiessen
#'         associada a cada ponto.
#' @import dplyr sf
#' @export
#'
#' @examples
#' pontos <- data.frame(x = c(1, 2), y = c(3, 4), id = c("A", "B"))
#' shp <- st_read("path_to_shapefile.shp")
#' FUNC_thiessen(pontos, shp)
FUNC_thiessen <- function(pontos, shp_aq) {
   # Carrega a shape caso um caminho seja fornecido
   if (is.character(shp_aq)) {
      shp <- read_sf(shp_aq)
   } else {
      shp <- shp_aq
   }

   # Converte os pontos para uma classe sf (spatial feature)
   pontos_sf <- pontos %>%
      st_as_sf(coords = c("x", "y"))

   # Define o sistema de coordenadas dos pontos como o mesmo da shape
   st_crs(pontos_sf) <- st_crs(shp)

   # Gera os polígonos de Thiessen (Voronoi) para os pontos
   thiessen_polygons <- pontos_sf %>%
      st_geometry() %>%
      do.call(c, .) %>%
      st_voronoi() %>%
      st_collection_extract()

   # Define o sistema de coordenadas dos polígonos de Thiessen
   st_crs(thiessen_polygons) <- st_crs(shp)
   thiessen_polygons <- st_as_sf(thiessen_polygons)

   # Desativa geometria s2 para interseções, garantindo precisão
   sf_use_s2(FALSE)

   # Ordena os polígonos de Thiessen para corresponder à ordem dos pontos
   mat_inter <- {st_intersects(pontos_sf, thiessen_polygons, sparse = FALSE)}%>%suppressMessages()
   ordena_polig <- apply(mat_inter, 1, which)

   thiessen.sort <- thiessen_polygons[ordena_polig,]
   thiessen.sort$id <- pontos$id

   # Calcula a fração da área em cada polígono de Thiessen
   intersec <- {st_intersection(thiessen.sort, shp)}%>%suppressWarnings()%>%suppressMessages()
   vec_weights <- rep(0, nrow(pontos))
   names(vec_weights) <- pontos$id
   vec_weights[as.character(intersec$id)] <- st_area(intersec) %>% as.numeric()
   vec_weights <- vec_weights / sum(vec_weights)

   # Retorna o vetor de pesos de Thiessen
   return(vec_weights)
}

# Função para identificar pontos interiores a uma shape
#' Identifica pontos que estão dentro de uma shape
#'
#' Esta função verifica quais pontos de um conjunto de coordenadas estão localizados
#' dentro de uma área delimitada por uma shape (polígono).
#'
#' @param pontos Data.frame com as coordenadas $x e $y e identificador do ponto/posto $id.
#' @param shp_aq Spatial Feature (lida com `read_sf`) ou caminho para o arquivo com a shape.
#'
#' @return Vetor de IDs dos pontos que estão dentro da shape.
#' @import dplyr sf
#' @export
#' @examples
#' pontos <- data.frame(x = c(1, 2), y = c(3, 4), id = c("A", "B"))
#' shp <- st_read("path_to_shapefile.shp")
#' FUNC_pts_internos(pontos, shp)
FUNC_pts_internos <- function(pontos, shp_aq) {
   # Carrega a shape caso um caminho seja fornecido
   if (is.character(shp_aq)) {
      shp <- read_sf(shp_aq)
   } else {
      shp <- shp_aq
   }

   # Converte os pontos para uma classe sf (spatial feature)
   pontos_sf <- pontos %>%
      st_as_sf(coords = c("x", "y"))

   # Define o sistema de coordenadas dos pontos como o mesmo da shape
   st_crs(pontos_sf) <- st_crs(shp)

   # Desativa geometria s2 para interseções, garantindo precisão
   sf_use_s2(FALSE)

   # Identifica os pontos que estão dentro da shape
   internos <- suppressMessages({st_intersects(pontos_sf, shp, sparse = FALSE)}) %>% which()
   ids_internos <- pontos_sf$id[internos]

   # Retorna os IDs dos pontos internos
   return(ids_internos)
}


# Função para calcular a ETP média ponderada pelos pesos de Thiessen
#' Calcula a ETP média ponderada por Thiessen para várias bacias
#'
#' Esta função calcula a evapotranspiração potencial média (ETP) para
#' várias bacias hidrográficas, utilizando a ponderação pelos pesos de Thiessen
#' calculados para cada bacia.
#'
#' @param MatETP Matriz com valores de ETP média mensal por ponto. Nomeadas nas linhas pelo id dos ponstos.
#' @param pontos Data.frame com as coordenadas $x e $y e identificador do ponto/posto $id.
#' @param arqBac Lista de caminhos para os arquivos das shapes das bacias hidrográficas.
#' @param idsBac Vetor de IDs das bacias hidrográficas.
#'
#' @return Lista onde cada elemento corresponde à ETP média ponderada para uma bacia.
#' @export
FUNC_ETPmed_thiessen <- function(MatETP, pontos, arqBac, idsBac) {
   nETO_bacias <- list()
   for (i in 1:length(arqBac)) {
      thi <- FUNC_thiessen(pontos, arqBac[i])
      Mtmp <- sapply(names(thi), function(x) {
         thi[names(thi) == x] * MatETP[as.character(x), ]
      })
      nETO_bacias[[i]] <- apply(Mtmp, 1, sum)
   }
   names(nETO_bacias) <- idsBac
   return(nETO_bacias)
}

# Função para calcular a precipitação média em bacias hidrográficas com pontos internos
#' Calcula a precipitação média para várias bacias hidrográficas utilizando apenas pontos internos
#'
#' Esta função calcula a precipitação média em cada bacia hidrográfica, considerando apenas
#' os pontos que estão localizados dentro de cada bacia. A precipitação média é calculada
#' para cada bacia utilizando a lista de precipitações fornecida.
#'
#' @param listPrec Lista onde cada elemento é um vetor com os valores de precipitação para cada ponto.
#' @param pontos Data.frame com as coordenadas $x e $y e identificador do ponto/posto $id.
#' @param arqBac Lista de caminhos para os arquivos das shapes das bacias hidrográficas.
#' @param idsBac Vetor de IDs das bacias hidrográficas.
#'
#' @return Lista onde cada elemento corresponde à precipitação média para uma bacia,
#'         considerando apenas os pontos internos.
#'         @export
FUNC_Precmed_internos <- function(listPrec, pontos, arqBac, idsBac) {
   Pre_bacia <- list()  # Inicializa uma lista para armazenar a precipitação média por bacia

   # Loop através de cada bacia hidrográfica
   for (i in 1:length(arqBac)) {
      # Identifica os pontos que estão dentro da bacia atual
      id_internos <- FUNC_pts_internos(pontos, arqBac[i])

      # Calcula a precipitação média para a bacia atual, considerando apenas os pontos internos
      Pre_bacia[[i]] <- sapply(listPrec, function(P) { mean(P[id_internos]) })
      names(Pre_bacia[[i]]) <- names(listPrec)  # Atribui os nomes dos elementos da lista de precipitação
   }

   names(Pre_bacia) <- idsBac  # Atribui os nomes das bacias às entradas da lista
   return(Pre_bacia)  # Retorna a lista com a precipitação média por bacia
}

# Função para interpolar a precipitação média em uma grade
#'
#' Interpolação pelo ponderada pelo inverso da distância (IDW)
#'
#' @param listPrec Lista onde cada elemento é um vetor com os valores de precipitação para cada ponto.
#' @param pontos Data.frame com as coordenadas $x e $y e identificador do ponto/posto $id.
#' @param arqBac Lista de caminhos para os arquivos das shapes das bacias hidrográficas.
#' @param idsBac Vetor de IDs das bacias hidrográficas.
#'
#' @return Lista onde cada elemento corresponde à precipitação média para uma bacia,
#'         considerando apenas os pontos internos.
#' @import phylin
#' @export
FUNC_IDW <- function(series_est, coord_est, grid) {

  inds_com <- lapply(1:nrow(series_est), function(i){
    est_com_dados_i <- which(!is.na(series_est[i,]))
    return(est_com_dados_i)
  })

  if(any(sapply(inds_com, length) == 0)){stop("Há datas sem estações com dados") }

  interp <- lapply(1:nrow(series_est), function(i){
    est_com_dados_i <- inds_com[[i]]
    prec<-phylin::idw(values = series_est[i,est_com_dados_i],
                      coord_est[est_com_dados_i,],grid)
    # grid.image(prec,grid)
    # text(coord_est[est_com_dados_i,1],coord_est[est_com_dados_i,2],round(series_est[i,est_com_dados_i]),cex=0.5)
    return(prec$Z)
  })
  names(interp) <- rownames(series_est)
  return(list(P=interp, pontos=grid))
}

