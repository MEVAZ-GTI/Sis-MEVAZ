
#' Calcular Área e Perímetro de Polígonos
#'
#' Calcula a área e o perímetro de polígonos fornecidos em um objeto `sf` e retorna os resultados em km² e km, respectivamente.
#'
#' @param shpAc.UTM Lista de objetos `sf` representando os polígonos das bacias.
#' @param idsBac Vetor de IDs das bacias.
#' @return Uma lista com vetores de área (`vAreaAc`) e perímetro (`vPerAc`).
#' @import sf lwgeom
#' @export
FUNC_area_perim <- function(shpAc.UTM, idsBac){
  vAreaAc <- numeric()
  vPerAc <- numeric()
  for(i in 1:length(shpAc.UTM)){
    vAreaAc[i] <- st_area(shpAc.UTM[[i]]) / 10^6 # em km²
    vPerAc[i] <- st_perimeter(shpAc.UTM[[i]]) / 10^3 # em km
  }
  names(vAreaAc) <- idsBac
  names(vPerAc) <- idsBac

  return(list(vAreaAc = vAreaAc, vPerAc = vPerAc))
}

#' Calcular Comprimento Total de Drenagem
#'
#' Calcula o comprimento total dos rios interiores às bacias fornecidas.
#'
#' @param shpAc.UTM Lista de objetos `sf` representando os polígonos das bacias.
#' @param idsBac Vetor de IDs das bacias.
#' @param shp_Rio.UTM Objeto `sf` representando os rios.
#' @return Vetor com o comprimento total dos rios em cada bacia (em km).
#' @import sf
#' @import dplyr
#' @export
FUNC_compdren <- function(shpAc.UTM, idsBac, shp_Rio.UTM){
  vCTDAc <- numeric()
  for(i in 1:length(shpAc.UTM)){
    Inter <- st_intersection(shp_Rio.UTM, shpAc.UTM[[i]])%>%suppressWarnings()
    vCTDAc[i] <- sum(st_length(Inter)) / 10^3 # em km
  }
  names(vCTDAc) <- idsBac
  return(vCTDAc)
}

#' Calcular Área de Cristalino
#'
#' Calcula a área de cristalino interior às bacias fornecidas.
#'
#' @param shpAc.UTM Lista de objetos `sf` representando os polígonos das bacias.
#' @param idsBac Vetor de IDs das bacias.
#' @param shp_Cris.UTM Objeto `sf` representando as áreas de cristalino.
#' @return Vetor com a área de cristalino em cada bacia (em km²).
#' @import sf
#' @import dplyr
#' @export
FUNC_arecris <- function(shpAc.UTM, idsBac, shp_Cris.UTM){
  vCrisAc <- numeric()
  for(i in 1:length(shpAc.UTM)){
    Inter <- st_intersection(shp_Cris.UTM, shpAc.UTM[[i]])%>%suppressWarnings()
    vCrisAc[i] <- st_area(Inter) / 10^6 # em km²
    if(is.na(vCrisAc[i])){vCrisAc[i] <- 0}
  }
  names(vCrisAc) <- idsBac
  return(vCrisAc)
}

#' Calcular Capacidade de Água Disponível (CAD)
#'
#' Calcula a capacidade de água disponível para cada bacia com base na interseção com as áreas de solo.
#'
#' @param shpAc.UTM Lista de objetos `sf` representando os polígonos das bacias.
#' @param idsBac Vetor de IDs das bacias.
#' @param vAreaAc Vetor de áreas das bacias (em km²).
#' @param shp_solos.UTM Objeto `sf` representando as áreas de solo.
#' @return Vetor com a capacidade de água disponível em cada bacia.
#' @import sf
#' @import dplyr
#' @export
FUNC_cad <- function(shpAc.UTM, idsBac, vAreaAc, shp_solos.UTM){
  vCADAc <- numeric()
  L_solos <- lapply(1:nrow(shp_solos.UTM), function(x){shp_solos.UTM[x,]})
  CADsolos <- shp_solos.UTM$CAD5_2
  for(i in 1:length(shpAc.UTM)){
    AreaInter <- sapply(L_solos, function(sh){
      inter <- st_intersection(sh, shpAc.UTM[[i]])%>%suppressWarnings()
      AA <- st_area(inter) / 10^6 # em km²
      if(length(AA) == 0){
        return(0)
      } else {
        return(AA)
      }
    })
    vCADAc[i] <- sum(AreaInter / vAreaAc[i] * CADsolos)
  }
  names(vCADAc) <- idsBac
  return(vCADAc)
}

#' Calcular Média de Raster
#'
#' Calcula a média dos valores de um raster que intersectam as bacias fornecidas.
#'
#' @param shpAc.coordGeo Lista de objetos `sf` representando os polígonos das bacias.
#' @param idsBac Vetor de IDs das bacias.
#' @param rst Objeto `raster` a ser analisado.
#' @return Vetor com a média dos valores do raster em cada bacia.
#' @import raster
#' @export
FUNC_meanrst <- function(shpAc.coordGeo, idsBac, rst){
  vDeclAc <- numeric()
  for(i in 1:length(shpAc.coordGeo)){
    #print(i)
    Inter <- mask(rst, shpAc.coordGeo[[i]])
    vDeclAc[i] <- cellStats(Inter, stat = "mean")
    #rm(Inter)
  }
  names(vDeclAc) <- idsBac
  return(vDeclAc)
}

#' Calcular Características Fisiográficas
#'
#' Calcula várias características fisiográficas das bacias de acúmulo de água.
#'
#' @param idsBac Vetor de IDs das bacias.
#' @param arqBac Vetor de caminhos para os arquivos das bacias.
#' @param arqRio Caminho para o arquivo dos rios.
#' @param arqCris Caminho para o arquivo das áreas de cristalino.
#' @param arqSolo Caminho para o arquivo das áreas de solo.
#' @param arqDecl Caminho para o arquivo raster de declividade.
#' @param arqCN Caminho para o arquivo raster CN.
#' @return Matriz com várias características fisiográficas das bacias.
#' @import sf
#' @import dplyr
#' @import raster
#' @export
FUNC_CararacFisiogr_calc <- function(idsBac, arqBac, arqRio, arqCris, arqSolo, arqDecl, arqCN){

  # Leitura dos arquivos UTM
  shp_Rio.UTM <- read_sf(arqRio) # crs 31984
  shp_Cris.UTM <- read_sf(arqCris) # crs 31984
  shp_solos.UTM <- read_sf(arqSolo) # crs 31984

  # Leitura dos arquivos em graus
  rst_decl <- raster(arqDecl) # crs "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
  rst_CN <- raster(arqCN) # crs "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

  # Verificação dos sistemas de coordenadas
  if(st_crs(shp_Rio.UTM) != st_crs(31984)) { stop("Arquivo arqRio com CRS diferente do esperado SIRGAS 2000 / UTM zone 24S (EPSG 31984)") }
  if(st_crs(shp_Cris.UTM) != st_crs(31984)) { stop("Arquivo arqCris com CRS diferente do esperado SIRGAS 2000 / UTM zone 24S (EPSG 31984)") }
  if(st_crs(shp_solos.UTM) != st_crs(31984)) { stop("Arquivo arqSolo com CRS diferente do esperado SIRGAS 2000 / UTM zone 24S (EPSG 31984)") }
  if(st_crs(rst_decl) != st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) { stop("Arquivo rst_decl com CRS diferente do esperado SIRGAS 2000") }
  if(st_crs(rst_CN) != st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) { stop("Arquivo rst_CN com CRS diferente do esperado SIRGAS 2000") }

  # Leitura das shapes das bacias dos açudes
  shpAc <- list()
  shpAc.UTM <- list()
  shpAc.coordGeo <- list()
  for(i in 1:length(arqBac)){
    shpAc[[i]] <- read_sf(arqBac[i])
    # Verifica se as bacias têm CRS
    if(is.na(st_crs(shpAc[[i]]))){
      stop(paste0("O arquivo ", basename(arqBac[i]), " não tem Sistema de Coordenadas de Referência definido!"))
    }
    sf_use_s2(FALSE)%>%suppressMessages()
    shpAc.UTM[[i]] <- st_transform(shpAc[[i]], crs = st_crs(31984))
    shpAc.UTM[[i]] <- st_buffer(shpAc.UTM[[i]] ,dist = 0)
    shpAc.coordGeo[[i]] <- st_transform(shpAc[[i]], crs = st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  }
  names(shpAc.coordGeo) <- idsBac
  names(shpAc.UTM) <- idsBac

  # Área, Perímetro e Kc
  vGeom <- FUNC_area_perim(shpAc.UTM, idsBac)
  KcAc <- 0.5 * vGeom$vPerAc / sqrt(pi * vGeom$vAreaAc)

  print(" Densidade de Drenagem")
  # Densidade de Drenagem
  vCTDAc <- FUNC_compdren(shpAc.UTM, idsBac, shp_Rio.UTM)
  DDAc <- vCTDAc / vGeom$vAreaAc

  print(" Percentual de cristalino")
  # Percentual de cristalino
  vCrisAc <- FUNC_arecris(shpAc.UTM, idsBac, shp_Cris.UTM)
  vCrisAc <- vCrisAc / vGeom$vAreaAc

  print(" CAD")
  # CAD
  vCADAc <- FUNC_cad(shpAc.UTM, idsBac, vAreaAc = vGeom$vAreaAc, shp_solos.UTM)

  print(" Declividade")
  # Declividade
  vDeclAc <- FUNC_meanrst(shpAc.coordGeo, idsBac, rst = rst_decl)

  print(" CN")
  # CN
  vCNAc <- FUNC_meanrst(shpAc.coordGeo, idsBac, rst = rst_CN)


  # Matriz final com as características fisiográficas
  MatAc <- cbind(vGeom$vAreaAc, vGeom$vPerAc, vCTDAc, vCrisAc, KcAc, DDAc, vCADAc, vDeclAc, vCNAc)
  colnames(MatAc) <- c("Area_km2", "Perimetro_km", "CTD_km", "Cristalino", "Kc", "DD_km_km2", "CAD_mm", "Declividade_perc", "CN_mm")
  return(MatAc)
}


