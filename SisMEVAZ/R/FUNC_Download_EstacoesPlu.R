

#' Dowload de dados das estações pluviometricas
#'
#'
#' @param est_down Vetor de inteiros com o identificador das estações a serem baixadas
#' @param ano_i Ano inicial
#' @param ano_f Ano Final
#' @return Lista com dois elementos $series e $lonlat
#' as séries menasais de precipitação em cada estação no período determinado e
#' as coordenadas geográficas das estações
#' @import hydrobr
#' @export
Download_estacoesPlu<-function(est_down, ano_i, ano_f){

  if (!requireNamespace("hydrobr", quietly = TRUE)) {
    stop(
      paste0(
        "A atualização das estações pluviométricas não está disponível ",
        "porque o pacote 'hydrobr' não está instalado.\n\n",
        "O restante do Sis-MEVAZ continua disponível normalmente.\n",
        "Para habilitar novamente a atualização da pluviometria, ",
        "instale o pacote 'hydrobr'."
      ),
      call. = FALSE
    )
  }
  est_ce <- hydrobr::inventory(states = "CEARÁ",stationType = "plu")
  datas  <- seq(as.Date(paste0(ano_i,"-01-01")),as.Date(paste0(ano_f,"-12-01")), by="months")

  series<-numeric()
  for(est in est_down){

    i_ce    <- est_ce$station_code == est
    serie_i <- hydrobr::stationsData(est_ce[i_ce,])[[1]]
    serie_i <- as.data.frame(serie_i)

    serie_i_rec <- numeric()
    for(d in datas){
      selec <- which(serie_i$data==d)
      if(length(selec)==0){
        serie_i_rec <- c(serie_i_rec,NA)
      }else{
        prec        <- as.numeric(serie_i[selec,"total"])
        serie_i_rec <- c(serie_i_rec,prec)
      }
    }

    series <- cbind(series,serie_i_rec)
  }
  rownames(series)<-as.character(datas)
  colnames(series)<-est_down

  lonlat <- est_ce[is.element(est_ce$station_code,est_down),
                   c("station_code","long","lat")]

  lonlat <- as.data.frame(lonlat)
  return(list(series=series,lonlat=lonlat))
}
