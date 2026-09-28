
#' Agregação Anual de Séries Temporais
#'
#' Agrega séries temporais em uma base anual, aplicando uma função de agregação especificada.
#'
#' @param Lista_series Lista contendo séries temporais nomeadas.
#' @param func_agg Função de agregação a ser aplicada (padrão: `mean`).
#' @return Lista com as séries agregadas anualmente. Valores para anos incompletos são definidos como `NA`.
#' @import xts
#' @export
Agg_anual<-function(Lista_series, func_agg=mean){
  Resp=lapply(Lista_series, function(x){
    datas=as.Date(names(x))
    conta = table(year(datas))
    anos = names(conta)
    serie_xts = xts(x,order.by = datas)
    S = apply.yearly(serie_xts,func_agg)
    S = as.numeric(S)
    S[conta!=12] = NA
    names(S)=as.character(seq(as.Date(paste0(anos[1],"-01-01")),
                              as.Date(paste0(tail(anos,1),"-01-01")),
                              by="years"))
    return(S)
  })
  return(Resp)
}

#' Converter Matrizes de Médias Mensais para Séries Temporais
#'
#' Converte uma matriz de médias mensais em uma lista de séries temporais associadas a estações.
#'
#' @param Mat_MedMensal Data frame com médias mensais, onde colunas de 3 a 14 representam meses.
#' @param ResEst Data frame contendo informações sobre estações (códigos e IDs).
#' @param Datas Vetor de datas associado às séries.
#' @return Lista de séries temporais para cada estação.
#' @export
MedMensalEst_para_SerieRes <- function(Mat_MedMensal, ResEst, Datas){
  meses <- month(Datas)
  L_series <- list()
  for(i in 1:nrow(ResEst)){
    est   <- ResEst$COD_EST_EVAP[i]
    res   <- ResEst$ID[i]
    Evapm <- Mat_MedMensal[Mat_MedMensal$COD==est,3:14]
    serie <- sapply(meses,function(m){as.numeric(Evapm[m])})
    names(serie)    <- Datas
    L_series[[res]] <- serie
  }
  return(L_series)
}

#' Extrair Dados de Afluência Total de Listas de Simulação
#'
#' Extrai dados de afluência total a partir de uma lista de simulações.
#'
#' @param ListSimul Lista contendo resultados de simulações.
#' @return Lista de séries temporais de afluência total para cada simulação.
#' @export
Get_AfluTot_fromListSimul <- function(ListSimul){
  ListAflul <- lapply(ListSimul, function(li){
    lapply(li, function(si){
    af <- si$Aflu_m3s
    names(af) <- as.character(si$Datas)
    return(af)
  })})
  return(ListAflul)
}

#' Agregação de Regiões Hidrográficas
#'
#' Realiza a agregação de dados por região hidrográfica, considerando capacidade e dados médios.
#'
#' @param RegHidro Vetor com regiões hidrográficas para cada reservatório.
#' @param Capacid Vetor com capacidades dos reservatórios.
#' @param L_DadosRedeBalancoMedio Lista de dados médios da rede.
#' @return Lista com os dados agregados por região hidrográfica.
#' @export
Agg_RegHidr <- function(RegHidro, Capacid, L_DadosRedeBalancoMedio){

  n_mod <- names(L_DadosRedeBalancoMedio)
  n_qg <- names(L_DadosRedeBalancoMedio[[1]])

  reg_unique <- sort(unique(RegHidro))

  L_DadosRedeBalanco_Agg <- list()
  for(mod in 1:length(n_mod)){
    L_DadosRedeBalanco_Agg[[mod]] <- list()
    for(qg in 1:length(n_qg)){
      Dados <- L_DadosRedeBalancoMedio[[mod]][[qg]]
      L_DadosRedeBalanco_Agg[[mod]][[qg]]<-numeric()
      for(reg in 1:length(reg_unique)){
        ids_reg <- names(RegHidro)[RegHidro==reg_unique[reg]]
        vec_reg <- c(sum(Dados[ids_reg,"Afluencia_hm3_ano"]),
                     NA,
                     NA,
                     sum(Dados[ids_reg,"Q9X_l_s"]),
                     sum(Dados[ids_reg,"Regularizado_hm3_ano"]),
                     NA,
                     sum(Dados[ids_reg,"Vertido_hm3_ano"]),
                     NA,
                     sum(Dados[ids_reg,"Evaporado_hm3_ano"]),
                     NA)
        names(vec_reg) <- colnames(Dados)
        vec_reg["Cap_VA"]            <- sum(Capacid[ids_reg])/vec_reg["Afluencia_hm3_ano"]
        vec_reg["Regularizado_perc"] <-vec_reg["Regularizado_hm3_ano"]/vec_reg["Afluencia_hm3_ano"]
        vec_reg["Vertido_perc"] <-vec_reg["Vertido_hm3_ano"]/vec_reg["Afluencia_hm3_ano"]
        vec_reg["Evaporado_perc"] <-vec_reg["Evaporado_hm3_ano"]/vec_reg["Afluencia_hm3_ano"]

        L_DadosRedeBalanco_Agg[[mod]][[qg]]<- rbind(L_DadosRedeBalanco_Agg[[mod]][[qg]],vec_reg)
      }
      rownames(L_DadosRedeBalanco_Agg[[mod]][[qg]])<-reg_unique
    }
    names(L_DadosRedeBalanco_Agg[[mod]])<-n_qg
  }
  names(L_DadosRedeBalanco_Agg)<-n_mod
  return(L_DadosRedeBalanco_Agg)
}

#' Adicionar Linhas de Agregação a Tabelas de Dados
#'
#' Insere linhas de agregação de regiões hidrográficas em listas de data frames.
#'
#' @param RegHidro Vetor com regiões hidrográficas para cada reservatório.
#' @param Capacid Vetor com capacidades dos reservatórios.
#' @param DF_list Lista de data frames onde as linhas serão adicionadas.
#' @param L_DadosRedeBalancoMedio_AggReg Lista de dados agregados por região.
#' @param ind_acres Vetor de índices indicando onde adicionar as novas linhas.
#' @return Lista de data frames com as linhas adicionadas.
#' @export
Add_linhasAggRegHidro <- function(RegHidro, Capacid, DF_list, L_DadosRedeBalancoMedio_AggReg, ind_acres){
  vec_ncol     <- sapply(DF_list,ncol)-30
  reg_unique <- sort(unique(RegHidro))

  for(kk in 1:length(DF_list)){
    DF <- DF_list[[kk]]

   for(i in 1:length(ind_acres)){
      #linha a adicionar
     linha_add  <- c(rep(NA,vec_ncol[kk]),
                     L_DadosRedeBalancoMedio_AggReg[[kk]]$Q90[i,],
                     L_DadosRedeBalancoMedio_AggReg[[kk]]$Q95[i,],
                     L_DadosRedeBalancoMedio_AggReg[[kk]]$Q98[i,])

     names(linha_add) <- names(DF)
     linha_add <- as.data.frame(lapply(linha_add,identity))

     linha_add$Regiao_Hidrografica <- reg_unique[i]
     linha_add$Capacidade_hm3      <- sum(Capacid[RegHidro==reg_unique[i]])

     ind_acres_dinamico <- ind_acres[i]+(i-1)
      if(ind_acres_dinamico<=nrow(DF)){
        DF <-  rbind(DF[1:(ind_acres_dinamico-1),],linha_add,DF[ind_acres_dinamico:nrow(DF),])
      }else{
        DF <-  rbind(DF[1:(ind_acres_dinamico-1),],linha_add)
      }

    }
    DF_list[[kk]] <- DF
  }
  names(DF_list) <-  c("KNN", "ML", "Selec")
  return(DF_list)
}

#' Verificar Ordem Crescente em Colunas de um Data Frame
#'
#' Identifica IDs que possuem colunas não ordenadas crescentemente.
#'
#' @param CAVs Data frame contendo colunas a serem verificadas.
#' @return Vetor de IDs que apresentam colunas fora de ordem.
#' @export
Verif_CAVs_ordcres <- function(CAVs){
  ids <- unique(CAVs$ID)
  ids_cor <- numeric()
  for(i in 1:length(ids)){
    cav_i <- CAVs[CAVs$ID==ids[i],2:4]
    tem_decres <- any(apply(cav_i,2,function(col){any(order(col)!=1:length(col))}))
    if(tem_decres){ ids_cor <- c(ids_cor,ids[i])}
  }
  return(ids_cor)
}


#' Força as CAVs a serem todas crescentes
#'
#' Apaga as linhas das CAVs para garantir que a CAV é crescente
#'
#' @param CAVs Data frame contendo colunas a serem verificadas.
#' @param ids_cor Vetor de IDs que apresentam colunas fora de ordem.
#' @return CAVs corrigidas
#' @export
Forca_CAVs_ordcres <- function(CAVs,ids_cor){
  linhas_rm <- numeric()
  for(i in 1:length(ids_cor)){
    cav_i    <- CAVs[CAVs$ID==ids_cor[i],]
    ind_area <- which(diff(cav_i$AREA_KM2)<0)
    ind_vol  <- which(diff(cav_i$VOLUME_M3)<0)
    # O problema pode ser i, no i+1 ou em ambos
    ind      <- unique(c(ind_area,ind_area+1,ind_vol,ind_vol+1))
    linhas_rm<- rbind(linhas_rm,cav_i[ind,])
  }
  ind_CAVs <- apply(linhas_rm,1, function(ln){which(apply(CAVs,1, function(lnstot){all(lnstot==ln)}))})
  CAVs_cor <- CAVs
  CAVs_cor <- CAVs_cor[-ind_CAVs,]
  return(CAVs_cor)
}


#' Leitura de CAVs a Partir do Banco de Dados
#'
#' Realiza a leitura dos dados de CAVs para um conjunto de reservatórios a partir de um banco de dados PostgreSQL.
#'
#' @param id_res Vetor. IDs dos reservatórios cujos dados de CAVs serão lidos.
#' @param dbname Nome do banco de dados. Padrão: "func".
#' @param host Host do banco de dados. Padrão: "flora.funceme.br".
#' @param port Porta do banco de dados. Padrão: 5432.
#' @param user Usuário para conexão. Padrão: "consulta".
#'
#' @return Data frame. Matriz com os dados de CAVs, incluindo colunas de ID, cota, volume (em m³) e área (em km²).
#' @export
#'
#' @examples
#' # Exemplo de uso
#' cavs <- FUNC_ler_CAVs_banco(c(1, 2, 3))
#'
#' @importFrom RPostgreSQL dbConnect dbGetQuery dbDisconnect
FUNC_ler_CAVs_banco <- function(id_res,
                                dbname = "func",
                                host = "flora.funceme.br",
                                port = 5432,
                                user = "alyson_estacio") {
  
  connection <- dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = dbname,
                          host = host,
                          port = port,
                          user = user,
                          password = "n6cj3zy4xB3W5gP")
  
  L_CAV <- lapply(id_res, function(id) {
    dbGetQuery(connection, paste0("SELECT * FROM acude.referencia_cav_cogerh",
                                  " WHERE rca_reservatorio = ", id,
                                  " ORDER BY rca_cota "))
  })
  
  on.exit(dbDisconnect(connection))
  
  mat_CAV <- do.call(rbind, L_CAV)
  mat_CAV <- mat_CAV[, c("rca_reservatorio", "rca_cota", "rca_volume", "rca_area")]
  colnames(mat_CAV) <- c("ID", "COTA", "VOLUME_M3", "AREA_KM2")
  
  return(mat_CAV)
}



#' Leitura de cota do vertedouro a Partir do Banco de Dados
#'
#' Realiza a leitura dos dados de cota do vertedouro para um conjunto de reservatórios a partir de um banco de dados PostgreSQL.
#'
#' @param id_res Vetor. IDs dos reservatórios cujos dados de cota do vertedouro serão lidos.
#' @param dbname Nome do banco de dados. Padrão: "func".
#' @param host Host do banco de dados. Padrão: "flora.funceme.br".
#' @param port Porta do banco de dados. Padrão: 5432.
#' @param user Usuário para conexão. Padrão: "consulta".
#'
#' @return Data frame. Matriz incluindo colunas de ID e cota do vertedouro.
#' @export
#'
#' @importFrom RPostgreSQL dbConnect dbGetQuery dbDisconnect
FUNC_ler_cotaver <- function(id_res,
                             dbname = "func",
                             host = "flora.funceme.br",
                             port = 5432,
                             user = "alyson_estacio") {
  
  connection <- dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = dbname,
                          host = host,
                          port = port,
                          user = user,
                          password = "n6cj3zy4xB3W5gP")
  
  Cota_ver <- dbGetQuery(connection, paste0("SELECT res_cod, res_cota_sangria FROM acude.reservatorio",
                                            " ORDER BY res_cod "))
  
  on.exit(dbDisconnect(connection))
  
  colnames(Cota_ver) <- c("ID", "COTA_VERT_M")
  Cota_ver <- Cota_ver[is.element(Cota_ver$ID, id_res),]
  
  return(Cota_ver)
}


#' Organizar Série Temporal em Tabela Mensal
#'
#' Reorganiza uma série temporal em formato de tabela com colunas representando meses.
#'
#' @param serie Vetor da série temporal.
#' @param datas Vetor de datas associado à série.
#' @return Data frame com colunas representando meses e linhas representando anos.
#' @export
Org_serie_tab <- function(serie, datas){

  anos <- unique(year(datas))
  Mat <- matrix(serie, nrow=length(anos),ncol=12, byrow = T)

  rownames(Mat)=anos
  colnames(Mat)=c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                           "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")

  Mat <- as.data.frame(Mat)
  return(Mat)
}






#' Leitura de capacidade de reservatórios a partir do banco de dados
#'
#' Lê `res_capacidade` de `acude.reservatorio`, no mesmo padrão de
#' `FUNC_ler_cotaver`.
#'
#' @param id_res Vetor de IDs dos reservatórios.
#' @param dbname,host,port,user Parâmetros da conexão PostgreSQL.
#' @return Data frame com as colunas `ID` e `CAPACIDADE_HM3`.
#' @export
FUNC_ler_capacidade_banco <- function(id_res,
                                      dbname = "func",
                                      host = "flora.funceme.br",
                                      port = 5432,
                                      user = "alyson_estacio") {
  connection <- dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = dbname, host = host, port = port,
                          user = user,
                          password = "n6cj3zy4xB3W5gP")
  
  on.exit(dbDisconnect(connection))
  capacidade <- dbGetQuery(connection, paste0(
    "SELECT res_cod, res_capacidade FROM acude.reservatorio ORDER BY res_cod"
  ))
  colnames(capacidade) <- c("ID", "CAPACIDADE_HM3")
  capacidade <- capacidade[is.element(capacidade$ID, id_res), ]
  capacidade$ID <- as.numeric(capacidade$ID)
  capacidade$CAPACIDADE_HM3 <- as.numeric(capacidade$CAPACIDADE_HM3)
  return(capacidade)
}


#' Verifica a cota do vertedouro contra o range de cotas das CAVs
#'
#' Mantém a verificação de monotonicidade e a comparação volumétrica por
#' integração trapezoidal, mas o status principal compara COTA_VERT_M com o
#' intervalo de COTA da CAV.
#'
#' @param cotas_vert Data frame com ID e COTA_VERT_M, como FUNC_ler_cotaver.
#' @param CAVs Data frame com ID, COTA, AREA_KM2 e VOLUME_M3.
#' @param arquivo_pdf Caminho do PDF de saída.
#' @param dadosRes Opcional data frame com ID e ACUDE para os títulos.
#' @return Data frame com os limites de cota, status e erros volumétricos.
#' @export
FUNC_verif_cotas_vert_CAVs <- function(cotas_vert, CAVs,
                                       arquivo_pdf = "Relatorio_CAVs.pdf",
                                       dadosRes = NULL) {
  if (!all(c("ID", "COTA_VERT_M") %in% names(cotas_vert))) {
    stop("'cotas_vert' deve conter ID e COTA_VERT_M, no formato de FUNC_ler_cotaver.")
  }
  if (!all(c("ID", "COTA", "AREA_KM2", "VOLUME_M3") %in% names(CAVs))) {
    stop("'CAVs' deve conter ID, COTA, AREA_KM2 e VOLUME_M3.")
  }
  cotas_vert <- cotas_vert[, c("ID", "COTA_VERT_M")]
  cotas_vert[] <- lapply(cotas_vert, as.numeric)
  CAVs <- CAVs[, c("ID", "COTA", "AREA_KM2", "VOLUME_M3")]
  CAVs[] <- lapply(CAVs, as.numeric)
  if (anyDuplicated(cotas_vert$ID)) stop("Há mais de uma cota do vertedouro para o mesmo ID.")

  resultado <- do.call(rbind, lapply(cotas_vert$ID, function(id) {
    cota_vert <- cotas_vert$COTA_VERT_M[cotas_vert$ID == id]
    cav <- CAVs[CAVs$ID == id, , drop = FALSE]
    if (nrow(cav) == 0) {
      return(data.frame(ID = id, COTA_VERT_M = cota_vert,
                        COTA_MIN_M = NA_real_, COTA_MAX_M = NA_real_,
                        AREA_ESTRITAMENTE_CRESCENTE = NA,
                        VOLUME_ESTRITAMENTE_CRESCENTE = NA,
                        ERRO_MAX_ABS_M3 = NA_real_, ERRO_MAX_REL_PCT = NA_real_,
                        STATUS = "SEM_CAV"))
    }
    cav <- cav[order(cav$COTA), ]
    area_crescente <- all(diff(cav$AREA_KM2) > 0)
    volume_crescente <- all(diff(cav$VOLUME_M3) > 0)
    dh <- diff(cav$COTA)
    area_media_m2 <- (cav$AREA_KM2[-nrow(cav)] + cav$AREA_KM2[-1]) * 1e6 / 2
    volume_estimado <- c(0, cumsum(area_media_m2 * dh))
    diferenca <- volume_estimado - cav$VOLUME_M3
    erro_abs <- max(abs(diferenca), na.rm = TRUE)
    escala <- max(abs(cav$VOLUME_M3), na.rm = TRUE)
    erro_rel <- if (is.finite(escala) && escala > 0) 100 * erro_abs / escala else NA_real_
    cota_min <- min(cav$COTA, na.rm = TRUE)
    cota_max <- max(cav$COTA, na.rm = TRUE)
    status <- if (is.na(cota_vert)) "SEM_COTA_VERTEDOURO" else if (cota_vert < cota_min) {
      "ABAIXO_DO_RANGE"
    } else if (cota_vert > cota_max) {
      "ACIMA_DO_RANGE"
    } else "DENTRO_DO_RANGE"
    data.frame(ID = id, COTA_VERT_M = cota_vert, COTA_MIN_M = cota_min,
               COTA_MAX_M = cota_max, AREA_ESTRITAMENTE_CRESCENTE = area_crescente,
               VOLUME_ESTRITAMENTE_CRESCENTE = volume_crescente,
               ERRO_MAX_ABS_M3 = erro_abs, ERRO_MAX_REL_PCT = erro_rel, STATUS = status)
  }))
  resultado$ACUDE <- NA_character_
  if (!is.null(dadosRes) && all(c("ID", "ACUDE") %in% names(dadosRes))) {
    resultado$ACUDE <- dadosRes$ACUDE[match(resultado$ID, dadosRes$ID)]
  }
  resultado <- resultado[order(resultado$ID), ]

  itens <- function(x) {
    if (nrow(x) == 0) return("Nenhum")
    nomes <- ifelse(is.na(x$ACUDE), paste0("ID ", x$ID), paste0(x$ACUDE, " (ID ", x$ID, ")"))
    strwrap(paste(nomes, collapse = "; "), width = 110)
  }
  fora <- resultado[resultado$STATUS %in% c("ABAIXO_DO_RANGE", "ACIMA_DO_RANGE"), ]
  nao_crescentes <- resultado[!is.na(resultado$AREA_ESTRITAMENTE_CRESCENTE) &
                                (!resultado$AREA_ESTRITAMENTE_CRESCENTE | !resultado$VOLUME_ESTRITAMENTE_CRESCENTE), ]

  grDevices::pdf(arquivo_pdf, width = 11, height = 8.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  graphics::title(main = "Verificação da cota do vertedouro contra as CAVs")
  resumo <- c(
    paste("Reservatórios avaliados:", nrow(resultado)),
    paste("Cota do vertedouro fora do range:", nrow(fora)),
    paste("CAV com área ou volume não estritamente crescente:", nrow(nao_crescentes)),
    paste("Maior erro relativo volume CAV x trapézios:", sprintf("%.2f%%", max(resultado$ERRO_MAX_REL_PCT, na.rm = TRUE))),
    "Volume estimado = soma de ((Área_i + Área_i+1)/2) × (Cota_i+1 - Cota_i)."
  )
  graphics::text(0.04, seq(0.92, 0.72, length.out = length(resumo)), resumo, adj = c(0, 0.5), cex = 0.88)
  graphics::text(0.04, 0.65, "Reservatórios com cota do vertedouro fora do range:", adj = c(0, 0.5), font = 2, cex = 0.85)
  graphics::text(0.04, 0.61, paste(itens(fora), collapse = "\n"), adj = c(0, 1), cex = 0.72)
  graphics::text(0.04, 0.36, "Reservatórios com área ou volume não estritamente crescente:", adj = c(0, 0.5), font = 2, cex = 0.85)
  graphics::text(0.04, 0.32, paste(itens(nao_crescentes), collapse = "\n"), adj = c(0, 1), cex = 0.72)
  graphics::text(0.04, 0.03, "O status e os erros de cada reservatório constam na página individual.", adj = c(0, 0), cex = 0.72, col = "grey35")

  for (i in seq_len(nrow(resultado))) {
    id <- resultado$ID[i]
    cav <- CAVs[CAVs$ID == id, , drop = FALSE]
    nome <- ifelse(is.na(resultado$ACUDE[i]), "Reservatório", resultado$ACUDE[i])
    titulo <- paste0(nome, " (ID ", id, ") — ", resultado$STATUS[i])
    graphics::par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
    if (nrow(cav) == 0) {
      for (j in 1:3) { graphics::plot.new(); graphics::title(main = titulo); graphics::text(0.5, 0.5, "CAV não encontrada") }
      next
    }
    cav <- cav[order(cav$COTA), ]
    dh <- diff(cav$COTA)
    area_media_m2 <- (cav$AREA_KM2[-nrow(cav)] + cav$AREA_KM2[-1]) * 1e6 / 2
    volume_estimado <- c(0, cumsum(area_media_m2 * dh))
    subtxt <- paste0("Área crescente: ", resultado$AREA_ESTRITAMENTE_CRESCENTE[i],
                     " | Volume crescente: ", resultado$VOLUME_ESTRITAMENTE_CRESCENTE[i],
                     " | Erro máx.: ", sprintf("%.2f%%", resultado$ERRO_MAX_REL_PCT[i]))
    graphics::plot(cav$COTA, cav$AREA_KM2, type = "b", pch = 19, col = "#0072B2", xlab = "Cota (m)", ylab = expression("Área (km"^2*")"), main = paste(titulo, "\nCota x Área"), sub = subtxt)
    graphics::grid(col = "grey85")
    graphics::plot(cav$COTA, cav$VOLUME_M3, type = "b", pch = 19, col = "#0072B2", xlab = "Cota (m)", ylab = expression("Volume (m"^3*")"), main = paste(titulo, "\nCota x Volume"))
    if (is.finite(resultado$COTA_VERT_M[i])) {
      graphics::abline(v = resultado$COTA_VERT_M[i], col = "#D55E00", lwd = 2, lty = 2)
      graphics::legend("topleft", legend = "Cota do vertedouro", col = "#D55E00", lty = 2, lwd = 2, bty = "n")
    }
    graphics::grid(col = "grey85")
    limite <- max(c(cav$VOLUME_M3, volume_estimado), na.rm = TRUE) / 1e6
    if (!is.finite(limite) || limite == 0) limite <- 1
    graphics::plot(cav$VOLUME_M3 / 1e6, volume_estimado / 1e6, pch = 19, col = "#009E73", xlim = c(0, limite), ylim = c(0, limite), xlab = "Volume na CAV (hm³)", ylab = "Volume estimado por trapézios (hm³)", main = paste(titulo, "\nVolume CAV x estimado"))
    graphics::abline(a = 0, b = 1, lty = 2, lwd = 2, col = "grey40")
    graphics::grid(col = "grey85")
  }
  return(resultado)
}
