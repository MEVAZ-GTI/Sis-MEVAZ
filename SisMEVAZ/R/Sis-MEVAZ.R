

#' Simulação de Rede de reservatórios em teste
#'
#' Permite replicar a MEVAZ (Metodologia de Cálculo de Vazões)
#' incuindo um ou mais reservatórios à rede original.
#' Os dados dos novos reservatórios e daqueles reservatórios cujos dados forem modificados
#' por essa inclusão (bacia incremental ou ordem de afluência) devem ser inseridos no
#' diretório "diretorio_de_dados/RededeReservatorios_em_teste/input "
#'
#' @param diretorio_de_dados Caractere com o caminho do pacote
#'
#' @return  Os resultados da inclusão do(s) novo(s) reservatório(s) são salvos no diretório
#' "diretorio_de_dados/RededeReservatorios_em_teste/output". Para gerar os arquivos xlsx de
#' saída veja SisMEVAZ_print_Sintese e SisMEVAZ_print_Series
#'
#' @seealso \code{\link{SisMEVAZ_print_Sintese}}
#' @seealso \code{\link{SisMEVAZ_print_Series}}
#' @import openxlsx
#' @export
#'
#'
SisMEVAZ_redeTeste <- function(diretorio_de_dados=paste0(getwd(),"/../../dados")){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_Plu_ETP    <- paste0(diretorio_de_dados,"/Plu_ETP")

  ## LEITURA DE DADOS DE INPUT PARA A REDE-TESTE
  # Caminhos dos arquivos de shape das bacias adicionadas/modificadas na rede-teste
  # OBS: devem constar não apenas os novos resevtórios, mas também aqueles imediatamente a jusante destes
  # pois estes também têm suas bacias incrementais modificadas com a inclusão dos novos reservatórios
  arqBac <- list.files(path=paste0(dir_rede_teste,"/input/shapes"),pattern = ".shp",full.names = T)
  idsBac <- gsub(pattern = ".shp",replacement = "",x = basename(arqBac))
  idsBac <- gsub(pattern = "graus_id_sagreh_",replacement = "", x = idsBac)

  # Dados base e CAVs dos novos resevatórios testados
  # OBS: em dadosRes_tmp devem constar não apenas os novos resevtórios, mas também aqueles imediatamente a montante destes
  # pois sua ordem de afluência deve ser modificada na coluna ID_ACJUS.
  # Em CAVs_tmp podem ser mantidos apenas os novos reservatórios testados
  dadosRes_tmp <- read.xlsx(paste0(dir_rede_teste,"/input/Dados_Novos_Reservatorios.xlsx"),colNames = F,rowNames = T)
  dadosRes_tmp <- as.data.frame(t(dadosRes_tmp))
  dadosRes_tmp[, "COTA_VERT_M"]         <- as.numeric(dadosRes_tmp[, "COTA_VERT_M"] )
  dadosRes_tmp[, "LONG"]             <- as.numeric(dadosRes_tmp[, "LONG"] )
  dadosRes_tmp[, "LAT"]              <- as.numeric(dadosRes_tmp[, "LAT"] )
  dadosRes_tmp[, "AREA_BAC_TOT_KM2"] <- as.numeric(dadosRes_tmp[, "AREA_BAC_TOT_KM2"] )
  dadosRes_tmp[, "AREA_BAC_INC_KM2"] <- as.numeric(dadosRes_tmp[, "AREA_BAC_INC_KM2"] )
  dadosRes_tmp[, "PRINT_FINAL"]      <- 1
  CAVs_tmp     <- read.xlsx(paste0(dir_rede_teste,"/input/CAVs_Novos_Reservatorios.xlsx"),colNames = T)


  ## LEITURA DE DADOS FIXOS (independem da rede de reservatórios)
  # Caminhos dos arquivos de características fisiográficas
  arqRio  <- paste0(dir_dado_fixo,"/Carac_Fisiograficas/Drenagem/Rios_mSimpl_1_100000_SRH.shp") # Carta de rios 1:100.000 (comparação visual com a da SUDENE 1:100.000). Não foi usada a da SUDENE pois possui margens duplas, lagos, etc.
  arqCris <- paste0(dir_dado_fixo,"/Carac_Fisiograficas/Cristalino/Cristalino.shp")
  arqSolo <- paste0(dir_dado_fixo,"/Carac_Fisiograficas/Capacidade_Armazenamento_Solo/solos_1972.shp")
  arqDecl <- paste0(dir_dado_fixo,"/Carac_Fisiograficas/Declividade/slop.tif")
  arqCN   <- paste0(dir_dado_fixo,"/Carac_Fisiograficas/CurveNumber/CNII_CE.tif")

  # Dados de ETP (estações do INMET selecionadas)
  ETP_MedMen_PM <- readRDS(paste0(dir_Plu_ETP, "/ETP_INMETselec_MensalMed_PenmanMonteith.rds"))
  MatETP        <- ETP_MedMen_PM$ETP
  pontos_ETP    <- ETP_MedMen_PM$metadados[,c("Long","Lat","Codigo")]
  names(pontos_ETP) <- c("x","y","id")

  # Evaporação de referência
  Evapm_ref <- read.xlsx(paste0(dir_Plu_ETP,"/EVAPORACAO_PICHE.xlsx"),colNames = T)

  # Dados de precipitação em grade para o Ceará
  Prec_gradeCE <- readRDS(paste0(dir_Plu_ETP, "/PLU_IDW_Mensal_Ceara_0.01graus.rds"))
  grade_Prec   <- data.frame(x=Prec_gradeCE$pontos$Long, y=Prec_gradeCE$pontos$Lat, id=1:nrow(Prec_gradeCE$pontos))
  Datas        <- names(Prec_gradeCE$P)

  # Características fisiográficas das estações fluviométricas e reta de regressão
  CaracEst <- readRDS(paste0(dir_dado_fixo,"/Param_Reg_KNN/MatrizCarac_EstFlu.rds"))
  Reg_lm   <- readRDS(paste0(dir_dado_fixo,"/Param_Reg_KNN/LMRegionalizacaoVarReduzidas.rds"))

  # Parâmetros do SMAP calibrados para as estações fluviomátricas
  ParamEstFlu <- readRDS(paste0(dir_dado_fixo,"/Param_Reg_KNN/ParamCalibDREAM_medianos.rds"))

  # Dados dos reservatórios com regionalização do tipo Médias Regionais
  datMedReg <- read.xlsx(paste0(dir_dado_fixo,"/CE_CV_MedReg.xlsx"),colNames = T)


  ## LEITURA DE DADOS DA REDE CONSOLIDADA (a serem atualizados na rede-teste)
  Carac_consol <- readRDS(paste0(dir_rede_conso,"/input/MatrizCarac_BacAcu.rds"))
  ETP_consol   <- readRDS(paste0(dir_rede_conso,"/input/ETP_Mensal_PM_Bacias_Incrementais.rds"))
  Pre_consol   <- readRDS(paste0(dir_rede_conso,"/input/Precipitacao_Mensal_Bacias_Incrementais.rds"))

  AfluInc_KNN_consol <- readRDS(paste0(dir_rede_conso,"/input/AfluenciasIncrementais_KNN.rds"))
  AfluInc_ML_consol  <- readRDS(paste0(dir_rede_conso,"/input/AfluenciasIncrementais_ML1.rds"))
  Aflu_CalLocal_consol <- readRDS(paste0(dir_rede_conso,"/input/Afluencias_CalLocal.rds"))

  dadosRes_consol <- read.xlsx(paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"),colNames = T)
  CAVs_consol     <- read.xlsx(paste0(dir_rede_conso,"/input/CAVs.xlsx"),colNames = T)

  ResSimul_KNN_consol    <- readRDS(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_KNN.rds"))
  ResSimul_ML_consol     <- readRDS(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_ML.rds"))
  ResSimul_Selec_consol  <- readRDS(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))


  ## PROCESSAMENTOS DE DADOS NAS NOVAS BACIAS
  # Calcula características FISIOGRÁFICAS, ETP média mensal e Precipitação nas novas bacias
  Carac_tmp <-FUNC_CararacFisiogr_calc(idsBac, arqBac, arqRio, arqCris, arqSolo, arqDecl, arqCN)
  ETP_tmp <- FUNC_ETPmed_thiessen(MatETP, pontos = pontos_ETP, arqBac, idsBac)
  Pre_tmp <- FUNC_Precmed_internos(listPrec = Prec_gradeCE$P, pontos = grade_Prec, arqBac, idsBac)

  # Atualiza Características, ETP e Precipitações da rede consolidada para a rede-teste
  Carac_teste <- FUNC_atualiz_mat(mat_tmp = Carac_tmp, mat_consol = Carac_consol,
                                  res_tmp = rownames(Carac_tmp), res_consol = row.names(Carac_consol))
  ETP_teste   <- FUNC_atualiz_lista_vec(List_tmp = ETP_tmp, List_consol = ETP_consol)
  Pre_teste   <- FUNC_atualiz_lista_vec(List_tmp = Pre_tmp, List_consol = Pre_consol)

  ## REGIONALIZAÇÃO DOS PARÂMETROS NAS NOVAS BACIAS
  # Identifica vizinho mais próximo
  carac_compara <- c("Cristalino","Kc","DD_km_km2","CAD_mm","Declividade_perc","CN_mm")
  est_prox_ac   <- FUNC_estproxKNN(CaracEst = CaracEst, CaracAcu = Carac_teste,
                                carac_compara, pesos_carac = Reg_lm$coefficients,n_viz = 1)
  est_prox_ac   <- unlist(est_prox_ac)

  # Precipitação média (usada na regionalização com o ML)
  Prec_anual <- Agg_anual(Lista_series = Pre_teste,func_agg = sum)
  Prec_med   <- sapply(Prec_anual,mean,na.rm=T)

  # Parâmetros obtidos via regionalização
  ParamKNN <- FUNC_paramKNN(est_prox = est_prox_ac, ParamEstFlu = ParamEstFlu)
  ParamML  <- FUNC_paramML(Carac = Carac_teste, Prec_med)

  ## SIMULAÇÃO HIDROLÓGICA
  # Cálculo de afluências incrementais apenas para as bacias novas (idsBac)
  vecArea     <- Carac_teste[,"Area_km2"]
  AfluInc_KNN_tmp <- SMAP.listaBac(matParam = ParamKNN, vecArea = vecArea,
                        lPrec = Pre_teste, lETo_med = ETP_teste,
                        ids = idsBac)

  AfluInc_ML_tmp  <- SMAP.listaBac(matParam = ParamML, vecArea = vecArea,
                        lPrec = Pre_teste, lETo_med = ETP_teste,
                        ids = idsBac)

  # Atualiza afluências incrementais da rede consolidada para a rede-teste
  AfluInc_KNN_teste  <- FUNC_atualiz_lista_vec(List_tmp = AfluInc_KNN_tmp, List_consol = AfluInc_KNN_consol)
  AfluInc_ML_teste   <- FUNC_atualiz_lista_vec(List_tmp = AfluInc_ML_tmp, List_consol = AfluInc_ML_consol)

  # Atualiza dados base dos reservatórios e CAVs
  dadosRes_teste <- FUNC_atualiz_mat(mat_tmp = dadosRes_tmp, mat_consol = dadosRes_consol,
                      res_tmp = dadosRes_tmp[,"ID"], res_consol = dadosRes_consol[,"ID"])
  CAVs_teste     <- FUNC_atualiz_mat(mat_tmp = CAVs_tmp, mat_consol = CAVs_consol,
                                     res_tmp = CAVs_tmp[,"ID"], res_consol = CAVs_consol[,"ID"])

  # Verifica consistência dos reservatórios com regionalização simplificada (Modelos Regionais)
  ModRegion          <- dadosRes_teste$MODELO_REGIONALIZACAO # já atualizado
  names(ModRegion)   <- dadosRes_teste$ID
  RegHidro           <- dadosRes_teste$REG_HIDRO # já atualizado
  names(RegHidro)    <- dadosRes_teste$ID
  ids_MR <- names(ModRegion)[ModRegion=="Médias Regionais"]
  verif  <- all(sapply(ids_MR, function(x){is.element(x,datMedReg$ID)}))
  if(!verif){stop("Reservatório com regionalização do tipo Médias Regionais não está presente no arquivo CE_CV_MedReg.xlsx")}


  # Afluências incrmentais (seleção do modelo de regionalização)
  #!!!Incluir aqui a opção AfluInc_Local
  AfluInc_Selec_teste <- FUNC_SelecRegion(AfluInc_KNN = AfluInc_KNN_teste,
                                          AfluInc_ML = AfluInc_ML_teste,
                                          Aflu_CalLocal = Aflu_CalLocal_consol,
                                          ModRegion, RegHidro, Prec_med,
                                          Areas=vecArea, datMedReg)

  # ordem de afluencia entre reservatórios da rede-teste
  Ord_aflu <- dadosRes_teste[,c("ID","ID_ACJUS")]

  # Seleciona reservatórios finais das cascatas a serem simuladas
  resFimRede_selec <- FUNC_resFimRede_selec(Ord_aflu, res_mod=idsBac) # apenas os finais
  resRede_selec    <- FUNC_resRede_selec(Ord_aflu, resFimRede_selec)  # todos na rede

  # Serie de Evaporação nos reservatórios
  ResEst_selec <- dadosRes_teste[which(is.element(dadosRes_teste$ID, resRede_selec)),
                                 c("ID","COD_EST_EVAP")]
  EvapSerie <- MedMensalEst_para_SerieRes(Mat_MedMensal = Evapm_ref,
                                          ResEst = ResEst_selec,
                                          Datas = Datas)

  # Inicialização dos volumes nos reservatórios
  vecVoliniperc        <- rep(0.5,length(resRede_selec))
  names(vecVoliniperc) <- resRede_selec


  # Simula as redes de reservatórios às quais pertencem os reservatórios novos na rede-teste
  ResSimul_KNN_tmp  <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                 vecGarantia = c(0.90, 0.95, 0.98),
                                                 AfluInc = AfluInc_KNN_teste,
                                                 EvapSerie, Ord_aflu, DadosRes=dadosRes_teste,
                                                 DadosCAV=CAVs_teste, vecVoliniperc)

  ResSimul_ML_tmp   <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                 vecGarantia = c(0.90, 0.95, 0.98),
                                                 AfluInc = AfluInc_ML_teste,
                                                 EvapSerie, Ord_aflu, DadosRes=dadosRes_teste,
                                                 DadosCAV=CAVs_teste, vecVoliniperc)

  ResSimul_Selec_tmp <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                 vecGarantia = c(0.90, 0.95, 0.98),
                                                 AfluInc = AfluInc_Selec_teste,
                                                 EvapSerie, Ord_aflu, DadosRes=dadosRes_teste,
                                                 DadosCAV=CAVs_teste, vecVoliniperc)

  # Atualiza resultados das simulações da rede consolidada para a rede-teste
  Qgaran_nms <- c("Q90","Q95","Q98")
  ResSimul_KNN_teste <- lapply(Qgaran_nms, function(qq){
    FUNC_atualiz_lista_vec(List_tmp = ResSimul_KNN_tmp[[qq]], List_consol = ResSimul_KNN_consol[[qq]])
    })

  ResSimul_ML_teste <- lapply(Qgaran_nms, function(qq){
    FUNC_atualiz_lista_vec(List_tmp = ResSimul_ML_tmp[[qq]], List_consol = ResSimul_ML_consol[[qq]])
  })

  ResSimul_Selec_teste <- lapply(Qgaran_nms, function(qq){
    FUNC_atualiz_lista_vec(List_tmp = ResSimul_Selec_tmp[[qq]], List_consol = ResSimul_Selec_consol[[qq]])
  })
  names(ResSimul_KNN_teste)   <- Qgaran_nms
  names(ResSimul_ML_teste)    <- Qgaran_nms
  names(ResSimul_Selec_teste) <- Qgaran_nms

  # Extrai vazão regulariza a partir da simlação (máxima valor operado na simulação)
  Qgaran_KNN   <- sapply(ResSimul_KNN_teste, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_ML    <- sapply(ResSimul_ML_teste, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_Selec <- sapply(ResSimul_Selec_teste, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})


  ## SALVA ARQUIVOS ATUALIZADOS
  if(!dir.exists(paste0(dir_rede_teste, "/output"))){
    dir.create(paste0(dir_rede_teste, "/output"),showWarnings = F)
  }
  saveRDS(Carac_teste,paste0(dir_rede_teste,"/output/MatrizCarac_BacAcu.rds"))
  saveRDS(ETP_teste,paste0(dir_rede_teste,"/output/ETP_Mensal_PM_Bacias_Incrementais.rds"))
  saveRDS(Pre_teste,paste0(dir_rede_teste,"/output/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  saveRDS(est_prox_ac,paste0(dir_rede_teste,"/output/KNNest_prox.rds"))
  saveRDS(ParamKNN,paste0(dir_rede_teste,"/output/Param_KNN.rds"))
  saveRDS(ParamML,paste0(dir_rede_teste,"/output/Param_ML1.rds"))
  saveRDS(AfluInc_KNN_teste,paste0(dir_rede_teste,"/output/AfluenciasIncrementais_KNN.rds"))
  saveRDS(AfluInc_ML_teste ,paste0(dir_rede_teste,"/output/AfluenciasIncrementais_ML1.rds"))
  saveRDS(Aflu_CalLocal_consol ,paste0(dir_rede_teste,"/output/Afluencias_CalLocal.rds"))
  saveRDS(AfluInc_Selec_teste ,paste0(dir_rede_teste,"/output/AfluenciasIncrementais_Selec.rds"))
  write.xlsx(dadosRes_teste,paste0(dir_rede_teste,"/output/Dados_Reservatorios.xlsx"))
  write.xlsx(CAVs_teste,paste0(dir_rede_teste,"/output/CAVs.xlsx"))
  saveRDS(ResSimul_KNN_teste,paste0(dir_rede_teste,"/output/Resultados_SimulacaoRede_KNN.rds"))
  saveRDS(ResSimul_ML_teste ,paste0(dir_rede_teste,"/output/Resultados_SimulacaoRede_ML.rds"))
  saveRDS(ResSimul_Selec_teste ,paste0(dir_rede_teste,"/output/Resultados_SimulacaoRede_Selec.rds"))
  saveRDS(Qgaran_KNN,paste0(dir_rede_teste,"/output/Qgaran_KNN.rds"))
  saveRDS(Qgaran_ML ,paste0(dir_rede_teste,"/output/Qgaran_ML.rds"))
  saveRDS(Qgaran_Selec ,paste0(dir_rede_teste,"/output/Qgaran_Selec.rds"))

  return("Simlação da rede teste finalizada")
}

#' Escrever arquivos com séries de vazão
#'
#' Função escreve arquivos .xlsx com séries de vazão afluente (incremental e total)
#' para cada reservatório, a partir dos resultados simulados.
#' Pode ser aplicada para a rede de reservatórios em teste ou para a
#' rede de reservatórios consolidada.
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param versao_print Caractere com a versão de referência ("redeConsol", "redeTeste", "Versao_i" ou "Teste_i" com i inteiro>=0)

#' @seealso \code{\link{SisMEVAZ_print_Series}}
#' @return "Arquivo Síntese.xlsx escrito"
#' @import openxlsx
#'
#' @export
#'
#'
SisMEVAZ_print_Sintese <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"),versao_print="redeTeste"){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")

  if(versao_print=="redeConsol"){
    dir_rede_print <- dir_rede_conso
  }else if(versao_print=="redeTeste"){
    dir_rede_print <- dir_rede_teste
  }else if(grepl("Versao_", versao_print)){
    vv <-  gsub(pattern = "Versao_", replacement = "", x = versao_print)
    dir_rede_print <- paste0(dir_rede_conso,"/versoes_anteriores/versao_",vv)
  }else{
    tt <-  gsub(pattern = "Teste_", replacement = "", x = versao_print)
    dir_rede_print <- paste0(diretorio_de_dados,"/Testes_outros/teste_",tt)
  }

  ## LEITURA DE DADOS
  dadosRes     <- read.xlsx(paste0(dir_rede_print,"/output/Dados_Reservatorios.xlsx"))
  dadosRes     <- dadosRes[dadosRes$PRINT_FINAL==1,] # filtra reservatórios
  CAVs         <- read.xlsx(paste0(dir_rede_print,"/output/CAVs.xlsx"))
  AreaBaciaInc <- as.numeric(dadosRes$AREA_BAC_INC_KM2)
  Cota_ver     <- as.numeric(dadosRes$COTA_VERT_M)

  ParamKNN       <- readRDS(paste0(dir_rede_print,"/output/Param_KNN.rds"))
  ParamML        <- readRDS(paste0(dir_rede_print,"/output/Param_ML1.rds"))
  Param_CalLocal <- as.matrix(read.xlsx(paste0(dir_dado_fixo,"/Calibracao_com_balanco_reverso.xlsx"),colNames = T,rowNames = T)[,-1])

  PrecBaciaInc   <- readRDS(paste0(dir_rede_print,"/output/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  AfluInc_KNN    <- readRDS(paste0(dir_rede_print,"/output/AfluenciasIncrementais_KNN.rds"))
  AfluInc_ML     <- readRDS(paste0(dir_rede_print,"/output/AfluenciasIncrementais_ML1.rds"))
  AfluInc_Selec  <- readRDS(paste0(dir_rede_print,"/output/AfluenciasIncrementais_Selec.rds"))
  est_prox_ac    <- readRDS(paste0(dir_rede_print,"/output/KNNest_prox.rds"))
  est_nomes      <- readRDS(paste0(dir_dado_fixo,"/EstFluNomes.rds"))
  ResSimul_KNN   <- readRDS(paste0(dir_rede_print,"/output/Resultados_SimulacaoRede_KNN.rds"))
  ResSimul_ML    <- readRDS(paste0(dir_rede_print,"/output/Resultados_SimulacaoRede_ML.rds"))
  ResSimul_Selec <- readRDS(paste0(dir_rede_print,"/output/Resultados_SimulacaoRede_Selec.rds"))
  Qgaran_KNN     <- readRDS(paste0(dir_rede_print,"/output/Qgaran_KNN.rds"))
  Qgaran_ML      <- readRDS(paste0(dir_rede_print,"/output/Qgaran_ML.rds"))
  Qgaran_Selec   <- readRDS(paste0(dir_rede_print,"/output/Qgaran_Selec.rds"))
  LAfluTot_KNN   <- Get_AfluTot_fromListSimul(ListSimul = ResSimul_KNN)
  LAfluTot_ML    <- Get_AfluTot_fromListSimul(ListSimul = ResSimul_ML)
  LAfluTot_Selec <- Get_AfluTot_fromListSimul(ListSimul = ResSimul_Selec)

  # Filtra resservatórios
  ParamKNN       <- ParamKNN[as.character(dadosRes$ID),]
  ParamML        <- ParamML[as.character(dadosRes$ID),]
  Param_CalLocal <- Param_CalLocal[is.element(rownames(Param_CalLocal),as.character(dadosRes$ID)),]
  PrecBaciaInc   <- PrecBaciaInc[as.character(dadosRes$ID)]
  AfluInc_KNN    <- AfluInc_KNN[as.character(dadosRes$ID)]
  AfluInc_ML     <- AfluInc_ML[as.character(dadosRes$ID)]
  AfluInc_Selec  <- AfluInc_Selec[as.character(dadosRes$ID)]
  est_prox_ac    <- est_prox_ac[as.character(dadosRes$ID)]
  Qgaran_KNN     <- Qgaran_KNN[as.character(dadosRes$ID),]
  Qgaran_ML      <- Qgaran_ML[as.character(dadosRes$ID),]
  Qgaran_Selec   <- Qgaran_Selec[as.character(dadosRes$ID),]
  ResSimul_KNN   <- lapply(ResSimul_KNN,function(lis){lis[as.character(dadosRes$ID)]})
  ResSimul_ML    <- lapply(ResSimul_ML,function(lis){lis[as.character(dadosRes$ID)]})
  ResSimul_Selec <- lapply(ResSimul_Selec,function(lis){lis[as.character(dadosRes$ID)]})
  LAfluTot_KNN   <- lapply(LAfluTot_KNN,function(lis){lis[as.character(dadosRes$ID)]})
  LAfluTot_ML    <- lapply(LAfluTot_ML,function(lis){lis[as.character(dadosRes$ID)]})
  LAfluTot_Selec <- lapply(LAfluTot_Selec,function(lis){lis[as.character(dadosRes$ID)]})

  # Lê arquivo com formatação
  arq_format <- openxlsx::loadWorkbook(paste0(dir_dado_fixo,"/Arquivo_Sintese_Formatacao.xlsx"))

  # Calcula capacidades
   Cap_hm3 <- sapply(dadosRes$ID, function(id){
    Cota   <- CAVs$COTA[CAVs$ID==id]# em m
    Area   <- CAVs$AREA_KM2[CAVs$ID==id]# em Km²
    Volume <- CAVs$VOLUME_M3[CAVs$ID==id]/10^6 #de m³ para hm³
    cav    <- data.frame(Cota,Area,Volume)

    cota_vert      <- dadosRes$COTA_VERT_M[dadosRes$ID==id]
    Cap <- Interpola(vecIn = cav$Cota, vecOut = cav$Volume, input = cota_vert) # em hm³
    return(Cap)
  })

  # Agrega séries de precipitação e vazão para a escala anual
  PrecBaciaInc_anual  <- Agg_anual(PrecBaciaInc,func_agg = sum)
  AfluInc_KNN_anual   <- Agg_anual(AfluInc_KNN)
  AfluInc_ML_anual    <- Agg_anual(AfluInc_ML)
  AfluInc_Selec_anual <- Agg_anual(AfluInc_Selec)

  LAfluTot_KNN_anual   <- lapply(LAfluTot_KNN,Agg_anual)
  LAfluTot_ML_anual    <- lapply(LAfluTot_ML,Agg_anual)
  LAfluTot_Selec_anual <- lapply(LAfluTot_Selec,Agg_anual)


  # DADOS GERAIS
  DadosGerais <- data.frame(ID=dadosRes$ID,
                    Reservatorio=dadosRes$ACUDE,
                    Regiao_Hidrografica=dadosRes$REG_HIDRO,
                    Rio_Barrado=dadosRes$RIO_BARRADO,
                    Coordenadas_aprox_Long=as.numeric(dadosRes$LONG),
                    Coordenadas_aprox_Lat=as.numeric(dadosRes$LAT),
                    Capacidade_hm3=Cap_hm3,
                    Bacia_Hidrografica_km2=as.numeric(dadosRes$AREA_BAC_TOT_KM2),
                    Area_nao_Controlada_km2=as.numeric(dadosRes$AREA_BAC_INC_KM2))


  # DADOS REFERENTES A BACIA INCREMENTAL
  # Parâmetros do modelo de regionalização selecionado
  ParamSelec <- apply(dadosRes, 1, function(res){
    if(res["MODELO_REGIONALIZACAO"]=="KNN"){return(ParamKNN[res["ID"],])}
    if(res["MODELO_REGIONALIZACAO"]=="ML"){return(ParamML[res["ID"],])}
    if(res["MODELO_REGIONALIZACAO"]=="Calibração Local (Balanço Reverso)"){return(Param_CalLocal[res["ID"],])}
    if(res["MODELO_REGIONALIZACAO"]=="Médias Regionais"){return(rep(NA,4))}
    if(res["MODELO_REGIONALIZACAO"]=="Multimodelo"){return(rep(NA,4))}
  })
  ParamSelec <- t(ParamSelec)
  colnames(ParamSelec) <- c("SAT",  "PES" , "CREC", "K" )
  rownames(ParamSelec) <- dadosRes$ID

  # Organiza dados comuns às três abas KNN, ML e Selec
  L_Param         <- list(ParamKNN, ParamML, ParamSelec)
  L_AfluInc_anual <- list(AfluInc_KNN_anual, AfluInc_ML_anual, AfluInc_Selec_anual)
  L_DadBacInc     <- list()
  for(i in 1:3){
    L_DadBacInc[[i]]= data.frame(SAT  = L_Param[[i]][,"SAT"],
                                 PES  = L_Param[[i]][,"PES"],
                                 CREC = L_Param[[i]][,"CREC"],
                                 K    = L_Param[[i]][,"K"],
                                 Prec_media = sapply(PrecBaciaInc_anual,mean,na.rm=T),
                                 Deflu_medio_mm_ano  = sapply(L_AfluInc_anual[[i]],mean,na.rm=T)*3600*24*365*10^9/(AreaBaciaInc*10^12),
                                 Deflu_medio_hm3_ano = sapply(L_AfluInc_anual[[i]],mean,na.rm=T)*3600*24*365/10^6,
                                 CE = sapply(L_AfluInc_anual[[i]],mean,na.rm=T)*3600*24*365*10^9/(AreaBaciaInc*10^12)/sapply(PrecBaciaInc_anual,mean,na.rm=T),
                                 CV = sapply(L_AfluInc_anual[[i]],sd,na.rm=T)/sapply(L_AfluInc_anual[[i]],mean,na.rm=T))
  }
  names(L_DadBacInc) <- c("KNN", "ML", "Selec")

  # Dados próprios das abas KNN e Selec
  est_nomes_prox_ac <- sapply(est_prox_ac, function(est){est_nomes$Nome[est_nomes$Codigo==est]})
  DadBacInc_KNN_add   <- data.frame(Posto_Flu_ID = est_prox_ac, Posto_Flu_Nome = est_nomes_prox_ac)
  DadBacInc_Selec_add <- data.frame(Modelo_Escolhido = dadosRes$MODELO_REGIONALIZACAO)

  L_DadBacInc$KNN   <- data.frame(DadBacInc_KNN_add,L_DadBacInc$KNN)
  L_DadBacInc$Selec <- data.frame(DadBacInc_Selec_add,L_DadBacInc$Selec)

  # DADOS REFERENTES A REGULARIZAÇÃO E SIMULAÇÃO DA REDE
  # Regularização e Vertimento médios por açude
  L_Simulacao <- list(ResSimul_KNN,ResSimul_ML,ResSimul_Selec)
  n_mod       <- c("KNN", "ML","Selec")
  n_qg        <- c("Q90", "Q95", "Q98")
  L_Regularizacao <- list()
  L_Vertimento    <- list()

  for(mod in 1:length(n_mod)){
    L_Regularizacao[[mod]] <- list()
    L_Vertimento[[mod]]    <- list()

    for(qg in 1:length(n_qg)){
      L_Regularizacao[[mod]][[qg]] <- sapply(L_Simulacao[[mod]][[qg]],function(M){mean(M$Ret_m3s)})
      L_Vertimento[[mod]][[qg]]    <- sapply(L_Simulacao[[mod]][[qg]],function(M){mean(M$Ver_m3s)})
    }
    names(L_Regularizacao[[mod]]) <- n_qg
    names(L_Vertimento[[mod]])    <- n_qg
  }
  names(L_Regularizacao) <- n_mod
  names(L_Vertimento)    <- n_mod

  # Afluência total, Regularização e Balanço Médio
  L_AfluTot_anual <- list(LAfluTot_KNN_anual, LAfluTot_ML_anual, LAfluTot_Selec_anual)
  L_Qgaran        <- list(Qgaran_KNN, Qgaran_ML, Qgaran_Selec)
  L_DadosRedeBalancoMedio <- list()
  for(mod in 1:length(n_mod)){
    L_DadosRedeBalancoMedio[[mod]] <- list()
    for(qg in 1:length(n_qg)){
      Aflu <- L_AfluTot_anual[[mod]][[qg]]
      Reg  <- L_Regularizacao[[mod]][[qg]]
      Ver  <- L_Vertimento[[mod]][[qg]]
      Q9X  <- L_Qgaran[[mod]][,qg]
      L_DadosRedeBalancoMedio[[mod]][[qg]] <- data.frame( Afluencia_hm3_ano = sapply(Aflu,mean,na.rm=T)*3600*24*365/10^6,
                                                          CV_Afluencia      = sapply(Aflu,sd,na.rm=T)/sapply(Aflu,mean,na.rm=T),
                                                          Cap_VA            = Cap_hm3/(sapply(Aflu,mean,na.rm=T)*3600*24*365/10^6),
                                                          Q9X_l_s           = Q9X*1000,
                                                          Regularizado_hm3_ano = Reg*3600*24*365/10^6,
                                                          Regularizado_perc    = Reg/sapply(Aflu,mean,na.rm=T),
                                                          Vertido_hm3_ano      = Ver*3600*24*365/10^6,
                                                          Vertido_perc         = Ver/sapply(Aflu,mean,na.rm=T),
                                                          Evaporado_hm3_ano    = (sapply(Aflu,mean,na.rm=T)-Reg-Ver)*3600*24*365/10^6,
                                                          Evaporado_perc       = 1-(Reg+Ver)/sapply(Aflu,mean,na.rm=T))

    }
    names(L_DadosRedeBalancoMedio[[mod]]) <- n_qg
  }
  names(L_DadosRedeBalancoMedio) <- n_mod

  # DADOS AGREGADOS POR REGIÃO HIDROGRÁFICA
  RegHidro <- DadosGerais$Regiao_Hidrografica
  Capacid  <- DadosGerais$Capacidade_hm3
  names(RegHidro) <- DadosGerais$ID
  names(Capacid)  <- DadosGerais$ID
  L_DadosRedeBalancoMedio_AggReg <- Agg_RegHidr(RegHidro, Capacid, L_DadosRedeBalancoMedio)

  # SALVA ARQUIVOS
  if(!dir.exists(paste0(dir_rede_print, "/output/sintese"))){
    dir.create(paste0(dir_rede_print, "/output/sintese"),showWarnings = F)
  }
  unlink(paste0(dir_rede_print,"/output/sintese/*"),recursive = F)
  saveRDS(DadosGerais, paste0(dir_rede_print,"/output/sintese/DadosGerais.rds"))
  saveRDS(L_DadBacInc,paste0(dir_rede_print,"/output/sintese/L_DadBacInc.rds"))
  saveRDS(L_DadosRedeBalancoMedio,paste0(dir_rede_print,"/output/sintese/L_DadosRedeBalancoMedio.rds"))
  saveRDS(L_DadosRedeBalancoMedio_AggReg,paste0(dir_rede_print,"/output/sintese/L_DadosRedeBalancoMedio_AggReg.rds"))

  # ORGANIZA DADOS PARA ARQUIVO SÍNTESE
  # Reordena segundo os nomes dos reservatórios
  ordem           <- order(DadosGerais$Regiao_Hidrografica, DadosGerais$Reservatorio)
  DadosGerais_ord <- DadosGerais[ordem,]
  L_DadBacInc_ord <- lapply(L_DadBacInc, function(X){X[ordem,]})
  L_DadosRedeBalancoMedio_ord <- lapply(L_DadosRedeBalancoMedio, function(li){lapply(li, function(X){X[ordem,]})})

  DF_KNN  = data.frame(DadosGerais_ord, L_DadBacInc_ord$KNN,
                       L_DadosRedeBalancoMedio_ord$KNN$Q90,
                       L_DadosRedeBalancoMedio_ord$KNN$Q95,
                       L_DadosRedeBalancoMedio_ord$KNN$Q98)
  DF_ML   = data.frame(DadosGerais_ord, L_DadBacInc_ord$ML,
                       L_DadosRedeBalancoMedio_ord$ML$Q90,
                       L_DadosRedeBalancoMedio_ord$ML$Q95,
                       L_DadosRedeBalancoMedio_ord$ML$Q98)
  DF_Selec = data.frame(DadosGerais_ord, L_DadBacInc_ord$Selec,
                       L_DadosRedeBalancoMedio_ord$Selec$Q90,
                       L_DadosRedeBalancoMedio_ord$Selec$Q95,
                       L_DadosRedeBalancoMedio_ord$Selec$Q98)

  # indices para adição de dados agregados por bacia
  n_linhas <- nrow(DadosGerais_ord)
  selec <- logical()
  for(i in 1:(n_linhas-1)){
    selec[i] <- DadosGerais_ord$Regiao_Hidrografica[i+1] != DadosGerais_ord$Regiao_Hidrografica[i]
  }
  ind_acres <- which(selec)
  ind_acres <- c(ind_acres, n_linhas)+1

  # Adição de linhas com valores agregados por região hidrogáfica
  DF_list     <- list(KNN=DF_KNN, ML=DF_ML, Modelo_Selecionado=DF_Selec)
  DF_list_fim <- Add_linhasAggRegHidro(RegHidro, Capacid, DF_list, L_DadosRedeBalancoMedio_AggReg, ind_acres)


  # ESTILOS
  # Estilo basico
  estilo_base <- createStyle(fontName = "Calibre", fontSize = 11,
                            fgFill = NULL, numFmt = "0.00")
  estilo_prec <- createStyle(fontName = "Calibre", fontSize = 11, numFmt = "0.0%")

  # Estilo de linhas com valores agregados por Região Hidrográfica
  estilo_agg <- createStyle(fontName = "Calibre", fontSize = 11,
                            textDecoration = "bold", fgFill = "yellow", numFmt = "0.00")
  estilo_agg_prec <- createStyle(fontName = "Calibre", fontSize = 11,
                            textDecoration = "bold", fgFill = "yellow", numFmt = "0.00%")

  # Estilo de linhas de reservatórios com regionalização do tipo Médias Regionais
  estilo_MR <- createStyle(fontName = "Calibre", fontSize = 11,
                            fgFill = "lightgray", numFmt = "0.00")
  estilo_MR_prec <- createStyle(fontName = "Calibre", fontSize = 11,
                                 fgFill = "lightgray", numFmt = "0.00%")

  # ESCREVE ARQUIVO SÍNTESE SOBRE A NOVA FORMATAÇÃO
  # Obter os nomes das planilhas
  sheets <- arq_format$sheet_names

  # Número de colunas de cada sheet
  nCol_sheet <- sapply(DF_list_fim, ncol)

  # Colunas com percentuais
  col_per <- c(17, 24, 26, 28, 34, 36, 38, 44, 46, 48)
  L_col_per <-list(col_per+2, col_per, col_per+1)

  # Iterar sobre as planilhas
  for (sh in 1:3) {
    sheet_name <- sheets[sh]

    # Escrever os dados começando na linha 5 e na coluna 1
    writeData(arq_format, sheet = sheet_name, x = DF_list_fim[[sh]], startRow = 5, startCol = 1, colNames = FALSE)

    # Estilo básico
    addStyle(arq_format, sheet = sheet_name, style = estilo_base, rows = 5:(nrow(DF_list_fim[[sh]])+4), cols = 1:nCol_sheet[sh], gridExpand = T)
    addStyle(arq_format, sheet = sheet_name, style = estilo_prec, rows = 5:(nrow(DF_list_fim[[sh]])+4), cols = (nCol_sheet[sh] - c(0,2,4,10,12,14,20,22,24,31)), gridExpand = T)

    # Adiciona estilo de linhas com valores agregados por Região Hidrográfica
    lnhs_agg <- which(is.na(DF_list_fim[[sh]]$ID))+4
    addStyle(arq_format, sheet = sheet_name, style = estilo_agg,      rows = lnhs_agg, cols = 1:nCol_sheet[sh], gridExpand = T)
    addStyle(arq_format, sheet = sheet_name, style = estilo_agg_prec, rows = lnhs_agg, cols = L_col_per[[sh]], gridExpand = T)
  }
  # Adiciona estilo de linhas de reservatórios com regionalização do tipo Médias Regionais
  lnhs_MR <- which(DF_list_fim[[3]]$Modelo_Escolhido=="Médias Regionais")+4
  addStyle(arq_format, sheet = sheets[3], style = estilo_MR,      rows = lnhs_MR, cols = 1:nCol_sheet[3], gridExpand = T)
  addStyle(arq_format, sheet = sheets[3], style = estilo_MR_prec, rows = lnhs_MR, cols = L_col_per[[3]], gridExpand = T)


  # Salvar o workbook
  saveWorkbook(arq_format, paste0(dir_rede_print, "/output/sintese/Sintese.xlsx"), overwrite = TRUE)

  return("Arquivo Síntese.xlsx escrito")

}


#' Escrever arquivos de séries da simulação
#'
#' Função escreve arquivos .xlsx com séries de vazão afluente
#' Pode ser aplicada para a rede de reservatórios em teste ou para a
#' rede de reservatórios consolidada (versão atual ou anteriores).
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param versao_ref Caractere com a versão de referência ("redeConsol", "redeTeste", "Versao_i" ou "Teste_i" com i inteiro>=0)
#'
#' @seealso \code{\link{FUNC_print_Sintese}}
#' @return "Arquivos de séries de afluência escritos"
#' @import openxlsx
#'
#' @export
#'
#'
SisMEVAZ_print_Series <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"), versao_print="redeTeste"){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")

  if(versao_print=="redeConsol"){
    dir_rede_print <- dir_rede_conso
  }else if(versao_print=="redeTeste"){
    dir_rede_print <- dir_rede_teste
  }else if(grepl("Versao_", versao_print)){
    vv <-  gsub(pattern = "Versao_", replacement = "", x = versao_print)
    dir_rede_print <- paste0(dir_rede_conso,"/versoes_anteriores/versao_",vv)
  }else{
    tt <-  gsub(pattern = "Teste_", replacement = "", x = versao_print)
    dir_rede_print <- paste0(diretorio_de_dados,"/Testes_outros/teste_",tt)
  }

  ## LEITURA DE DADOS
  dadosRes       <- read.xlsx(paste0(dir_rede_print,"/output/Dados_Reservatorios.xlsx"))
  dadosRes       <- dadosRes[dadosRes$PRINT_FINAL==1,] # filtra reservatórios
  AfluInc_Selec  <- readRDS(paste0(dir_rede_print,"/output/AfluenciasIncrementais_Selec.rds"))
  ResSimul_Selec <- readRDS(paste0(dir_rede_print,"/output/Resultados_SimulacaoRede_Selec.rds"))

  if(!dir.exists(paste0(dir_rede_print, "/output/series"))){
    dir.create(paste0(dir_rede_print, "/output/series"),showWarnings = F)
  }
  unlink(paste0(dir_rede_print,"/output/series/*"),recursive = T)

  dir.create(paste0(dir_rede_print, "/output/series/aflu_incr"),showWarnings = F)
  for(i in 1:length(dadosRes$ID)){
    serie <- AfluInc_Selec[[i]]
    datas <- as.Date(names(serie))

    mat   <- Org_serie_tab(serie, datas)
    acude <- dadosRes$ACUDE[i]
    id    <- dadosRes$ID[i]
    write.xlsx(mat,rowNames=T,paste0(dir_rede_print,"/output/series/aflu_incr/aflu_incr_",acude,"_",id,".xlsx"))
  }

  dir.create(paste0(dir_rede_print, "/output/series/aflu_tot"),showWarnings = F)
  Qreg <- names(ResSimul_Selec)
  for(i in 1:length(dadosRes$ID)){

    for(j in 1:length(ResSimul_Selec)){
      serie <- ResSimul_Selec[[j]][[i]]$Aflu_m3s
      datas <- ResSimul_Selec[[j]][[i]]$Datas

      mat   <- Org_serie_tab(serie, datas)
      acude <- dadosRes$ACUDE[i]
      id    <- dadosRes$ID[i]
      write.xlsx(mat,rowNames=T,paste0(dir_rede_print,"/output/series/aflu_tot/aflu_tot_",Qreg[j],"_",acude,"_",id,".xlsx"))
    }
  }
  return("Arquivos de séries de afluência escritos")
}

#' Atualização da rede consolidada - inclusão de reservatório
#'
#' Substitui a rede teste como nova rede consolidada, substituindo os arquivos no
#' diretório "diretorio_de_dados/RededeReservatorios_consolidada"
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#'
#' @details Salva a veãrso anterior da rede consolidada no diretório
#' "diretorio_de_dados/RededeReservatorios_consolidada/versoes_anteriores" e atualiza o log
#'
#' @seealso \code{\link{FUNC_redeTeste}}
#' @return "Simlação da rede teste finalizada"
#' @import openxlsx
#' @export
#'
#'
SisMEVAZ_atualiz_redeTeste_para_redeConsol <- function(diretorio_de_dados=paste0(getwd(),"/../../dados")){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")

  # RESERVATÓRIOS MODIFICADOS
  dadosRes_tmp <- read.xlsx(paste0(dir_rede_teste,"/input/Dados_Novos_Reservatorios.xlsx"),colNames = F,rowNames = T)
  dadosRes_tmp <- as.data.frame(t(dadosRes_tmp))
  nvs_acu <- dadosRes_tmp$ACUDE

  # FAZ BACKUP DA REDE CONSOLIDADA
  versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
  versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva))
  file.copy(from=paste0(dir_rede_conso,"/input"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)
  file.copy(from=paste0(dir_rede_conso,"/output"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)

  # ATUALIZA REDE CONSOLIDADA A PARTIR DA REDE TESTE
  # Diretório Input
  unlink(paste0(dir_rede_conso,"/input/*"),recursive = F)
  arq <- paste0(dir_rede_teste,"/output",  c("/output/Afluencias_CalLocal.rds",
                                             "/output/AfluenciasIncrementais_KNN.rds",
                                             "/output/AfluenciasIncrementais_ML1.rds",
                                             "/output/CAVs.xlsx",
                                             "/output/Dados_Reservatorios.xlsx",
                                             "/output/ETP_Mensal_PM_Bacias_Incrementais.rds" ,
                                             "/output/MatrizCarac_BacAcu.rds",
                                             "/output/Precipitacao_Mensal_Bacias_Incrementais.rds",
                                             "/output/Resultados_SimulacaoRede_KNN.rds",
                                             "/output/Resultados_SimulacaoRede_ML.rds",
                                             "/output/Resultados_SimulacaoRede_Selec.rds") )

  file.copy(from= arq,
            to = paste0(dir_rede_conso,"/input"),  overwrite = T)
  shp <- list.files(paste0(dir_rede_teste,"/input/shapes"), full.names = T)
  file.copy(from=shp,
            to = paste0(dir_rede_conso,"/input/shapes"), overwrite = T)

  # Diretório Output
  file.copy(from=paste0(dir_rede_teste,"/output"),
            to = paste0(dir_rede_conso),
            recursive = T)

  # ATUALIZA LOG
  logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_rede_conso,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão ",n_versao_salva," --> versão atual: Incorporação de Rede teste (",paste0(nvs_acu,collapse = " - "),")"),file = logfile,append = T)

  # SINTESE
  SisMEVAZ_print_Sintese(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  # COMPARAÇÃO COM A VERSÃO IMEDIATAMENTE ANTERIOR
  arq_format <- SisMEVAZ_comparaVersoes(diretorio_de_dados = diretorio_de_dados,
                                        versao_ref = paste0("Versao_",n_versao_salva), versao_nov = "redeConsol")

  saveWorkbook(arq_format, paste0(dir_rede_conso, "/output/sintese/Comparacao Versao_",n_versao_salva+1," em relacao a Versao_",n_versao_salva,".xlsx"), overwrite = TRUE)

  # SÉRIES
  SisMEVAZ_print_Series(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  return("Cópia da rede teste finalizada")

}



#' Atualização da rede consolidada - retirada de reservatórios da rede
#'
#' Condideram-se duas modalidades de retirada de reservatório da rede:
#' (1) Dados não apresentados: reservatórios com impacto relevante, ainda são simulados,
#'  mas, por não serem mais monitorados pela COGERH, deixaram de ter seus dados apresentados
#' (2) Retirados efetivamente da simulação: Por razões espefícas (ex: bacia ou reservatório
#'  muito pequeno) o nível de incerteza da simulação pode levar a optar por excluir o
#'  reservatório da cascata. Nesse caso, os reservatórios a jusante precisaraão ser resimulados"
#'  O segundo modo ainda está em desenvolvimento
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param ids_res_ret vetor de inteiros com identificadores dos reservatórios a serem retirados
#' @param modo  Caractere com os dois modos de retirada ("NO_SHOW" ou "FORCED")
#'
#'
#' @details Salva a veãrso anterior da rede consolidada no diretório
#' "diretorio_de_dados/RededeReservatorios_consolidada/versoes_anteriores" e atualiza o log
#'
#' @seealso \code{\link{FUNC_redeTeste}}
#' @return "Simlação da rede finalizada"
#' @import openxlsx
#' @export
#'
#'
SisMEVAZ_retira_reserv_redeConsol <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"), ids_res_ret ,modo = "NO_SHOW"){

  # VALIDA MODO DE RETIRADA
    if(modo != "NO_SHOW" & modo != "FORCED"){
      stop("Modo de retirada incompatível. Opções: NO_SHOW ou FORCED")
    }
  if(modo == "FORCED"){ stop("Modo não implementado")}

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")

  # RESERVATÓRIOS RETIRADOS
  dadosRes_consol <- read.xlsx(paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"),colNames = T)
  nms_res_ret     <- sapply(as.character(ids_res_ret), function(id){dadosRes_consol$ACUDE[dadosRes_consol$ID==id]})

  # FAZ BACKUP DA REDE CONSOLIDADA
  versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
  versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva))
  file.copy(from=paste0(dir_rede_conso,"/input"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)
  file.copy(from=paste0(dir_rede_conso,"/output"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)

  # ATUALIZA REDE CONSOLIDADA
  if(modo == "NO_SHOW"){

    tmp <- which(is.element(dadosRes_consol$ID,as.character(ids_res_ret)))
    dadosRes_consol[tmp,"PRINT_FINAL"] <- 0

    # Diretório Input
    write.xlsx(dadosRes_consol,paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"))
    # Diretório Output
    write.xlsx(dadosRes_consol,paste0(dir_rede_conso,"/output/Dados_Reservatorios.xlsx"))


  }else{
    # Nesse modo, devemos excluir as shapes dos reservatórios retirados,
    # reagragar a bacia incremental dos resevtórios a jusante deste,
    # recalcular suas precipitaçoes, reaplicar a regionalização e a simulação
    # hidrológica.
    #
    # As etapas final podem ser realizadas com o uso das funções:
    # SisMEVAZ_redeTeste e SisMEVAZ_atualiz_redeTeste_para_redeConsol
    stop("Modo não implementado")

  }

  # ATUALIZA LOG
  logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_rede_conso,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão ",n_versao_salva," --> versão atual: Retirada de reservatórios (",paste0(nms_res_ret,collapse = ", "),") no modo ",modo),file = logfile,append = T)


  # SINTESE
  SisMEVAZ_print_Sintese(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  # COMPARAÇÃO COM A VERSÃO IMEDIATAMENTE ANTERIOR
  arq_format <- SisMEVAZ_comparaVersoes(diretorio_de_dados = diretorio_de_dados,
                                        versao_ref = paste0("Versao_",n_versao_salva), versao_nov = "redeConsol")

  saveWorkbook(arq_format, paste0(dir_rede_conso, "/output/sintese/Comparacao Versao_",n_versao_salva+1," em relacao a Versao_",n_versao_salva,".xlsx"), overwrite = TRUE)

  # SÉRIES
  SisMEVAZ_print_Series(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  return("Retirada de reservatórios finalizada")

}


#' Atualização da rede consolidada - modifica método de cálculo das afluências de reservatórios
#' considerando os parâmetros calibrados com balanço reverso (calibração local)
#'
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param ids_bal_rev vetor de inteiros com identificadores dos reservatórios a terem afluências calcyuladas com parâmetros poveninetes da calibração local (balanço reverso)
#'
#' @details Salva a veãrso anterior da rede consolidada no diretório
#' "diretorio_de_dados/RededeReservatorios_consolidada/versoes_anteriores" e atualiza o log
#'
#' @seealso \code{\link{FUNC_redeTeste}}
#' @return "Simlação da rede finalizada"
#' @import openxlsx
#' @export
#'
#'
SisMEVAZ_atrbui_calib_local_redeConsol <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"), ids_bal_rev ){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_Plu_ETP    <- paste0(diretorio_de_dados,"/Plu_ETP")

  ## LEITURA DE DADOS FIXOS (independem da rede de reservatórios)
  # Evaporação de referência
  Evapm_ref <- read.xlsx(paste0(dir_Plu_ETP,"/EVAPORACAO_PICHE.xlsx"),colNames = T)

  # Dados dos reservatórios com regionalização do tipo Médias Regionais
  datMedReg <- read.xlsx(paste0(dir_dado_fixo,"/CE_CV_MedReg.xlsx"),colNames = T)

  # Parâmetros calibrados com vazões do balanço reverso (calibração local)
  Param_CalLocal  <- read.xlsx(paste0(dir_dado_fixo,"/Calibracao_com_balanco_reverso.xlsx"),colNames = T,rowNames = T)[,-1]



  ## LEITURA DE DADOS DA REDE CONSOLIDADA (a serem atualizados na rede-teste)
  dadosRes_consol    <- read.xlsx(paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"),colNames = T)
  id_res             <- dadosRes_consol$ID

  Carac_consol       <- readRDS(paste0(dir_rede_conso,"/input/MatrizCarac_BacAcu.rds"))
  ETP_consol         <- readRDS(paste0(dir_rede_conso,"/input/ETP_Mensal_PM_Bacias_Incrementais.rds"))
  Pre_consol         <- readRDS(paste0(dir_rede_conso,"/input/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  Datas              <- names(Pre_consol$`1`)

  AfluInc_KNN_consol   <- readRDS(paste0(dir_rede_conso,"/input/AfluenciasIncrementais_KNN.rds"))
  AfluInc_ML_consol    <- readRDS(paste0(dir_rede_conso,"/input/AfluenciasIncrementais_ML1.rds"))
  Aflu_CalLocal_consol <-readRDS(paste0(dir_rede_conso,"/input/Afluencias_CalLocal.rds"))

  CAVs_consol        <- read.xlsx(paste0(dir_rede_conso,"/input/CAVs.xlsx"),colNames = T)

  ResSimul_Selec_consol <- readRDS(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))

  # ordem de afluencia entre reservatórios da rede-teste
  Ord_aflu        <- dadosRes_consol[,c("ID","ID_ACJUS")]

  # Precipitação média (usada nos casos de médias regionais)
  Prec_anual <- Agg_anual(Lista_series = Pre_consol,func_agg = sum)
  Prec_med   <- sapply(Prec_anual,mean,na.rm=T)


  ## TESTA EXISTÊNCIA DE PARÂMETROS PARA AS BACIAS DOS RESERVATÓRIOS A TEREM NOVO MÉTODO APLICADO
  ids_bal_rev <- as.character(ids_bal_rev)
  nms_bal_rev <- sapply(as.character(ids_bal_rev), function(id){dadosRes_consol$ACUDE[dadosRes_consol$ID==id]})
  ids_possiv  <- rownames(Param_CalLocal)[apply(Param_CalLocal,1,function(x){all(!is.na(x))})]

  selec <- !is.element(ids_bal_rev,ids_possiv)
  if(sum(selec)>0){
    stop(paste0("Reservatórios com os seguintes IDs não têm disponíveis parametros calibrados localmente: ",paste(ids_bal_rev[selec],collapse = ", "),
                "(ver arquivo dados/Dados_fixos/Calibracao_com_balanco_reverso.xlsx)"))
  }

  ## TESTA SE AS BACIAS DOS RESERVATÓRIOS A TEREM NOVO MÉTODO APLICADO SÃO DE MONTANTE (COMPATÍVEL COM A CALIBRAÇÃO)
  selec <- is.element(ids_bal_rev,Ord_aflu$ID_ACJUS)
  if(sum(selec)>0){
    stop(paste0("Reservatórios com os seguintes IDs não têm outros reservatórios a montante: ",paste(ids_bal_rev[selec],collapse = ", "),
                "(os parâmetros calibrados localmente são para a bacia total)"))
  }

  ## SIMULAÇÃO HIDROLÓGICA
  # Cálculo de afluências incrementais apenas para as bacias dos reservatórios com novo método (ids_bal_rev)
  vecArea     <- Carac_consol[,"Area_km2"]
  Aflu_CalLocal_tmp <- SMAP.listaBac(matParam = Param_CalLocal, vecArea = vecArea,
                                   lPrec = Pre_consol, lETo_med = ETP_consol,
                                   ids = ids_bal_rev)


  # Atualiza Aflu_CalLocal
  Aflu_CalLocal_consol  <- FUNC_atualiz_lista_vec(List_tmp = Aflu_CalLocal_tmp, List_consol = Aflu_CalLocal_consol)


  # Alteração do valor na coluna MODELO_REGIONALIZACAO incluindo Calibração Local (Balanço Reverso)
  dadosRes_consol[is.element(dadosRes_consol$ID, ids_bal_rev),]$MODELO_REGIONALIZACAO <- "Calibração Local (Balanço Reverso)"

  # Verifica consistência dos reservatórios com regionalização simplificada (Modelos Regionais)
  ModRegion          <- dadosRes_consol$MODELO_REGIONALIZACAO
  names(ModRegion)   <- dadosRes_consol$ID
  RegHidro           <- dadosRes_consol$REG_HIDRO
  names(RegHidro)    <- dadosRes_consol$ID
  ids_MR <- names(ModRegion)[ModRegion=="Médias Regionais"]
  verif  <- all(sapply(ids_MR, function(x){is.element(x,datMedReg$ID)}))
  if(!verif){stop("Reservatório com regionalização do tipo Médias Regionais não está presente no arquivo CE_CV_MedReg.xlsx")}



  # Afluências incrmentais (seleção do modelo de regionalização)
  AfluInc_Selec_consol <- FUNC_SelecRegion(AfluInc_KNN = AfluInc_KNN_consol,
                                          AfluInc_ML = AfluInc_ML_consol,
                                          Aflu_CalLocal = Aflu_CalLocal_consol,
                                          ModRegion, RegHidro, Prec_med,
                                          Areas=vecArea, datMedReg)



  # Seleciona reservatórios finais das cascatas a serem simuladas
  resFimRede_selec <- FUNC_resFimRede_selec(Ord_aflu, res_mod=ids_bal_rev) # apenas os finais
  resRede_selec    <- FUNC_resRede_selec(Ord_aflu, resFimRede_selec)  # todos na rede

  # Serie de Evaporação nos reservatórios
  ResEst_selec <- dadosRes_consol[which(is.element(dadosRes_consol$ID, resRede_selec)),
                                 c("ID","COD_EST_EVAP")]
  EvapSerie <- MedMensalEst_para_SerieRes(Mat_MedMensal = Evapm_ref,
                                          ResEst = ResEst_selec,
                                          Datas = Datas)

  # Inicialização dos volumes nos reservatórios
  vecVoliniperc        <- rep(0.5,length(resRede_selec))
  names(vecVoliniperc) <- resRede_selec


  # Simula as redes de reservatórios às quais pertencem os reservatórios com método modificado
  ResSimul_Selec_tmp <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                          vecGarantia = c(0.90, 0.95, 0.98),
                                                          AfluInc = AfluInc_Selec_consol,
                                                          EvapSerie, Ord_aflu, DadosRes=dadosRes_consol,
                                                          DadosCAV=CAVs_consol, vecVoliniperc)

  # Atualiza resultados das simulações da rede consolidada para a rede-teste
  Qgaran_nms <- c("Q90","Q95","Q98")
  ResSimul_Selec_consol <- lapply(Qgaran_nms, function(qq){
    FUNC_atualiz_lista_vec(List_tmp = ResSimul_Selec_tmp[[qq]], List_consol = ResSimul_Selec_consol[[qq]])
  })
  names(ResSimul_Selec_consol) <- Qgaran_nms

  # Extrai vazão regulariza a partir da simlação (máxima valor operado na simulação)
  Qgaran_Selec <- sapply(ResSimul_Selec_consol, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})


  # FAZ BACKUP DA REDE CONSOLIDADA
  versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
  versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva))
  file.copy(from=paste0(dir_rede_conso,"/input"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)
  file.copy(from=paste0(dir_rede_conso,"/output"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)


  # ATUALIZA ARQUIVOS PARA NOVA REDE CONSOLIDADA (Input e Output pré-simulação da rede)
  # Input
  file.remove(paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Afluencias_CalLocal.rds"))

  # Output
  file.remove(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_Selec.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Afluencias_CalLocal.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Dados_Reservatorios.xlsx"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_Selec.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_Selec.rds"))
  unlink(paste0(dir_rede_conso,"/output/sintese/*"),recursive=T)
  unlink(paste0(dir_rede_conso,"/output/series/aflu_incr/*"),recursive=T)
  unlink(paste0(dir_rede_conso,"/output/series/aflu_tot/*"),recursive=T)

  ## SALVA ARQUIVOS ATUALIZADOS
  # Input
  write.xlsx(dadosRes_consol,    paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"))
  saveRDS(ResSimul_Selec_consol, paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))
  saveRDS(Aflu_CalLocal_consol,  paste0(dir_rede_conso,"/input/Afluencias_CalLocal.rds"))

  # Output
  saveRDS(AfluInc_Selec_consol, paste0(dir_rede_conso,"/output/AfluenciasIncrementais_Selec.rds"))
  saveRDS(Aflu_CalLocal_consol, paste0(dir_rede_conso,"/output/Afluencias_CalLocal.rds"))
  write.xlsx(dadosRes_consol,   paste0(dir_rede_conso,"/output/Dados_Reservatorios.xlsx"))
  saveRDS(ResSimul_Selec_consol,paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_Selec.rds"))
  saveRDS(Qgaran_Selec,         paste0(dir_rede_conso,"/output/Qgaran_Selec.rds"))



  # ATUALIZA LOG
  logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_rede_conso,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão ",n_versao_salva," --> versão atual: Atribuição do modo Calibração Local para os reservatórios:",paste0(nms_bal_rev,collapse = ", ")),file = logfile,append = T)


  # SINTESE
  SisMEVAZ_print_Sintese(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  # COMPARAÇÃO COM A VERSÃO IMEDIATAMENTE ANTERIOR
  arq_format <- SisMEVAZ_comparaVersoes(diretorio_de_dados = diretorio_de_dados,
                                        versao_ref = paste0("Versao_",n_versao_salva), versao_nov = "redeConsol")

  saveWorkbook(arq_format, paste0(dir_rede_conso, "/output/sintese/Comparacao Versao_",n_versao_salva+1," em relacao a Versao_",n_versao_salva,".xlsx"), overwrite = TRUE)

  # SÉRIES
  SisMEVAZ_print_Series(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  return("Retirada de reservatórios finalizada")

}



#' Atualização da rede consolidada - nova geometria dos reservatórios
#'
#' Simulação da MEVAZ com novas CAVs e capacidades (a partir da cota do vertedouro)
#' dos reservatórios e atualiza a rede consolidada
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote (descompactado)
#' @param arq_CAVs Caractere com o caminho para o arquivo com novas CAVs. Se NULL busca CAVs no banco de dados.
#' @param arq_cotaVert Caractere com o caminho para o arquivo com novas cotas do vertedouro.Se NULL busca CAVs no banco de dados.
#'
#'@details Salva a versão anterior da rede consolidada no diretório
#' "diretorio_de_dados/RededeReservatorios_consolidada/versoes_anteriores" e atualiza o log
#'
#' @return "Atualização de CAVs finalizada"
#' @import openxlsx
#' @importFrom RPostgreSQL dbConnect dbGetQuery dbDisconnect
#'
#' @export
#'
SisMEVAZ_atualiz_CAVs_redeConsol <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"),
                                             arq_CAVs=NULL, arq_cotaVert=NULL){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_Plu_ETP    <- paste0(diretorio_de_dados,"/Plu_ETP")

  ## DADOS DA REDE CONSOLIDADA (ANTERIOR)
  dadosRes <- read.xlsx(paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"),colNames = T)
  CAVs     <- read.xlsx(paste0(dir_rede_conso,"/input/CAVs.xlsx"),colNames = T)
  id_res   <- dadosRes$ID

  # Evaporação de referência
  Evapm_ref <- read.xlsx(paste0(dir_Plu_ETP,"/EVAPORACAO_PICHE.xlsx"),colNames = T)

  # Afluências incrementais
  AfluInc_KNN    <- readRDS(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_KNN.rds"))
  AfluInc_ML     <- readRDS(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_ML1.rds"))
  AfluInc_Selec  <- readRDS(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_Selec.rds"))

  Datas <- names(AfluInc_Selec$`1`)

  ## LÊ CAVS E COTAS DO VERTEDOURO
  if(is.null(arq_CAVs)){
    CAVs_nvs <- FUNC_ler_CAVs_banco(id_res) # Lê no banco
  }else{
    CAVs_nvs <- read.xlsx(arq_CAVs) # Lê de arquivo
    if(any(!is.element(id_res, CAVs_nvs$ID))){
      res_semdado <- id_res[!is.element(id_res, CAVs_nvs$ID)]
      warning(paste0("Novo arquivo de CAVs não possui dados  para o(s) reservatório(s): ",paste0(res_semdado,collapse = ", ")))
      confirmacao <- readline("As CAVs desses reservatórios serão mantidas iguais às anteriores. Confirma? (S/N)")
      if(confirmacao=="N"){
        stop("Atualização de CAVs iterrompida")
      }
    }
    if(any(!is.element(as.numeric(CAVs_nvs$ID),id_res))){
      CAVs_nvs <- CAVs_nvs[-which(!is.element(as.numeric(CAVs_nvs$ID),id_res)),]
    }
    CAVs_nvs$COTA <- as.numeric(CAVs_nvs$COTA)
    CAVs_nvs$VOLUME_M3 <- as.numeric(CAVs_nvs$VOLUME_M3)
    CAVs_nvs$AREA_KM2 <- as.numeric(CAVs_nvs$AREA_KM2)

    CAVs_nvs <- CAVs_nvs[order(as.numeric(CAVs_nvs$ID)),]
  }


  if(is.null(arq_cotaVert)){
    Cota_ver <- FUNC_ler_cotaver(id_res) # Lê no banco
  }else{
    Cota_ver <- read.xlsx(arq_cotaVert)
  }

  FUNC_verif_cotas_vert_CAVs(cotas_vert = Cota_ver,CAVs = CAVs_nvs,  arquivo_pdf = file.path(dir_rede_conso,"Relatorio_CAVs.pdf"), dadosRes = dadosRes)

  warning(paste0("Analise o Relatório de CAVs impresso no diretório ",dir_rede_conso))
  resp <- readline("Deseja continuar a atualização de CAVs e forçar CAV a ficarem crescentes? (s/n) ")
  if(resp=="s"){
    CAVs_nvs <- Forca_CAVs_ordcres(CAVs_nvs,ids_cor)
  }else{
    stop("Atualização de CAVs interrompida!")
  }


  # FAZ BACKUP DA REDE CONSOLIDADA
  versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
  versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva))
  file.copy(from=paste0(dir_rede_conso,"/input"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)
  file.copy(from=paste0(dir_rede_conso,"/output"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)


  # ATUALIZA ARQUIVOS PARA NOVA REDE CONSOLIDADA (Input e Output pré-simulação da rede)
  CAVs_atualiz      <- FUNC_atualiz_mat(mat_tmp = CAVs_nvs, mat_consol = CAVs,
                                     res_tmp = CAVs_nvs[,"ID"], res_consol = CAVs[,"ID"])
  Cotas_atualiz     <- FUNC_atualiz_mat(mat_tmp = Cota_ver, mat_consol = dadosRes[,c("ID","COTA_VERT_M")],
                                       res_tmp = Cota_ver[,"ID"], res_consol = dadosRes[,c("ID")])
  dadosRes_atualiz  <- dadosRes
  dadosRes_atualiz$COTA_VERT_M <- Cotas_atualiz$COTA_VERT_M

  # Input
  write.xlsx(dadosRes_atualiz,paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"))
  write.xlsx(CAVs_atualiz,paste0(dir_rede_conso,"/input/CAVs.xlsx"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_ML.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))

  # Output
  write.xlsx(dadosRes_atualiz,paste0(dir_rede_conso,"/output/Dados_Reservatorios.xlsx"))
  write.xlsx(CAVs_atualiz,paste0(dir_rede_conso,"/output/CAVs.xlsx"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_ML.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_Selec.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_ML.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_Selec.rds"))
  unlink(paste0(dir_rede_conso,"/output/sintese/*"),recursive=T)
  unlink(paste0(dir_rede_conso,"/output/series/aflu_incr/*"),recursive=T)
  unlink(paste0(dir_rede_conso,"/output/series/aflu_tot/*"),recursive=T)

  # SIMULAÇÃO HIDROLÓGICA COM NOVAS CAVS (Atualiza Input e Output pós-simulação da rede)
  # ordem de afluencia entre reservatórios da rede-teste
  Ord_aflu <- dadosRes_atualiz[,c("ID","ID_ACJUS")]

  # Seleciona reservatórios finais das cascatas a serem simuladas
  resFimRede_selec <- FUNC_resFimRede_selec(Ord_aflu, res_mod=id_res) # apenas os finais

  # Serie de Evaporação nos reservatórios
  ResEst_selec <- dadosRes_atualiz[,c("ID","COD_EST_EVAP")]
  EvapSerie <- MedMensalEst_para_SerieRes(Mat_MedMensal = Evapm_ref,
                                          ResEst = ResEst_selec,
                                          Datas = Datas)

  # Inicialização dos volumes nos reservatórios
  vecVoliniperc        <- rep(0.5,length(id_res))
  names(vecVoliniperc) <- id_res


  # Simula as redes de reservatórios às quais pertencem os reservatórios novos na rede-teste
  ResSimul_KNN  <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                         vecGarantia = c(0.90, 0.95, 0.98),
                                                         AfluInc = AfluInc_KNN,
                                                         EvapSerie, Ord_aflu, DadosRes=dadosRes_atualiz,
                                                         DadosCAV=CAVs_atualiz, vecVoliniperc)

  ResSimul_ML   <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                         vecGarantia = c(0.90, 0.95, 0.98),
                                                         AfluInc = AfluInc_ML,
                                                         EvapSerie, Ord_aflu, DadosRes=dadosRes_atualiz,
                                                         DadosCAV=CAVs_atualiz, vecVoliniperc)

  ResSimul_Selec <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                          vecGarantia = c(0.90, 0.95, 0.98),
                                                          AfluInc = AfluInc_Selec,
                                                          EvapSerie, Ord_aflu, DadosRes=dadosRes_atualiz,
                                                          DadosCAV=CAVs_atualiz, vecVoliniperc)

  # Extrai vazão regulariza a partir da simlação (máxima valor operado na simulação)
  Qgaran_KNN   <- sapply(ResSimul_KNN, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_ML    <- sapply(ResSimul_ML, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_Selec <- sapply(ResSimul_Selec, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})

  # Salva Arquivos Input
  saveRDS(ResSimul_KNN,paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_KNN.rds"))
  saveRDS(ResSimul_ML ,paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_ML.rds"))
  saveRDS(ResSimul_Selec ,paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))

  # Salva Arquivos Output
  saveRDS(ResSimul_KNN,paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_KNN.rds"))
  saveRDS(ResSimul_ML ,paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_ML.rds"))
  saveRDS(ResSimul_Selec ,paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_Selec.rds"))
  saveRDS(Qgaran_KNN,paste0(dir_rede_conso,"/output/Qgaran_KNN.rds"))
  saveRDS(Qgaran_ML ,paste0(dir_rede_conso,"/output/Qgaran_ML.rds"))
  saveRDS(Qgaran_Selec ,paste0(dir_rede_conso,"/output/Qgaran_Selec.rds"))

  # ATUALIZA LOG
  logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_rede_conso,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão ",n_versao_salva," --> versão atual: Atualização de CAVs"),file = logfile,append = T)

  # SINTESE
  SisMEVAZ_print_Sintese(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  # COMPARAÇÃO COM A VERSÃO IMEDIATAMENTE ANTERIOR
  arq_format <- SisMEVAZ_comparaVersoes(diretorio_de_dados = diretorio_de_dados,
                          versao_ref = paste0("Versao_",n_versao_salva) , versao_nov = "redeConsol")

  saveWorkbook(arq_format, paste0(dir_rede_conso, "/output/sintese/Comparacao Versao_",n_versao_salva+1," em relacao a Versao_",n_versao_salva,".xlsx"), overwrite = TRUE)

  # SÉRIES
  SisMEVAZ_print_Series(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  return("Atualização de CAVs finalizada")
}

#' Atualização temporal das séries
#'
#' Simulação da MEVAZ com a atilização temporal das séries, a partir da atualização da precipitação
#'  até o ano anterior ao corrente
#'
#'
#' @return "Atualização temporal das séries finalizada"
#' @import lubridate openxlsx phylin
#' @export
#'
SisMEVAZ_atualiz_Pluviometria_redeConsol <- function(diretorio_de_dados=paste0(getwd(),"/../../dados")){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_Plu_ETP    <- paste0(diretorio_de_dados,"/Plu_ETP")


  ## LEITURA DE DADOS QUE INDEPENDEM DA REDE DE RESERVATÓRIOS
  # Estações de referência
  EstPlu_ref <- readRDS(paste0(dir_dado_fixo,"/EstPlu_ref.rds"))

  # Grade de precipitações anterior
  Prec_gradeCE <- readRDS(paste0(dir_Plu_ETP,"/PLU_IDW_Mensal_Ceara_0.01graus.rds"))
  grade_Prec   <- data.frame(x=Prec_gradeCE$pontos$Long, y=Prec_gradeCE$pontos$Lat, id=1:nrow(Prec_gradeCE$pontos))
  Datas        <- names(Prec_gradeCE$P)
  ano_f0       <- year(tail(Datas,1))
  n_dts        <- length(Datas)

  # Evaporação de referência
  Evapm_ref <- read.xlsx(paste0(dir_Plu_ETP,"/EVAPORACAO_PICHE.xlsx"),colNames = T)

  # Dados dos reservatórios com regionalização do tipo Médias Regionais
  datMedReg <- read.xlsx(paste0(dir_dado_fixo,"/CE_CV_MedReg.xlsx"),colNames = T)

  # Parâmetros calibrados com vazões do balanço reverso (calibração local)
  Param_CalLocal  <- read.xlsx(paste0(dir_dado_fixo,"/Calibracao_com_balanco_reverso.xlsx"),colNames = T,rowNames = T)[,-1]


  ## LEITURA DE DADOS DA REDE CONSOLIDADA
  arqBac <- list.files(path=paste0(dir_rede_conso,"/input/shapes"),pattern = ".shp",full.names = T)
  idsBac <- gsub(pattern = ".shp",replacement = "",x = basename(arqBac))
  idsBac <- gsub(pattern = "graus_id_sagreh_",replacement = "", x = idsBac)
  idsBac <- idsBac[order(as.numeric(idsBac))]

  ETP_consol      <- readRDS(paste0(dir_rede_conso,"/input/ETP_Mensal_PM_Bacias_Incrementais.rds"))
  Pre_consol      <- readRDS(paste0(dir_rede_conso,"/input/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  dadosRes_consol <- read.xlsx(paste0(dir_rede_conso,"/input/Dados_Reservatorios.xlsx"),colNames = T)
  CAVs_consol     <- read.xlsx(paste0(dir_rede_conso,"/input/CAVs.xlsx"),colNames = T)
  Carac_consol    <- readRDS(paste0(dir_rede_conso,"/input/MatrizCarac_BacAcu.rds"))
  ParamKNN        <- readRDS(paste0(dir_rede_conso,"/output/Param_KNN.rds"))
  ParamML         <- readRDS(paste0(dir_rede_conso,"/output/Param_ML1.rds"))

  # DOWNLOAD DE ESTACÕES DE REFERÊNCIA
  ano_i    <- ano_f0+1
  ano_f_nv <- year(Sys.Date())-1

  if(ano_f_nv < ano_i){
    stop(paste0("A grade de precipitações já está atualizada até o ano de ",ano_f0,
                "! Atualizações são realizadas apenas com anos completos!"))
  }

  # ~40 min
  dados_plu <- Download_estacoesPlu(est_down=EstPlu_ref$NumeroPostos, ano_i=ano_i, ano_f=ano_f_nv)

  ## INTERPOLA PARA A GRADE
  # ~4 min
  Prec_gradeCE_tmp            <- FUNC_IDW(series_est=dados_plu$series, coord_est=dados_plu$lonlat[,c("long","lat")], grid= Prec_gradeCE$pontos)
  Prec_gradeCE_atualiz        <- Prec_gradeCE
  Prec_gradeCE_atualiz$P[(n_dts+1):(n_dts+length(Prec_gradeCE_tmp$P))] <- Prec_gradeCE_tmp$P

  Datas_atualiz                 <- c(names(Prec_gradeCE$P), names(Prec_gradeCE_tmp$P))
  names(Prec_gradeCE_atualiz$P) <- Datas_atualiz


  ## CALCULA PRECIPITAÇÃO MÉDIA NAS BACIAS (NO PERÍODO DE ATUALIZAÇÃO)
  # ~2 min
  Pre_tmp     <- FUNC_Precmed_internos(listPrec = Prec_gradeCE_tmp$P, pontos = grade_Prec, arqBac, idsBac)
  Pre_atualiz <- lapply(idsBac, function(id){c(Pre_consol[[id]],Pre_tmp[[id]])})
  names(Pre_atualiz) <- idsBac

  # Precipitação média (usada na geração de séries sintéticas)
  # OBS: parâmetros da regionalização ML não alterados, evitando modificação da série de vazões incrementais no período anterior à atualização
  Prec_anual <- Agg_anual(Lista_series = Pre_atualiz,func_agg = sum)
  Prec_med   <- sapply(Prec_anual,mean,na.rm=T)

  ## SIMULAÇÃO HIDROLÓGICA
  # Cálculo de afluências incrementais (Não recalcula-se parâmetros a partir da nova precipitação)
  vecArea         <- Carac_consol[,"Area_km2"]
  AfluInc_KNN <- SMAP.listaBac(matParam = ParamKNN, vecArea = vecArea,
                               lPrec = Pre_atualiz, lETo_med = ETP_consol,
                               ids = idsBac)

  AfluInc_ML  <- SMAP.listaBac(matParam = ParamML, vecArea = vecArea,
                               lPrec = Pre_atualiz, lETo_med = ETP_consol,
                               ids = idsBac)

  ids_bal_rev   <- dadosRes_consol$ID[dadosRes_consol$MODELO_REGIONALIZACAO=="Calibração Local (Balanço Reverso)"]

  if(length(ids_bal_rev)>0){
    Aflu_CalLocal <- SMAP.listaBac(matParam = Param_CalLocal, vecArea = vecArea,
                                   lPrec = Pre_atualiz, lETo_med = ETP_consol,
                                   ids = ids_bal_rev)
  }else{
    Aflu_CalLocal <- list()
  }


  # Verifica consistência dos reservatórios com regionalização simplificada (Modelos Regionais)
  ModRegion          <- dadosRes_consol$MODELO_REGIONALIZACAO # já atualizado
  names(ModRegion)   <- dadosRes_consol$ID
  RegHidro           <- dadosRes_consol$REG_HIDRO # já atualizado
  names(RegHidro)    <- dadosRes_consol$ID
  ids_MR <- names(ModRegion)[ModRegion=="Médias Regionais"]
  verif  <- all(sapply(ids_MR, function(x){is.element(x,datMedReg$ID)}))
  if(!verif){stop("Reservatório com regionalização do tipo Médias Regionais não está presente no arquivo CE_CV_MedReg.xlsx")}


  # Afluências incrmentais (seleção do modelo de regionalização)
  AfluInc_Selec <- FUNC_SelecRegion(AfluInc_KNN = AfluInc_KNN,
                                          AfluInc_ML = AfluInc_ML,
                                          Aflu_CalLocal = Aflu_CalLocal,
                                          ModRegion, RegHidro, Prec_med,
                                          Areas=vecArea, datMedReg)

  # ordem de afluencia entre reservatórios da rede-teste
  Ord_aflu <- dadosRes_consol[,c("ID","ID_ACJUS")]

  # Seleciona reservatórios finais das cascatas a serem simuladas
  resFimRede_selec <- FUNC_resFimRede_selec(Ord_aflu, res_mod=idsBac) # apenas os finais
  resRede_selec    <- FUNC_resRede_selec(Ord_aflu, resFimRede_selec)  # todos na rede

  # Serie de Evaporação nos reservatórios
  ResEst_selec <- dadosRes_consol[which(is.element(dadosRes_consol$ID, resRede_selec)),
                                 c("ID","COD_EST_EVAP")]
  EvapSerie <- MedMensalEst_para_SerieRes(Mat_MedMensal = Evapm_ref,
                                          ResEst = ResEst_selec,
                                          Datas = Datas_atualiz)

  # Inicialização dos volumes nos reservatórios
  vecVoliniperc        <- rep(0.5,length(resRede_selec))
  names(vecVoliniperc) <- resRede_selec


  # Simula as redes de reservatórios às quais pertencem os reservatórios novos na rede-teste
  ResSimul_KNN  <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                         vecGarantia = c(0.90, 0.95, 0.98),
                                                         AfluInc = AfluInc_KNN,
                                                         EvapSerie, Ord_aflu, DadosRes=dadosRes_consol,
                                                         DadosCAV=CAVs_consol, vecVoliniperc)

  ResSimul_ML   <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                         vecGarantia = c(0.90, 0.95, 0.98),
                                                         AfluInc = AfluInc_ML,
                                                         EvapSerie, Ord_aflu, DadosRes=dadosRes_consol,
                                                         DadosCAV=CAVs_consol, vecVoliniperc)

  ResSimul_Selec <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                          vecGarantia = c(0.90, 0.95, 0.98),
                                                          AfluInc = AfluInc_Selec,
                                                          EvapSerie, Ord_aflu, DadosRes=dadosRes_consol,
                                                          DadosCAV=CAVs_consol, vecVoliniperc)


  # Extrai vazão regulariza a partir da simlação (máxima valor operado na simulação)
  Qgaran_KNN   <- sapply(ResSimul_KNN, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_ML    <- sapply(ResSimul_ML, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_Selec <- sapply(ResSimul_Selec, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})

  # FAZ BACKUP DA GRADE DE PRECIPITAÇÃO (E DEMAIS DADOS DO DIRETÓRIO)
  versoes <- basename(list.dirs(paste0(dir_Plu_ETP,"/versoes_anteriores"),recursive = F))
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_prec_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_Plu_ETP,"/versoes_anteriores/versao_prec_",n_versao_salva))
  file.copy(from=paste0(dir_Plu_ETP,"/ETP_INMETselec_MensalMed_PenmanMonteith.rds"),
            to = paste0(dir_Plu_ETP,"/versoes_anteriores/versao_prec_",n_versao_salva),
            recursive = FALSE)
  file.copy(from=paste0(dir_Plu_ETP,"/EVAPORACAO_PICHE.xlsx"),
            to = paste0(dir_Plu_ETP,"/versoes_anteriores/versao_prec_",n_versao_salva),
            recursive = FALSE)
  file.copy(from=paste0(dir_Plu_ETP,"/PLU_IDW_Mensal_Ceara_0.01graus.rds"),
            to = paste0(dir_Plu_ETP,"/versoes_anteriores/versao_prec_",n_versao_salva),
            recursive = FALSE)

  # ATUALIZA A PRECIPITAÇÃO EM GRADE
  saveRDS(Prec_gradeCE_atualiz,paste0(dir_Plu_ETP,"/PLU_IDW_Mensal_Ceara_0.01graus.rds"))

  # ATUALIZA LOG PLU_ETP
  logfile <- list.files(paste0(dir_Plu_ETP,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_Plu_ETP,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_Plu_ETP,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão precipitação ",n_versao_salva," --> versão precipitação atual: Atualização temporal da precipitação de 1911-",ano_f0," para 1911-",ano_f_nv),
        file = logfile,append = T)


  # FAZ BACKUP DA REDE CONSOLIDADA
  versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
  versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva))
  file.copy(from=paste0(dir_rede_conso,"/input"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)
  file.copy(from=paste0(dir_rede_conso,"/output"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)



  # ATUALIZA ARQUIVOS PARA NOVA REDE CONSOLIDADA (Input e Output pré-simulação da rede)
  # Input
  file.remove(paste0(dir_rede_conso,"/input/AfluenciasIncrementais_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/input/AfluenciasIncrementais_ML1.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Afluencias_CalLocal.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_ML.rds"))
  file.remove(paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))

  # Output
  file.remove(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_ML1.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Afluencias_CalLocal.rds"))
  file.remove(paste0(dir_rede_conso,"/output/AfluenciasIncrementais_Selec.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_ML.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_Selec.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_KNN.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_ML.rds"))
  file.remove(paste0(dir_rede_conso,"/output/Qgaran_Selec.rds"))
  unlink(paste0(dir_rede_conso,"/output/sintese/*"),recursive=T)
  unlink(paste0(dir_rede_conso,"/output/series/aflu_incr/*"),recursive=T)
  unlink(paste0(dir_rede_conso,"/output/series/aflu_tot/*"),recursive=T)

  ## SALVA ARQUIVOS ATUALIZADOS
  # Input
  saveRDS(AfluInc_KNN,   paste0(dir_rede_conso,"/input/AfluenciasIncrementais_KNN.rds"))
  saveRDS(AfluInc_ML,    paste0(dir_rede_conso,"/input/AfluenciasIncrementais_ML1.rds"))
  saveRDS(Aflu_CalLocal, paste0(dir_rede_conso,"/input/Afluencias_CalLocal.rds"))
  saveRDS(Pre_atualiz,   paste0(dir_rede_conso,"/input/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  saveRDS(ResSimul_KNN  , paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_KNN.rds"))
  saveRDS(ResSimul_ML   , paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_ML.rds"))
  saveRDS(ResSimul_Selec, paste0(dir_rede_conso,"/input/Resultados_SimulacaoRede_Selec.rds"))

  # Output
  saveRDS(AfluInc_KNN,   paste0(dir_rede_conso,"/output/AfluenciasIncrementais_KNN.rds"))
  saveRDS(AfluInc_ML,    paste0(dir_rede_conso,"/output/AfluenciasIncrementais_ML1.rds"))
  saveRDS(Aflu_CalLocal, paste0(dir_rede_conso,"/output/Afluencias_CalLocal.rds"))
  saveRDS(AfluInc_Selec, paste0(dir_rede_conso,"/output/AfluenciasIncrementais_Selec.rds"))
  saveRDS(Pre_atualiz,   paste0(dir_rede_conso,"/output/Precipitacao_Mensal_Bacias_Incrementais.rds"))
  saveRDS(ResSimul_KNN,  paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_KNN.rds"))
  saveRDS(ResSimul_ML,   paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_ML.rds"))
  saveRDS(ResSimul_Selec,paste0(dir_rede_conso,"/output/Resultados_SimulacaoRede_Selec.rds"))
  saveRDS(Qgaran_KNN,   paste0(dir_rede_conso,"/output/Qgaran_KNN.rds"))
  saveRDS(Qgaran_ML,    paste0(dir_rede_conso,"/output/Qgaran_ML.rds"))
  saveRDS(Qgaran_Selec, paste0(dir_rede_conso,"/output/Qgaran_Selec.rds"))


  # ATUALIZA LOG
  logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_rede_conso,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão ",n_versao_salva," --> versão atual: Atualização temporal da precipitação de 1911-",ano_f0," para 1911-",ano_f_nv),
        file = logfile,append = T)

  # SINTESE
  SisMEVAZ_print_Sintese(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  # COMPARAÇÃO COM A VERSÃO IMEDIATAMENTE ANTERIOR
  arq_format <- SisMEVAZ_comparaVersoes(diretorio_de_dados = diretorio_de_dados,
                                        versao_ref = paste0("Versao_",n_versao_salva), versao_nov = "redeConsol")

  saveWorkbook(arq_format, paste0(dir_rede_conso, "/output/sintese/Comparacao Versao_",n_versao_salva+1," em relacao a Versao_",n_versao_salva,".xlsx"), overwrite = TRUE)

  # SÉRIES
  SisMEVAZ_print_Series(diretorio_de_dados = diretorio_de_dados, versao_print = "redeConsol")

  return("Atualização da Pluviomatria finalizada")

}


####################
# REVISAR POIS FUNÇÃO COM ERRO --> olhar em especial o caso de diminuição de reservatórios no caso novo
##########################

#' Compara duas versões
#'
#' Calcula as mudanças de variáveis-chave da MEVAZ
#' (precipitação, capacidade, afluência, Q90, Q95 e Q98) entre duas versões
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param versao_nov Caractere com a versão a ser analisada ("redeConsol", "redeTeste", "Versao_i" ou "Teste_i" com i inteiro>=0)
#' @param versao_ref Caractere com a versão de referência ("redeConsol", "redeTeste", "Versao_i" ou "Teste_i" com i inteiro>=0)
#'
#' @return arquivo de comparação formatado
#' @import openxlsx
#' @export
#'
#'
SisMEVAZ_comparaVersoes<-function(diretorio_de_dados=paste0(getwd(),"/../../dados"), versao_ref="redeConsol", versao_nov="redeTeste", savexlsx=F){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")

  if(versao_ref=="redeConsol"){
    dir_versao_ref <- dir_rede_conso
  }else if(versao_ref=="redeTeste"){
    dir_versao_ref <- dir_rede_teste
  }else if(grepl("Versao_", versao_ref)){
    vv <-  gsub(pattern = "Versao_", replacement = "", x = versao_ref)
    dir_versao_ref <- paste0(dir_rede_conso,"/versoes_anteriores/versao_",vv)
  }else if(grepl("Teste_", versao_ref)){
    tt <-  gsub(pattern = "Teste_", replacement = "", x = versao_ref)
    dir_versao_ref <- paste0(diretorio_de_dados,"/Testes_outros/teste_",tt)
  }else{
    stop("Erro na nomenclatura da comparação!")
  }

  if(versao_nov=="redeConsol"){
    dir_versao_nov <- dir_rede_conso
  }else if(versao_nov=="redeTeste"){
    dir_versao_nov <- dir_rede_teste
  }else if(grepl("Versao_", versao_nov)){
    vv <-  gsub(pattern = "Versao_", replacement = "", x = versao_nov)
    dir_versao_nov <- paste0(dir_rede_conso,"/versoes_anteriores/versao_",vv)
  }else if(grepl("Teste_", versao_nov)){
    tt <-  gsub(pattern = "Teste_", replacement = "", x = versao_nov)
    dir_versao_nov <- paste0(diretorio_de_dados,"/Testes_outros/teste_",tt)
  }else{
    stop("Erro na nomenclatura da comparação!")
  }

  ## Lê arquivos de síntese
  sintese_ref <- read.xlsx(paste0(dir_versao_ref,"/output/sintese/Sintese.xlsx"),sheet = 3)[-(1:3),]
  sintese_nov <- read.xlsx(paste0(dir_versao_nov,"/output/sintese/Sintese.xlsx"),sheet = 3)[-(1:3),]

   ## Seleciona colunas
  identif_ref <- sintese_ref[,1:3]
  identif_nov <- sintese_nov[,1:3]
  variav_ref  <- sintese_ref[,c(7,15,20,23,33,43)]
  variav_nov  <- sintese_nov[,c(7,15,20,23,33,43)]


  alinha_regiao <- function(reg){

    ## referência
    ref_id  <- identif_ref[identif_ref[,3]==reg & !is.na(identif_ref[,1]),]
    ref_var <- variav_ref[identif_ref[,3]==reg & !is.na(identif_ref[,1]),]

    ## nova
    nov_id  <- identif_nov[identif_nov[,3]==reg & !is.na(identif_nov[,1]),]
    nov_var <- variav_nov[identif_nov[,3]==reg & !is.na(identif_nov[,1]),]

    ## cadastro completo
    cadastro <- unique(rbind(ref_id,nov_id))

    cadastro <- cadastro[order(cadastro[,2]),]

    ids <- cadastro[,1]

    ## procura posição em cada versão
    pos_ref <- match(ids,ref_id[,1])
    pos_nov <- match(ids,nov_id[,1])

    ref_var2 <- ref_var[pos_ref,,drop=FALSE]
    nov_var2 <- nov_var[pos_nov,,drop=FALSE]

    ## linha agregada
    agg_ref_id  <- identif_ref[identif_ref[,3]==reg & is.na(identif_ref[,1]),]
    agg_ref_var <- variav_ref[identif_ref[,3]==reg & is.na(identif_ref[,1]),]

    agg_nov_var <- variav_nov[identif_nov[,3]==reg & is.na(identif_nov[,1]),]

    ## usa o agregado da versão nova (ou da antiga se preferir)
    cadastro <- rbind(cadastro,agg_ref_id)

    ref_var2 <- rbind(ref_var2,agg_ref_var)
    nov_var2 <- rbind(nov_var2,agg_nov_var)

    list(
      ident=cadastro,
      ref=ref_var2,
      nov=nov_var2
    )

  }



  regioes <- unique(na.omit(identif_nov[,3]))

  identif <- NULL
  variav_ref2 <- NULL
  variav_nov2 <- NULL

  for(reg in regioes){

    tmp <- alinha_regiao(reg)

    identif     <- rbind(identif,tmp$ident)
    variav_ref2 <- rbind(variav_ref2,tmp$ref)
    variav_nov2 <- rbind(variav_nov2,tmp$nov)

  }

  variav_ref <- variav_ref2
  variav_nov <- variav_nov2

  variav_nov <- matrix(as.numeric(as.matrix(variav_nov)),
                       nrow=nrow(variav_nov))

  variav_ref <- matrix(as.numeric(as.matrix(variav_ref)),
                       nrow=nrow(variav_ref))

  variacao <- (variav_nov-variav_ref)/variav_ref


  status <- rep("Mantido", nrow(identif))

  status[is.na(variav_ref[,1]) & !is.na(variav_nov[,1])] <- "Novo"

  status[!is.na(variav_ref[,1]) & is.na(variav_nov[,1])] <- "Removido"

  dados <- cbind(
    identif,
    Status = status,
    variav_ref,
    variav_nov,
    variacao
  )



  # DEFINIÇÂO DE ESTILOS
  # Estilo basico
  estilo_base <- createStyle(fontName = "Calibre", fontSize = 11,
                             fgFill = NULL, numFmt = "0.00")
  estilo_prec <- createStyle(fontName = "Calibre", fontSize = 11, numFmt = "0.0%")

  # Estilo de linhas com valores agregados por Região Hidrográfica
  estilo_agg <- createStyle(fontName = "Calibre", fontSize = 11,
                            textDecoration = "bold", fgFill = "yellow", numFmt = "0.0")
  estilo_agg_prec <- createStyle(fontName = "Calibre", fontSize = 11,
                                 textDecoration = "bold", fgFill = "yellow", numFmt = "0.0%")

  # SALVA DADOS SOBRE O ARQUIVO DE FORMATAÇÃO
  # Lê arquivo com formatação
  arq_format <- openxlsx::loadWorkbook(paste0(dir_dado_fixo,"/Arquivo_Comparacao_Formatacao.xlsx"))

  # Escrever os dados começando na linha 5 e na coluna 1
  sheet_name <- arq_format$sheet_names[1]
  writeData(arq_format, sheet = sheet_name, x = dados, startRow = 5, startCol = 1, colNames = FALSE)

  # Estilo básico
  addStyle(arq_format, sheet = sheet_name, style = estilo_base, rows = 5:(nrow(dados)+4), cols = 1:16, gridExpand = T)
  addStyle(arq_format, sheet = sheet_name, style = estilo_prec, rows = 5:(nrow(dados)+4), cols = 17:22, gridExpand = T)
  # Adiciona estilo de linhas com valores agregados por Região Hidrográfica
  lnhs_agg <- which(is.na(dados[,1]))+4
  addStyle(arq_format, sheet = sheet_name, style = estilo_agg,      rows = lnhs_agg, cols = 1:16, gridExpand = T)
  addStyle(arq_format, sheet = sheet_name, style = estilo_agg_prec, rows = lnhs_agg, cols = 17:22, gridExpand = T)

  if(savexlsx){

    # Versão consolidada é nomeada na comparação pelo número identificador da versão
    versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
    versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
    if(length(versoes)==0){
      n_versao_consol <- 1
    }else{
      n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
      n_versao_consol <- max(n_vers)+1
    }

    versao_nov <- ifelse(versao_nov=="redeConsol",paste0("Versao_",n_versao_consol),versao_nov)
    versao_ref <- ifelse(versao_ref=="redeConsol",paste0("Versao_",n_versao_consol),versao_ref)

    # Salvar o workbook
    print("Arquivo salvo no diretorio_de_dados")
    saveWorkbook(arq_format, paste0(diretorio_de_dados, "/Comparacao ",versao_nov," em relacao a ",versao_ref,".xlsx"), overwrite = TRUE)

  }
  return(arq_format)

}

#' Restaura versão da rede consolidada
#'
#' Restaura versão anterior da rede consolidada, substituindo os arquivos no
#' diretório "diretorio_de_dados/RededeReservatorios_consolidada"
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param versao              Inteiro maior ou igual a 1 indicando a versao a ser restaurada
#'
#' @details Salva a veãrso anterior da rede consolidada no diretório
#' "diretorio_de_dados/RededeReservatorios_consolidada/versoes_anteriores" e atualiza o log
#'
#' @return "Restauração finalizada"
#' @export
#'
#'
SisMEVAZ_restaura_versao <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"), versao){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")
  dir_ver_restau <- paste0(dir_rede_conso,"/versoes_anteriores/versao_",versao)
  if(versao == 0){stop("Só é possível restaurar a partir da versão 1")}

  # FAZ BACKUP DA REDE CONSOLIDADA
  versoes <- basename(list.dirs(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F))
  versoes <- versoes[-1] # desconsidera versão 0 (anterior a estruturação do Sis-MEVAZ)
  if(length(versoes)==0){
    n_versao_salva <- 1
  }else{
    n_vers <- as.numeric(gsub(pattern = "versao_",replacement = "",x = versoes))
    n_versao_salva <- max(n_vers)+1
  }
  dir.create(paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva))
  file.copy(from=paste0(dir_rede_conso,"/input"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)
  file.copy(from=paste0(dir_rede_conso,"/output"),
            to = paste0(dir_rede_conso,"/versoes_anteriores/versao_",n_versao_salva),
            recursive = TRUE)

  # RESTAURA REDE CONSOLIDADA A PARTIR DE VERSÃO ANTERIOR
  # Diretório Input
  file.copy(from=paste0(dir_ver_restau,"/input"),
            to = paste0(dir_rede_conso),
            recursive = T)

  # Diretório Output
  file.copy(from=paste0(dir_ver_restau,"/output"),
            to = paste0(dir_rede_conso),
            recursive = T)

  # ATUALIZA LOG
  logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(dir_rede_conso,"/versoes_anteriores/log.txt"))
    logfile <- list.files(paste0(dir_rede_conso,"/versoes_anteriores"),recursive = F,pattern = "log",full.names = T)
  }else{ # Se log já existir
    # Ler o conteúdo do arquivo em um vetor de linhas
    log_lines <- readLines(logfile, warn = FALSE)
    last_line <- log_lines[length(log_lines)]
    # Substitui "versão atual"
    last_line_mod <- gsub(pattern = "atual", replacement = n_versao_salva, last_line)
    # Modifica a ultima linha
    log_lines[length(log_lines)] <- last_line_mod

    # Escrever de volta para o arquivo
    writeLines(log_lines, logfile)
  }
  write(x=paste0(Sys.time(),": versão ",n_versao_salva," --> versão atual: Restauração para a versão ",versao), file = logfile,append = T)

  return("Restauração finalizada")

}


#' Outros testes
#'
#' Realiza outros testes sobre uma versão específica, tais como: diferentes períodos de simulação da cascata,
#' diferentes inicializações dos reservatórios, otimização e simulações com garantias anuais
#'
#' @param diretorio_de_dados  Caractere com o caminho do pacote
#' @param versao_ref Caractere com a versão de referência ("redeConsol", "redeTeste", "Versao_i" ou "Teste_i" com i inteiro>=0)
#'
#' @details Salva a veãrso anterior da rede consolidada no diretório
#' "diretorio_de_dados/RededeReservatorios_consolidada/versoes_anteriores" e atualiza o log
#'
#' @return "Restauração finalizada"
#' @export
#'
#'
SisMEVAZ_outros_testes <- function(diretorio_de_dados=paste0(getwd(),"/../../dados"),
                                   versao_ref="redeConsol",
                                   ano_ini = 1911, ano_fin = year(Sys.time())-1,
                                   vol_ini= 0.5, garantia_anual= F){

  ## DIRETÓRIOS DE REFERÊNCIA
  dir_rede_teste <- paste0(diretorio_de_dados,"/RededeReservatorios_em_teste")
  dir_rede_conso <- paste0(diretorio_de_dados,"/RededeReservatorios_consolidada")
  dir_dado_fixo  <- paste0(diretorio_de_dados,"/Dados_fixos")
  dir_Plu_ETP    <- paste0(diretorio_de_dados,"/Plu_ETP")

  if(versao_ref=="redeConsol"){
    dir_rede_ref <- dir_rede_conso
  }else if(versao_ref=="redeTeste"){
    dir_rede_ref <- dir_rede_teste
  }else if(grepl("Versao_", versao_ref)){
    vv <-  gsub(pattern = "Versao_", replacement = "", x = versao_ref)
    dir_rede_ref <- paste0(dir_rede_conso,"/versoes_anteriores/versao_",vv)
  }else{
    tt <-  gsub(pattern = "Teste_", replacement = "", x = versao_ref)
    dir_rede_ref <- paste0(diretorio_de_dados,"/Testes_outros/teste_",tt)
  }


  # DIRETÓRIO DO TESTE
  testes <- basename(list.dirs(paste0(diretorio_de_dados,"/Testes_outros"), recursive = F))
  if(length(testes)==0){
    n_teste_nv <- 1
  }else{
    n_tes <- as.numeric(gsub(pattern = "teste_",replacement = "",x = testes))
    n_teste_nv <- max(n_tes)+1
  }
  dir_teste_nv <- paste0(diretorio_de_dados,"/Testes_outros/teste_",n_teste_nv)
  dir.create(dir_teste_nv)
  dir_teste_nv_out <- paste0(dir_teste_nv,"/output")
  dir.create(dir_teste_nv_out)

  ## DADOS DA REDE DE REFERÊNCIA
  dadosRes <- read.xlsx(paste0(dir_rede_ref,"/output/Dados_Reservatorios.xlsx"),colNames = T)
  CAVs     <- read.xlsx(paste0(dir_rede_ref,"/output/CAVs.xlsx"),colNames = T)
  id_res   <- dadosRes$ID

  # Evaporação de referência
  Evapm_ref <- read.xlsx(paste0(dir_Plu_ETP,"/EVAPORACAO_PICHE.xlsx"),colNames = T)

  # Afluências incrementais
  AfluInc_KNN    <- readRDS(paste0(dir_rede_ref,"/output/AfluenciasIncrementais_KNN.rds"))
  AfluInc_ML     <- readRDS(paste0(dir_rede_ref,"/output/AfluenciasIncrementais_ML1.rds"))
  AfluInc_Selec  <- readRDS(paste0(dir_rede_ref,"/output/AfluenciasIncrementais_Selec.rds"))

  # Recorta para perídos correspondentes
  Datas <- names(AfluInc_Selec$`1`)
  Datas <- Datas[year(Datas)>=ano_ini & year(Datas)<=ano_fin]
  AfluInc_KNN_teste   <- lapply(AfluInc_KNN, function(ser){ser[Datas]})
  AfluInc_ML_teste    <- lapply(AfluInc_ML, function(ser){ser[Datas]})
  AfluInc_Selec_teste <- lapply(AfluInc_Selec, function(ser){ser[Datas]})

  # SIMULAÇÃO HIDROLÓGICA PARA O TESTE
  # ordem de afluencia entre reservatórios da rede-teste
  Ord_aflu <- dadosRes[,c("ID","ID_ACJUS")]

  # Seleciona reservatórios finais das cascatas a serem simuladas
  resFimRede_selec <- FUNC_resFimRede_selec(Ord_aflu, res_mod=id_res) # apenas os finais

  # Serie de Evaporação nos reservatórios
  ResEst_selec <- dadosRes[,c("ID","COD_EST_EVAP")]
  EvapSerie <- MedMensalEst_para_SerieRes(Mat_MedMensal = Evapm_ref,
                                          ResEst = ResEst_selec,
                                          Datas = Datas)

  # Inicialização dos volumes nos reservatórios
  vecVoliniperc        <- rep(vol_ini,length(id_res))
  names(vecVoliniperc) <- id_res


  # Simula as redes de reservatórios às quais pertencem os reservatórios novos na rede-teste
  ResSimul_KNN_teste  <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                         vecGarantia = c(0.90, 0.95, 0.98),
                                                         AfluInc = AfluInc_KNN_teste,
                                                         EvapSerie, Ord_aflu, DadosRes=dadosRes,
                                                         DadosCAV=CAVs, vecVoliniperc, garantia_anual=garantia_anual)

  ResSimul_ML_teste   <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                         vecGarantia = c(0.90, 0.95, 0.98),
                                                         AfluInc = AfluInc_ML_teste,
                                                         EvapSerie, Ord_aflu, DadosRes=dadosRes,
                                                         DadosCAV=CAVs, vecVoliniperc, garantia_anual=garantia_anual)

  ResSimul_Selec_teste <- MultiplReservoirNetworkSimulation(vec_ID_final = resFimRede_selec,
                                                          vecGarantia = c(0.90, 0.95, 0.98),
                                                          AfluInc = AfluInc_Selec_teste,
                                                          EvapSerie, Ord_aflu, DadosRes=dadosRes,
                                                          DadosCAV=CAVs, vecVoliniperc, garantia_anual=garantia_anual)

  # Nomea casos de simulação
  Qgaran_nms <- c("Q90","Q95","Q98")
  names(ResSimul_KNN_teste)   <- Qgaran_nms
  names(ResSimul_ML_teste)    <- Qgaran_nms
  names(ResSimul_Selec_teste) <- Qgaran_nms

  # Extrai vazão regulariza a partir da simlação (máxima valor operado na simulação)
  Qgaran_KNN   <- sapply(ResSimul_KNN_teste, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_ML    <- sapply(ResSimul_ML_teste, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})
  Qgaran_Selec <- sapply(ResSimul_Selec_teste, function(LSimul){sapply(LSimul, function(Simul){max(Simul$Ret_m3s)})})


  ## COPIA DA REFERÊNCIA
  file.copy(from=paste0(dir_rede_ref,"/output/MatrizCarac_BacAcu.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/ETP_Mensal_PM_Bacias_Incrementais.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/Precipitacao_Mensal_Bacias_Incrementais.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/KNNest_prox.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/Param_KNN.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/Param_ML1.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/AfluenciasIncrementais_KNN.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/AfluenciasIncrementais_ML1.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/AfluenciasIncrementais_Selec.rds"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/Dados_Reservatorios.xlsx"),to = paste0(dir_teste_nv_out))
  file.copy(from=paste0(dir_rede_ref,"/output/CAVs.xlsx"),to = paste0(dir_teste_nv_out))

  ## SALVA ARQUIVOS DO TESTE
  saveRDS(ResSimul_KNN_teste,paste0(dir_teste_nv_out,"/Resultados_SimulacaoRede_KNN.rds"))
  saveRDS(ResSimul_ML_teste ,paste0(dir_teste_nv_out,"/Resultados_SimulacaoRede_ML.rds"))
  saveRDS(ResSimul_Selec_teste ,paste0(dir_teste_nv_out,"/Resultados_SimulacaoRede_Selec.rds"))
  saveRDS(Qgaran_KNN,paste0(dir_teste_nv_out,"/Qgaran_KNN.rds"))
  saveRDS(Qgaran_ML ,paste0(dir_teste_nv_out,"/Qgaran_ML.rds"))
  saveRDS(Qgaran_Selec ,paste0(dir_teste_nv_out,"/Qgaran_Selec.rds"))

  # ATUALIZA LOG
  logfile <- list.files(paste0(diretorio_de_dados,"/Testes_outros"),recursive = F,pattern = "log_testes",full.names = T)
  if(length(logfile)==0){
    file.create(paste0(diretorio_de_dados,"/Testes_outros/log_testes.txt"))
    logfile <- list.files(paste0(diretorio_de_dados,"/Testes_outros"),recursive = F,pattern = "log_testes",full.names = T)
  }
  write(x=paste0(Sys.time(),": Teste ",n_teste_nv," a partir de ",basename(dir_rede_ref),
                 ": (1) Período ",ano_ini," - ",ano_fin,
                 ", (2) Volume inicial ",round(vol_ini*100, 1),"%, (3) Garantia em frequencia ", ifelse(garantia_anual, "anual","mensal")),
        file = logfile,append = T)


  # SÍNTESE
  dir.create(paste0(dir_teste_nv,"/output/sintese"))
  SisMEVAZ_print_Sintese(diretorio_de_dados = diretorio_de_dados, versao_print = paste0("Teste_",n_teste_nv))

  #COMPARAÇÃO COM A VERSÃO IMEDIATAMENTE ANTERIOR

  arq_format <- SisMEVAZ_comparaVersoes(diretorio_de_dados = diretorio_de_dados,
                                        versao_ref = versao_ref, versao_nov = paste0("Teste_",n_teste_nv))

  saveWorkbook(arq_format, paste0(dir_teste_nv, "/output/sintese/Comparacao Teste_",n_teste_nv," em relacao a ",versao_ref,".xlsx"), overwrite = TRUE)

  return("Simlação da teste especial finalizada")

}


