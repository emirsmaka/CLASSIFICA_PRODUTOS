fn_cria_colunas <- function(x){
  ############# CRIA COLUNAS EXTRAS E DE APOIO#############
  df_cerveja[,"QTE_TRIB_AJUSTADO"] <- as.character()
  df_cerveja[,"VOLUME_TRIB_AJUSTADO"] <- as.character()
  df_cerveja[,"VUNTRIB_AJUSTADO"] <- as.double()
  df_cerveja[,"FATOR_MULTIPLICADOR"] <- as.double()
  df_cerveja[,"UNIDADE_SEFAZ"] <- "UN"
  df_cerveja[,"VOLUME_SEFAZ"] <- as.double()
  df_cerveja[,"UN_MEDIDA_SEFAZ"] <- as.character()
  df_cerveja[,"VLR_UNITARIO_SEFAZ"] <- as.double()
  df_cerveja[,"QTE_SEFAZ"] <- as.double()
  ########################################################
  return(x)
}
