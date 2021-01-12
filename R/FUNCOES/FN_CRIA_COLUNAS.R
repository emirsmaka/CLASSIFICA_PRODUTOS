fn_cria_colunas <- function(x){
  ############# CRIA COLUNAS EXTRAS E DE APOIO#############
  x[,"QTE_TRIB_AJUSTADO"] <- as.character()
  x[,"VOLUME_TRIB_AJUSTADO"] <- as.character()
  x[,"VUNTRIB_AJUSTADO"] <- as.double()
  x[,"FATOR_MULTIPLICADOR"] <- as.double()
  x[,"UNIDADE_SEFAZ"] <- "UN"
  x[,"VOLUME_SEFAZ"] <- as.double()
  x[,"UN_MEDIDA_SEFAZ"] <- as.character()
  x[,"VLR_UNITARIO_SEFAZ"] <- as.double()
  x[,"QTE_SEFAZ"] <- as.double()
  ########################################################
  return(x)
}
