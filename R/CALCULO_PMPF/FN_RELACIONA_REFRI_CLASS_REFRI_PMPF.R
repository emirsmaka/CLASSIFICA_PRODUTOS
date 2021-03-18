###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: RELACIONA REFRIGERANTES CLASSIFICADOS COM TABELA DE         ##
##               REFRIGERANTES DO PMPF                                       ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

fn_rel_class_pmpf <- function(tb_bebida){
  ### AGRUPAR POR PROD_XPROD, XPROD_SEFAZ, CEANTRIB, VOLUME_SEFAZ
  tb_bebida_gtin <- df_bebidas_NFE%>%
    group_by(PROD_CPROD_SEFAZ_AJUSTADO,PROD_XPROD_SEFAZ_AJUSTADO,PROD_CPROD_SEFAZ_DETALHADO,
             PROD_XPROD_SEFAZ_DETALHADO,PROD_CEANTRIB,PROD_VOLUME_SEFAZ_AJUSTADO,
             PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO,PROD_EMBALAGEM_AJUSTADO)%>%
    summarise(TOTAL = n())%>%
    arrange(desc(TOTAL))
  
  #### FUNCAO ENCONTRAR MAIOR FREQUENCIA POR GTIN
  max_total <- function(x){
    return(max(x))
  }
  lista_refri <- list()
  lista_refri <- split(x=tb_bebida_gtin,f=tb_bebida_gtin$PROD_CEANTRIB)
  maior_freq <- lapply(lista_refri, function(x) max_total(x$TOTAL))
  refri_maior_freq <- ldply(maior_freq,data.frame)
  refri_maior_freq <-  rename(refri_maior_freq,PROD_CEANTRIB = '.id',TOTAL = 'X..i..')
  refri_maior_freq$PROD_CEANTRIB <- as.double(refri_maior_freq$PROD_CEANTRIB)
  
  tb_bebida_gtin <- inner_join(refri_maior_freq,tb_bebida_gtin,by=c("PROD_CEANTRIB","TOTAL"))
  return(tb_bebida_gtin)
}
