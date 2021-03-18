###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: RETIRA OULIERS                                              ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

#### RETIRA OUTLIERS
fn_outliers <- function(tb_bebida){
  source("./SCRIPTS/FUNCOES/FN_OUT_BEBIDAS.R")
  
  ls_out <- list()
  ls_out <- split(x=tb_bebida,f=tb_bebida$PROD_CEANTRIB)
  
  ls_outlier <- lapply(ls_out,function(x) fn_out_bebida(x))
  tb_outlier <- ldply(ls_outlier,data.frame)
  
  df_out_true <- tb_outlier%>%
    filter(OUT == TRUE)
  x <- anti_join(tb_outlier,df_out_true,by="OUT")
  
}
#################################################