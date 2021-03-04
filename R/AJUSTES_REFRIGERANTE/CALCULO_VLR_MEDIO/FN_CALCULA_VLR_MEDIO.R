###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: CALCULA PMPF                                                ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

#### CALCULA PRECO MEDIO
fn_preco_medio <- function(tb_bebida){
  source("./SCRIPTS/FUNCOES/FN_PMPF_BEBIDAS.R")
  ls_pmpf <- list()
  tb_bebida <- as.data.table(tb_bebida)
  ls_pmpf <- split(tb_bebida,by=c("IDE_DHEMI_PERIODO","PROD_CEANTRIB"))
  ls_pmpf <- lapply(ls_pmpf,function(x) fn_pmpf_bebidas(x))
  tb_pmpf_nfce <- ldply(ls_pmpf,data.frame)
  
  df_pmpf_nfce <- tb_pmpf_nfce%>%
    distinct(IDE_DHEMI_PERIODO,PROD_CEANTRIB,VLR_PMPF_CALCULADO_NFCE)
  
  #### RELACIONA PRODUTOS COM PMPF CALCULADO PELA NFCE COM TABELA DO SISTEMA PMPF
  df_pmpf_nfce <- rename(df_pmpf_nfce, CEANTRIB_NFCE = 'PROD_CEANTRIB')
  tb_pmpf_nfce <- inner_join(df_pmpf_nfce,df_pmpf_bebidas,by="CEANTRIB_NFCE")
  
  tb_pmpf_nfce <- tb_pmpf_nfce%>%
    select(IDE_DHEMI_PERIODO, CEANTRIB_NFCE,CODIGO_PMPF,DESCRICAO_PROD_PMPF,VOLUME_PMPF,UNID_MEDIDA_PMPF
           ,VLR_PMPF_CALCULADO_NFCE)
  
  tb_valor_medio_bebidas <- tb_pmpf_nfce%>%
    group_by(CEANTRIB_NFCE)%>%
    mutate(VLR_MEDIO_TRIMESTRAL = mean(VLR_PMPF_CALCULADO_NFCE))
  return(tb_valor_medio_bebidas)
}

####################################################################################


