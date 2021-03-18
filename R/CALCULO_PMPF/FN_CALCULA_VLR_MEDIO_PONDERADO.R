###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: CALCULA PMPF                                                ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

fn_media_ponderada <- function(tb_produto){
  ### CALCULA QTE TOTAL VENDIDA E VLR TOTAL VENDIDO POR CONTRIBUINTE/MES/PRODUTO
  tb_bebida <- tb_produto%>%
    group_by(EMIT_CNPJ_CPF,IDE_DHEMI_PERIODO,PROD_CEANTRIB)%>%
    mutate(VLR_MEDIO_CONTRIB_MES = mean(PROD_VUNCOM))%>%
    mutate(QTE_TOTAL_CONTRIB_MES = sum(PROD_QCOM))%>%
    mutate(VLR_VENDA_CONTRIB = VLR_MEDIO_CONTRIB_MES*QTE_TOTAL_CONTRIB_MES)
  
  ### CALCULA A MEDIA PONDERADA, TOTAL_VENDA,VLR_MAXIMO E VLR_MINIMO POR PRODUTO/MES
  tb_bebida <- tb_bebida%>%
    group_by(IDE_DHEMI_PERIODO,PROD_CEANTRIB)%>%
    mutate(VLR_MEDIO_PONDERADO_MES = sum(VLR_VENDA_CONTRIB)/sum(QTE_TOTAL_CONTRIB_MES))%>%
    mutate(VLR_MINIMO = min(PROD_VUNCOM))%>%
    mutate(VLR_MAXIMO = max(PROD_VUNCOM))%>%
    mutate(QTE_TOTAL = sum(PROD_QCOM))%>%
    mutate(VLR_MEDIO_ARITM_MES = sum(PROD_VPROD)/sum(PROD_QCOM))
  
  tb_bebida_media_ponderada <- tb_bebida%>%
    select(IDE_DHEMI_PERIODO,PROD_CEANTRIB,PROD_XPROD,VLR_MEDIO_PONDERADO_MES,VLR_MEDIO_ARITM_MES,VLR_MAXIMO,
           VLR_MINIMO,QTE_TOTAL)%>%
    distinct(IDE_DHEMI_PERIODO,PROD_CEANTRIB,.keep_all = T)
  
  tb_bebida_media_ponderada <- tb_bebida_media_ponderada%>%
    group_by(PROD_CEANTRIB)%>%
    mutate(VLR_MEDIO_PONDERADO_TRIM = mean(VLR_MEDIO_PONDERADO_MES))%>%
    mutate(VLR_MEDIO_ARITM_TRIM = mean(VLR_MEDIO_ARITM_MES))
  return(tb_bebida_media_ponderada)
}
