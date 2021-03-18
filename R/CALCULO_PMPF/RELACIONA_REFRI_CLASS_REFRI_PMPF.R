###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: RELACIONA REFRIGERANTES CLASSIFICADOS COM TABELA DE         ##
##               REFRIGERANTES DO PMPF                                       ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

df_pmpf_bebidas <- rename(df_pmpf_bebidas,PROD_CEANTRIB = 'CEANTRIB_NFCE')
df_refri_NFE$PROD_CEANTRIB <- as.character(df_refri_NFE$PROD_CEANTRIB)
df_refri_class_refri_pmpf <-  inner_join(df_refri_NFE,df_pmpf_bebidas,by="PROD_CEANTRIB")
df_refri_class_refri_pmpf$PROD_CPROD_SEFAZ_DETALHADO <- as.character(df_refri_class_refri_pmpf$PROD_CPROD_SEFAZ_DETALHADO)
df_refri_class_refri_pmpf$IDNFE <- NULL
df_refri_class_refri_pmpf$DET_NITEM <- NULL
df_refri_class_refri_pmpf$PROD_XPROD <- NULL
tb_refri_class_refri_pmpf <- distinct(df_refri_class_refri_pmpf, PROD_CPROD_SEFAZ_DETALHADO,CODIGO_PMPF,.keep_all = T)

tb_refri_class_refri_pmpf <- tb_refri_class_refri_pmpf%>%
  select(PROD_CPROD_SEFAZ_DETALHADO,CODIGO_PMPF,PROD_CEANTRIB,PROD_CPROD_SEFAZ_DETALHADO,
         DESCRICAO_PROD_PMPF,VOLUME_PMPF,UNID_MEDIDA_PMPF,EMBALAGEM_PMPF)