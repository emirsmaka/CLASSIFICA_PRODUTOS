###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: ORQUESTRADROR                                               ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################
library(plyr)
library(dplyr)
library(DBI)
library(RODBC)
library(openxlsx)
library(data.table)
library(tidyverse)

## CARGA DE DADOS
source("./SCRIPTS/CALCULO_PMPF/CARGA_DADOS_REFRIGERANTE.R")
##################################################

## RETIRA OUTLIERS
source("./SCRIPTS/CALCULO_PMPF/FN_RETIRA_OUTLIERS.R")
df_bebidas_NFCE <- fn_outliers(df_bebidas_NFCE)
df_bebidas_NFCE$.id <- NULL
df_bebidas_NFCE_outlier <- df_bebidas_NFCE%>%
  filter(OUT == "TRUE")
df_bebidas_NFCE_sem_outlier <- anti_join(df_bebidas_NFCE,df_bebidas_NFCE_outlier,by="OUT")
rm(df_bebidas_NFCE)
gc(reset = T)
##################################################

## CALCULA MEDIAS ARITMETICA E PONDERADA
source("./SCRIPTS/CALCULO_PMPF/FN_CALCULA_VLR_MEDIO_PONDERADO.R")
df_bebidas_NFCE_media_ponderada <- fn_media_ponderada(df_bebidas_NFCE_sem_outlier)
tb_bebidas_media_ponderada <- left_join(df_pmpf_bebidas,df_bebidas_NFCE_media_ponderada, 
                                        by="PROD_CEANTRIB")
##################################################

## CHAMADA P/ ROTINA QUE RELACIONA REFRIGERANTES CLASSIFICADOS COM A TABELA 
## DE REFRIGERANTES DO PMPF
source("./SCRIPTS/CALCULO_PMPF/FN_RELACIONA_REFRI_CLASS_REFRI_PMPF.R")
tb_bebida_gtin <- fn_rel_class_pmpf(df_bebidas_NFE)
tb_bebidas_vlr_medio <- left_join(tb_bebidas_media_ponderada,tb_bebida_gtin,by="PROD_CEANTRIB")
tb_bebidas_vlr_medio <- tb_bebidas_vlr_medio%>%
  select(IDE_DHEMI_PERIODO,PROD_CEANTRIB,CODIGO_PMPF,PROD_CPROD_SEFAZ_AJUSTADO,PROD_CPROD_SEFAZ_DETALHADO,
         DESCRICAO_PROD_PMPF,PROD_XPROD_SEFAZ_AJUSTADO,PROD_XPROD_SEFAZ_DETALHADO,PROD_XPROD,VLR_PMPF,
         VLR_MEDIO_PONDERADO_MES,VLR_MEDIO_PONDERADO_TRIM,VLR_MEDIO_ARITM_MES,VLR_MEDIO_ARITM_TRIM,
         VLR_MINIMO,VLR_MAXIMO,QTE_TOTAL,VOLUME_PMPF,UNID_MEDIDA_PMPF,EMBALAGEM_PMPF,PROD_VOLUME_SEFAZ_AJUSTADO,
         PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO,PROD_EMBALAGEM_AJUSTADO)
#### AJUSTA COLUNAS
tb_bebidas_vlr_medio$PROD_CEANTRIB <- as.character(tb_bebidas_vlr_medio$PROD_CEANTRIB)
tb_bebidas_vlr_medio$PROD_CPROD_SEFAZ_DETALHADO <- as.character(tb_bebidas_vlr_medio$PROD_CPROD_SEFAZ_DETALHADO)
tb_bebidas_vlr_medio <- rename(tb_bebidas_vlr_medio,PROD_XPROD_NFCE = 'PROD_XPROD')
tb_bebidas_vlr_medio <- rename(tb_bebidas_vlr_medio,QTE_TOTAL_TRIM = 'QTE_TOTAL')
###################################
rm(list = ls())
gc(reset = T)
