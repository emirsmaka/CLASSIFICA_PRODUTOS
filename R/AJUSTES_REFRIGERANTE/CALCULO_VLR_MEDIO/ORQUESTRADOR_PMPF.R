###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: ORQUESTRADROR                                               ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

library(dplyr)
library(plyr)
library(DBI)
library(RODBC)
library(openxlsx)
library(data.table)
library(tidyverse)

## CHAMADA P/ CARGA DE DADOS
source("./SCRIPTS/REFRIGERANTE/CALCULO_PMPF/CARGA_DADOS_REFRIGERANTE.R")

## CHAMADA P/ RETIRADA DE OUTLIERS
source("./SCRIPTS/REFRIGERANTE/CALCULO_PMPF/FN_RETIRA_OUTLIERS.R")

## CHAMADA P/ ROTINA DE CALCULO DO VALOR MEDIO PELA NFCE
source("./SCRIPTS/REFRIGERANTE/CALCULO_PMPF/FN_CALCULA_VLR_MEDIO.R")

## CHAMADA P/ ROTINA QUE RELACIONA REFRIGERANTES CLASSIFICADOS COM A TABELA 
## DE REFRIGERANTES DO PMPF
source("./SCRIPTS/REFRIGERANTE/CALCULO_PMPF/FN_RELACIONA_REFRI_CLASS_REFRI_PMPF.R")

###################################

df_nfce_contrib <- fn_outliers(df_nfce_contrib)
df_refri_NFCE_ajustado <- fn_outliers(df_refri_NFCE)
tb_valor_medio <- fn_preco_medio(df_refri_NFCE_ajustado)
tb_rel_bebclass_pmpf <- fn_rel_class_pmpf(df_refri_NFE)
tb_valor_medio_contrib <- fn_preco_medio(df_nfce_contrib)

rm(df_contribuintes,df_nfce_contrib,df_pmpf_bebidas,df_refri_NFCE,df_refri_NFCE_ajustado,df_refri_NFE
   ,fn_conect_NZ_2,fn_out_bebida,fn_outliers,fn_pmpf_bebidas,fn_preco_medio,fn_rel_class_pmpf)
gc(reset = T)
