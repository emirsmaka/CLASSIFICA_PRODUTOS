###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: CARREGA DADOS PARA CALCULO PMPF                             ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################


source("./SCRIPTS/FUNCOES/FN_CONEXAO_NETEZZA_2.R")

NZ_TribRef <- fn_conect_NZ_2(NZ_TribRef)
library(DBI)
library(RODBC)

### CARREGA NFE BEBIDAS CLASSIFICADAS
df_refri_NFE <- dbGetQuery(NZ_TribRef,"SELECT DISTINCT
T1.IDNFE 
,T1.DET_NITEM 
,T1.PROD_CEANTRIB 
,T1.PROD_XPROD 
,T2.PROD_CPROD_SEFAZ_DETALHADO 
,T2.PROD_XPROD_SEFAZ_DETALHADO 
FROM MINING_SEFAZ.ADMIN.TB_BEBIDAS T2
INNER JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T1.IDNFE = T2.IDNFE AND T1.DET_NITEM = T2.DET_NITEM 
WHERE T1.PROD_CEANTRIB <> 'SEM GTIN' AND 
      T1.PROD_CEANTRIB <> 'NULO' AND
      T2.PROD_CPROD_SEFAZ_DETALHADO <> -9999999999999999 AND 
      T1.IDE_DHEMI_PERIODO BETWEEN 201901 AND 202012")
############################################################

#### CARREGA NFCE
df_refri_NFCE <- dbGetQuery(NZ_TribRef, "SELECT 
EMIT_CNPJ_CPF 
,IDE_DHEMI_PERIODO 
,PROD_CEANTRIB 
,PROD_XPROD 
,PROD_QCOM 
,PROD_VUNCOM 
FROM TRIBUTARIO_REFERENCIA.ADMIN.SPK_NFCE_ITEM 
WHERE SUBSTRING(PROD_NCM,1,4) = '2202' AND 
      IDE_DHEMI_PERIODO BETWEEN 202011 AND 202101")
############################################################

#### CARREGA CONTRIBUINTES RELEVANTES
df_contribuintes <- read.xlsx("./DADOS/TB_CONTRIB_REFRI.xlsx")
# RENOMEIA COLUNAS
df_contribuintes <- rename(df_contribuintes,EMIT_CNPJ_CPF = 'con_cnpj_cpf')
df_contribuintes <- rename(df_contribuintes,EMIT_INSC_EST = 'con_insc_est')
df_contribuintes <- rename(df_contribuintes,EMIT_MUNICIPIO = 'Municipio')
############################################################

#### CARREGA TABELA PMPF BEBIDAS
df_pmpf_bebidas <- read.xlsx("./DADOS/PMPF_BEBIDAS_fev2021.xlsx")

# RENOMEIA COLUNAS TABELA SISTEMA PMPF
df_pmpf_bebidas <- rename(df_pmpf_bebidas,CEANTRIB_NFCE = 'C??DIGO.EAN.TRIB')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,DESCRICAO_PROD_PMPF = 'DESCRI????O.DO.PRODUTO')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,VOLUME_PMPF = 'VOLUME')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,EMBALAGEM_PMPF = 'EMBALAGEM')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,CODIGO_PMPF = 'C??DIGO.SEFAZ')
df_pmpf_bebidas <- rename(df_pmpf_bebidas, VLR_PMPF = 'VALOR')
df_pmpf_bebidas <- rename(df_pmpf_bebidas, UNID_MEDIDA_PMPF = 'UNID..MEDIDA')

df_pmpf_bebidas <- df_pmpf_bebidas%>%
  select(CODIGO_PMPF,CEANTRIB_NFCE,DESCRICAO_PROD_PMPF,VOLUME_PMPF,UNID_MEDIDA_PMPF,
         EMBALAGEM_PMPF,VLR_PMPF)
############################################################

#### CRIA TABELA DE NFCEs POR CONTRIBUINTES RELEVANTES
df_refri_NFCE$EMIT_CNPJ_CPF <- as.numeric(df_refri_NFCE$EMIT_CNPJ_CPF)
df_contribuintes$EMIT_CNPJ_CPF <- as.numeric(df_contribuintes$EMIT_CNPJ_CPF)
df_nfce_contrib <- inner_join(df_refri_NFCE,df_contribuintes,by = "EMIT_CNPJ_CPF")
###################################################################################################