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

### CARREGA NFE BEBIDAS CLASSIFICADAS
df_bebidas_NFE <- dbGetQuery(NZ_TribRef,"SELECT DISTINCT 
T0.IDE_SUBGRUPO 
,T0.NOME_SUBGRUPO 
,T0.PROD_CPROD_SEFAZ_AJUSTADO 
,T0.PROD_XPROD_SEFAZ_AJUSTADO 
,T1.PROD_CPROD_SEFAZ_DETALHADO 
,T1.PROD_XPROD_SEFAZ_DETALHADO 
,T2.PROD_CEANTRIB 
,T1.PROD_VOLUME_SEFAZ_AJUSTADO 
,T1.PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO 
,T1.PROD_EMBALAGEM_AJUSTADO 
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE  T0
LEFT JOIN MINING_SEFAZ.ADMIN.TB_BEBIDAS T1
ON T0.IDNFE = T1.IDNFE  AND T0.DET_NITEM = T1.DET_NITEM 
INNER JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T2
ON T0.IDNFE = T2.IDNFE AND T0.DET_NITEM =T2.DET_NITEM 
WHERE T0.IDE_SUBGRUPO = 10030440 AND 
      T2.PROD_CEANTRIB <> 'NULO' AND 
      T2.PROD_CEANTRIB <> 'SEM GTIN' AND 
      T0.IDE_DHEMI_PERIODO BETWEEN 201601 AND 202012")
############################################################

#### CARREGA NFCE
df_bebidas_NFCE <- dbGetQuery(NZ_TribRef, "SELECT 
EMIT_CNPJ_CPF 
,IDE_DHEMI_PERIODO 
,PROD_CEANTRIB 
,PROD_XPROD 
,PROD_QCOM 
,PROD_VUNCOM 
,PROD_VPROD
FROM TRIBUTARIO_REFERENCIA.ADMIN.SPK_NFCE_ITEM 
WHERE SUBSTRING(PROD_NCM,1,4) = '2202' AND 
      PROD_CEANTRIB <> 'SEM GTIN' AND 
      PROD_CEANTRIB <> 'NULO' AND
      IDE_DHEMI_PERIODO BETWEEN 202011 AND 202101")
############################################################

#### CARREGA CONTRIBUINTES RELEVANTES
#df_contribuintes <- read.xlsx("./DADOS/TB_CONTRIB_REFRI.xlsx")
# RENOMEIA COLUNAS
#df_contribuintes <- rename(df_contribuintes,EMIT_CNPJ_CPF = 'con_cnpj_cpf')
#df_contribuintes <- rename(df_contribuintes,EMIT_INSC_EST = 'con_insc_est')
#df_contribuintes <- rename(df_contribuintes,EMIT_MUNICIPIO = 'Municipio')
############################################################

#### CARREGA TABELA PMPF BEBIDAS
df_pmpf_bebidas <- read.xlsx("./DADOS/PMPF_BEBIDAS_fev2021.xlsx")

# RENOMEIA COLUNAS TABELA SISTEMA PMPF
df_pmpf_bebidas <- rename(df_pmpf_bebidas,PROD_CEANTRIB = 'CÓDIGO.EAN.TRIB')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,DESCRICAO_PROD_PMPF = 'DESCRIÇÃO.DO.PRODUTO')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,VOLUME_PMPF = 'VOLUME')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,EMBALAGEM_PMPF = 'EMBALAGEM')
df_pmpf_bebidas <- rename(df_pmpf_bebidas,CODIGO_PMPF = 'CÓDIGO.SEFAZ')
df_pmpf_bebidas <- rename(df_pmpf_bebidas, VLR_PMPF = 'VALOR')
df_pmpf_bebidas <- rename(df_pmpf_bebidas, UNID_MEDIDA_PMPF = 'UNID..MEDIDA')
df_pmpf_bebidas$PROD_CEANTRIB <- as.numeric(df_pmpf_bebidas$PROD_CEANTRIB)

df_pmpf_bebidas <- df_pmpf_bebidas%>%
  select(CODIGO_PMPF,PROD_CEANTRIB,DESCRICAO_PROD_PMPF,VOLUME_PMPF,UNID_MEDIDA_PMPF,
         EMBALAGEM_PMPF,VLR_PMPF)
############################################################

#### CRIA TABELA DE NFCE's POR CONTRIBUINTES RELEVANTES
#df_refri_NFCE$EMIT_CNPJ_CPF <- as.numeric(df_refri_NFCE$EMIT_CNPJ_CPF)
#df_contribuintes$EMIT_CNPJ_CPF <- as.numeric(df_contribuintes$EMIT_CNPJ_CPF)
#df_nfce_contrib <- inner_join(df_refri_NFCE,df_contribuintes,by = "EMIT_CNPJ_CPF")

