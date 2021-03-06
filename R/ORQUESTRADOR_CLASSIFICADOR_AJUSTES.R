###############################################################################
##  SCRIPT.....: ORQUESTRADOR CLASSIFACAO E AJUSTES PRODUTOS SEFAZ           ##
##  DETALHE....: AJUSTA COLUNAS PADRONIZADAS PELA SEFAZ-MS                   ##
##  DATA.......: 17 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
##  DESCRICAO..: PROGRAMA QUE INSTANCIA DEMAIS SCRIPTS PARA A CLASSIFICACAO  ##
##               DE REFRIGERANTES E CERVEJAS E AJUSTA AS COLUNAS DE VOLUME,  ##
##               UNIDADE DE MEDIDA, VALOR UNITARIO, QUANTIDADE, UNIDADE      ##
##               E O FATOR MULTIPLICADOR QUE IDENTIFICA A QUANTIDADE REAL    ##
###############################################################################

################################### CARREGA BIBLIOTECAS NECESSARIAS  ######################################
library(plyr)
library(dplyr)
library(openxlsx)
library(stringr)
library(data.table)
library(h2o)
library(odbc)
library(RODBC)
################################## FIM CARREGA BIBLIOTECAS NECESSARIAS  ###################################

######################################### CONFIGURA PATH DA APP ###########################################
setwd("C:/DADOS_R/Scripts")

####################################### CARREGA OS DADOS PARA O R #########################################
### CONECTA SQL SERVERS s1670 e s904
con_sql1670 <- dbConnect(odbc::odbc(), "SQL_s1670")
con_sqls904 <- dbConnect(odbc::odbc(), "s904")

con_Netezza <- RODBC::odbcConnect("nz1436",uid = "esmaka",pwd = "@q158850i")

df_nfe_diaria <- sqlQuery(con_sqls904,"SELECT * FROM [ugst_bebidas].[dbarantes].[TB_NFE_CLASSIFICADA]")
delete_tb_sql <- "TRUNCATE TABLE s904 -> [ugst_bebidas].[dbarantes].[TB_NFE_CLASSIFICADA]"
###################################### FIM CARREGA OS DADOS PARA O R ######################################

######################################### CRIA FUNCOES UTILIZADAS #########################################
source("./R/FUNCOES_CALCULO.R")
source("./SCRIPTS/FUNCOES/FN_AJUSTA_COLUNAS.R")
source("./SCRIPTS/FUNCOES/FN_CRIA_COLUNAS.R")
################################# CRIA COLUNAS PARA AJUSTES E COLUNAS DE APOIO ############################
df_nfe_diaria <- fn_cria_col(df_nfe_diaria)
############################### FIM CRIA COLUNAS PARA AJUSTES E COLUNAS DE APOIO ##########################

######################################### SEPARAR PRODUTOS ############################################
## SEPARA  MILHO E SOJA
df_milho <- df_nfe_diaria %>%
  filter(PROD_CPROD_SEFAZ_AJUSTADO == 10010221003 | PROD_CPROD_SEFAZ_AJUSTADO == 10020221002)
df_milho$PROD_QCOM <- as.numeric(df_milho$PROD_QCOM)
df_soja <- df_nfe_diaria%>%
  filter(PROD_CPROD_SEFAZ_AJUSTADO == 10010220001 | PROD_CPROD_SEFAZ_AJUSTADO == 10020220004)
df_soja$PROD_QCOM <- as.numeric(df_soja$PROD_QCOM)
###################################

## BEBIDAS
df_refrigerante <- df_nfe_diaria %>%
  filter(PROD_CPROD_SEFAZ_AJUSTADO == 10030440006)
df_refrigerante$PROD_QCOM <- as.numeric(df_refrigerante$PROD_QCOM)
df_cerveja <- df_nfe_diaria %>%
  filter(PROD_CPROD_SEFAZ_AJUSTADO == 10030440008)
df_cerveja$PROD_QCOM <- as.numeric(df_cerveja$PROD_QCOM)
###################################

## CARNES (ANIMAIS)
#subgrupo <- c(10020101,10020102,10010103,10020104,10020105,10010106,10010109,10010110,10010111)

#df_animais <- df_nfe_diaria %>%
#  filter(IDE_SUBGRUPO %in% subgrupo)
######################################### FIM SEPARA PRODUTOS #########################################

################################# CHAMADA SCRIPTES CLASSIFICACAO E AJUSTES ############################

####### AJUSTA MILHO E SOJA #######

source("./AJUSTA_MILHO.R")
source("./AJUSTA_SOJA.R")
source("./SCRIPTS/REFRIGERANTE/AJUSTA_QTDE_FATOR.R")
source("./SCRIPTS/CERVEJA/AJUSTA_QTDE_FATOR.R")
#########################################




































