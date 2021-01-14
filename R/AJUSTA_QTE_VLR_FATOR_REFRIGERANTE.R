###############################################################################
##  SCRIPT.....: AJUSTES PARA REFRIGERANTES                                  ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR     ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

setwd("/home/local/FAZENDA/PROJETOS/CLASS_PROD")
library(dplyr)
library(data.table)
library(bit64)
library(stringr)
library(RODBC)
library(tidyverse)

########## ESTABELECE CONEXAO COM SERVER SQL ##########
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={ODBC Driver 17 for SQL Server};SERVER=s1670.ms\\itcsp01,1431;
                 \nDATABASE=suporte_ia;\nUID=esmaka;\nPWD=11qw-SG9M",timeout = 10)
#######################################################

######### LER DADOS PARA AJUSTES #########
df_refrigerante <-  dbGetQuery(con, "SELECT IDNFE,DET_NITEM, CPROD_REFRIGERANTE_SEFAZ,PROD_XPROD,PROD_UCOM,PROD_QCOM,
                                     PROD_VUNCOM,PROD_VPROD,FATOR_MULTIPLICADOR FROM TB_REFRIGERANTE_CLASS
                                     WHERE IDE_DHEMI_PERIODO BETWEEN 202001 AND 202012")
##########################################
source("./SCRIPTS/FUNCOES/FN_CRIA_COLUNAS.R")
#### CRIA COLUNAS DE APOIO
df_refrigerante <- fn_cria_colunas(df_refrigerante)
df_refrigerante$PROD_QCOM <- as.numeric(df_refrigerante$PROD_QCOM)
############################

df_nao_refri <- df_refrigerante%>%
  filter(CPROD_REFRIGERANTE_SEFAZ == -9999999)

df_refrigerante <- anti_join(df_refrigerante,df_nao_refri,by=c("IDNFE","DET_NITEM"))

######################################## PADROES DE QTDE EM PROD_QCOM ########################################################
### PADRAO DUZIAS EM PROD_UCOM == "DZ/DU"
id_dz <- grep("^dz|^du",df_refrigerante$PROD_UCOM,ignore.case = T)
df_dz <- df_refrigerante[id_dz,]
df_refrigerante <- anti_join(df_refrigerante,df_dz,by=c("IDNFE","DET_NITEM"))
df_dz$FATOR_MULTIPLICADOR <- 12
df_dz$QTE_SEFAZ <- df_dz$FATOR_MULTIPLICADOR * df_dz$PROD_QCOM
rm(id_dz)

### Seleciona Padrao PROD_UCOM descrito como "cxdd" onde dd representa o  numero de unidades na embalagem
id_cx<-grep("c[x]\\s?\\d{1,4}",df_refrigerante$PROD_UCOM, ignore.case = T)
df_cx <- df_refrigerante[id_cx,]
df_refrigerante <- anti_join(df_refrigerante,df_cx,by=c("IDNFE","DET_NITEM"))
df_cx$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cx$PROD_UCOM),"c[x]\\s?\\d{1,4}$"),"[0-9]{1,4}"))
df_cx$QTE_SEFAZ<- df_cx$FATOR_MULTIPLICADOR * df_cx$PROD_QCOM
rm(id_cx)

### PADRAO PROD_QCOM DECLARADO COMO 'UNdd' ONDE dd REPRESENTA A QTDE DE UNIDADES
id_undd <- grep("un\\d{1,4}",df_refrigerante$PROD_UCOM,ignore.case = T)
df_undd <- df_refrigerante[id_undd,]
df_refrigerante <- anti_join(df_refrigerante,df_undd,by=c("IDNFE","DET_NITEM"))
df_undd$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_undd$PROD_UCOM),"un\\d{1,4}"),"\\d{1,4}"))
df_undd$QTE_SEFAZ <- df_undd$FATOR_MULTIPLICADOR * df_undd$PROD_QCOM
rm(id_undd)

### PADRAO PROD_QCOM DECLARADO COMO 'UN..' ASSUME QUE A QUANTIDADE ESTEJA DECLARADA EM UNIDADES
id_un <-  grep("^u",df_refrigerante$PROD_UCOM,ignore.case = T)
df_un <- df_refrigerante[id_un,]
df_refrigerante <- anti_join(df_refrigerante,df_un,by=c("IDNFE","DET_NITEM"))
df_un$FATOR_MULTIPLICADOR <- 1
df_un$QTE_SEFAZ <- df_un$FATOR_MULTIPLICADOR * df_un$QTE_SEFAZ
rm(id_un)

### PADRAO DE REGISTROS 'SX' EM PROD_QCOM ==> SiX 6 UNIDADES
id_sx <- grep("sx",df_refrigerante$PROD_UCOM,ignore.case = T)
df_sx <- df_refrigerante[id_sx,]
df_refrigerante <- anti_join(df_refrigerante,df_sx,by=c("IDNFE","DET_NITEM"))
df_sx$FATOR_MULTIPLICADOR <- 6
df_sx$QTE_SEFAZ <- df_sx$FATOR_MULTIPLICADOR * df_sx$PROD_QCOM
rm(id_sx)

### UNIFICA DATAFRAME COM COLUNAS AJUSTADAS A PARTIR DE PROD_QCOM
df_refri_ajustado <- rbind(df_dz,df_cx,df_sx,df_un,df_undd)
rm(df_dz,df_cx,df_sx,df_un,df_undd)
###################################################################

#############################################################################################################################

######################################## PADROES DE QTDE EM PROD_XPROD ######################################################
id_un <- grep("\\s\\d{1,3}\\s?u",df_refrigerante$PROD_XPROD,ignore.case = T)
df_un <- df_refrigerante[id_un,]
df_refrigerante <- anti_join(df_refrigerante,df_un,by=c("IDNFE","DET_NITEM"))
df_un$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_un$PROD_XPROD),"\\s\\d{1,3}\\s?u"),"\\d{1,3}"))
df_un$QTE_SEFAZ <- df_un$FATOR_MULTIPLICADOR * df_un$PROD_QCOM
rm(id_un)

#### REGEX PARA IDENTIFICAR PADRAO 999 X 999 EM XPROD
id_qte <- grep("[1-9]{1,2}(\\s)?x(\\s)?\\d\\d\\d",df_refrigerante$PROD_XPROD,ignore.case = T) # cria indice dos padroes no data frame
df_refri_qte <- df_refrigerante[id_qte,]
df_refrigerante <- anti_join(df_refrigerante,df_refri_qte,by=c("IDNFE","DET_NITEM"))
df_refri_qte$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_refri_qte$PROD_XPROD),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))
df_refri_qte$QTE_SEFAZ <- df_refri_qte$FATOR_MULTIPLICADOR * df_refri_qte$PROD_QCOM
rm(id_qte)
###################################################

###
id_m2 <- grep("\\sx\\s(\\d\\d)\\d?\\s",df_refrigerante$PROD_XPROD,ignore.case = T)
df_refri_m2 <- df_refrigerante[id_m2,]
df_refrigerante <- anti_join(df_refrigerante,df_refri_m2,by=c("IDNFE","DET_NITEM"))
df_refri_m2$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_refri_m2$PROD_XPROD),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}"))
df_refri_m2$QTE_SEFAZ <- df_refri_m2$FATOR_MULTIPLICADOR  * df_refri_m2$PROD_QCOM
rm(id_m2)
###################################################

id_m3 <- grep("\\sc[x]?(.)?\\d{1,4}",df_refrigerante$PROD_XPROD,ignore.case = T)
df_refri_m3 <- df_refrigerante[id_m3,]
df_refrigerante <- anti_join(df_refrigerante,df_refri_m3,by=c("IDNFE","DET_NITEM"))
df_refri_m3$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_refri_m3$PROD_XPROD),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}"))
df_refri_m3$QTE_SEFAZ <-  df_refri_m3$FATOR_MULTIPLICADOR * df_refri_m3$PROD_QCOM
rm(id_m3)
###

#### AGRUPA REGISTROS AJUSTADOS ####
df_refri_ajustado <- rbind(df_refri_ajustado,df_refri_qte,df_refri_m2,df_refri_m3,df_un)
rm(df_refri_qte,df_refri_m2,df_refri_m3,df_un)
#####################################################

### Ajusta QTE_TRIB para outros padroes identificados (PCT ou PC)
id_m4 <- grep("(pc|pct|pctc)(\\s)?([0-9]){1,3}",df_refrigerante$PROD_XPROD,ignore.case = T)
df_m4 <- df_refrigerante[id_m4,]
df_refrigerante <- anti_join(df_refrigerante,df_m4,by=c("IDNFE","DET_NITEM"))
df_m4$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_m4$PROD_XPROD),"(pc|pct)(\\s)?([0-9]){1,3}"),"[0-9]{1,3}"))
df_m4$FATOR_MULTIPLICADOR <- ifelse(df_m4$FATOR_MULTIPLICADOR > 24, -1, df_m4$FATOR_MULTIPLICADOR)
df_m4$QTE_SEFAZ <-ifelse(df_m4$FATOR_MULTIPLICADOR > 24, -1, df_m4$FATOR_MULTIPLICADOR * df_m4$PROD_QCOM)

df_refri_ajustado <- rbind(df_refri_ajustado,df_m4)
rm(id_m4,df_m4)
####################################################

##### PADROES EM PROD_XPROD PARA PROD_UCOM == "CX"
id_cx <- grep("^cx$",df_refrigerante$PROD_UCOM,ignore.case = T)
df_cx <- df_refrigerante[id_cx,]
df_refrigerante <- anti_join(df_refrigerante,df_cx,by=c("IDNFE","DET_NITEM"))

i_cx1 <- grep("\\s\\d\\d\\s",df_cx$PROD_XPROD,ignore.case = T)
df_cx1 <- df_cx[i_cx1,]
df_cx <- anti_join(df_cx,df_cx1,by=c("IDNFE","DET_NITEM"))


i_cx2 <- grep("\\d{1,2}\\s?pack|\\d{1,2}\\s?p(\\s|$)",df_cx$PROD_XPROD,ignore.case = T)
df_cx2 <- df_cx[i_cx2,]
df_cx <- anti_join(df_cx,df_cx2,by=c("IDNFE","DET_NITEM"))

i_cx3 <- grep("(\\d\\dx\\d)|\\dx\\d\\s?l",df_cx$PROD_XPROD,ignore.case = T)
df_cx3 <- df_cx[i_cx3,]
df_cx <- anti_join(df_cx,df_cx3,by=c("IDNFE","DET_NITEM"))

i_cx4 <- grep("c\\/\\d\\d?|cx\\s?\\d\\d?",df_cx$PROD_XPROD,ignore.case = T)
df_cx4 <- df_cx[i_cx4,]
df_cx <- anti_join(df_cx,df_cx4,by=c("IDNFE","DET_NITEM"))

rm(i_cx1,i_cx2,i_cx3,i_cx4,id_cx)

df_cx1$FATOR_MULTIPLICADOR <- as.double(str_extract(df_cx1$PROD_XPROD,"\\s\\d\\d\\s"))
df_cx1$QTE_SEFAZ <- df_cx1$FATOR_MULTIPLICADOR * df_cx1$PROD_QCOM

df_cx2$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cx2$PROD_XPROD),"\\d{1,2}\\s?pack|\\d{1,2}\\s?p(\\s|$)"),"\\d{1,2}"))
df_cx2$QTE_SEFAZ <- df_cx2$FATOR_MULTIPLICADOR * df_cx2$PROD_QCOM

df_cx3$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(str_extract(tolower(df_cx3$PROD_XPROD),"(\\d\\dx\\d)|\\dx\\d\\s?l"),"\\d{1,2}x"),"\\d{1,2}"))
df_cx3$QTE_SEFAZ <- df_cx3$FATOR_MULTIPLICADOR * df_cx3$PROD_QCOM

df_cx4$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cx4$PROD_XPROD),"c\\/\\d\\d?|cx\\s?\\d\\d?"),"\\d{1,2}"))
df_cx4$QTE_SEFAZ <- df_cx4$FATOR_MULTIPLICADOR * df_cx4$PROD_QCOM

df_cxN <- rbind(df_cx1,df_cx2,df_cx3,df_cx4)
df_refri_ajustado <- rbind(df_refri_ajustado,df_cxN)

rm(df_cx1,df_cx2,df_cx3,df_cx4,df_cxN)
gc(reset = T)

df_refrigerante <- rbind(df_refrigerante,df_cx)
rm(df_cx)
######################################################################################

### PADRAO COM QTEXVOLUME
id_qte <- grep("\\s\\d\\d?x\\d(\\s|$)",df_refrigerante$PROD_XPROD,ignore.case = T)
df_qte <- df_refrigerante[id_qte,]
df_refrigerante <- anti_join(df_refrigerante,df_qte,by=c("IDNFE","DET_NITEM"))
df_qte$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_qte$PROD_XPROD),"\\s\\d\\d?x\\d(\\s|$)"),"\\d{1,2}"))
df_qte$QTE_SEFAZ <- df_qte$FATOR_MULTIPLICADOR * df_qte$PROD_QCOM
rm(id_qte)
######################################################################################

### PADROES DIVERSOS ENCONTRADOS EM PROD_XPROD
id_dv1 <- grep("lt\\s?\\d{1,2}(\\s|x)",df_refrigerante$PROD_XPROD, ignore.case = T)
df_dv1 <- df_refrigerante[id_dv1,]
df_refrigerante <- anti_join(df_refrigerante,df_dv1,by=c("IDNFE","DET_NITEM"))
df_dv1$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_dv1$PROD_XPROD),"lt\\s?\\d{1,2}(\\s|x)"),"\\d{1,2}"))
df_dv1$QTE_SEFAZ <- df_dv1$FATOR_MULTIPLICADOR * df_dv1$PROD_QCOM


id_dv2 <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_refrigerante$PROD_XPROD,ignore.case = T)
df_dv2 <- df_refrigerante[id_dv2,]
df_refrigerante <- anti_join(df_refrigerante,df_dv2,by=c("IDNFE","DET_NITEM"))
p1 <- as.double(str_extract(str_extract(tolower(df_dv2$PROD_XPROD),"\\d{1,2}\\s?x"),"\\d{1,2}"))
p2 <- as.double(str_extract(str_extract(tolower(df_dv2$PROD_XPROD),"x\\s?\\d{1,2}"),"\\d{1,2}"))
df_dv2$FATOR_MULTIPLICADOR <- ifelse(p1>=p2,p1,p2)
df_dv2$QTE_SEFAZ <- df_dv2$FATOR_MULTIPLICADOR * df_dv2$PROD_QCOM

## UNIR DATAFRAMES AJUSTADOS
df_refri_ajustado <- rbind(df_refri_ajustado,df_qte,df_dv1,df_dv2)
rm(df_qte,df_dv1,df_dv2,id_dv1,id_dv2,p1,p2)
######################################################################################

#### PADROES NUMERICOS INDICANDO EMBALAGENS COM 6,12 E 24 UNIDADES
id_emb <- grep("\\<6\\>|\\<06\\>|\\<12\\>|\\<24\\>|\\<lt12\\>|\\<lt6\\>",df_refrigerante$PROD_XPROD,ignore.case = T)
df_emb <- df_refrigerante[id_emb,]
df_refrigerante <- anti_join(df_refrigerante,df_emb,by=c("IDNFE","DET_NITEM"))
id_gr <- grep("gr",df_emb$PROD_UCOM,ignore.case = T)
df_gr <- df_emb[id_gr,]
df_emb <- anti_join(df_emb,df_gr,by=c("IDNFE","DET_NITEM"))

df_emb$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_emb$PROD_XPROD),"6|06|12|24|lt12|lt6"),"\\d{1,2}"))
df_emb$QTE_SEFAZ <- df_emb$FATOR_MULTIPLICADOR * df_emb$PROD_QCOM

df_gr$FATOR_MULTIPLICADOR <- 1
df_gr$QTE_SEFAZ <- df_gr$FATOR_MULTIPLICADOR * df_gr$PROD_QCOM

df_refri_ajustado <- rbind(df_refri_ajustado,df_emb,df_gr)
df_refri_ajustado$VLR_UNITARIO_SEFAZ <- df_refri_ajustado$PROD_VPROD / df_refri_ajustado$QTE_SEFAZ
rm(df_emb,df_gr,id_emb,id_gr)
gc(reset = T)
######################################################################################
df_refrigerante <- rbind(df_refrigerante,df_nao_refri)
df_refrigerante$FATOR_MULTIPLICADOR <- -1
df_refrigerante$QTE_SEFAZ <- -1
df_refrigerante$VLR_UNITARIO_SEFAZ <- -1

#### UNIFICA AS TABELAS
df_refrigerante <- rbind(df_refrigerante,df_refri_ajustado)
rm(df_refri_ajustado,df_nao_refri)
gc(reset = T)
######################################################### FIM PADROES QUANTIDADE #########################################################
source("./SCRIPTS/AJUSTA_VOLUME_UNID_MED_REFRIGERANTE.R")
