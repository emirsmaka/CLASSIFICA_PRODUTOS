###############################################################################
##  SCRIPT.....: AJUSTES PARA CERVEJAS                                       ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR     ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(stringr)

df_cerveja <- fread("./DADOS/nfe_cerveja2019e20.csv")
source("./SCRIPTS/FUNCOES/FN_AJUSTA_COLUNAS.R")
source("./SCRIPTS/FUNCOES/FN_CRIA_COLUNAS.R")

df_cerveja <- fn_cria_colunas(df_cerveja)

## RETIRA PRODUTO TIPO CHOPP
df_chopp <- df_cerveja%>%
  filter(CPROD_CERVEJA_SEFAZ == 4000899)
df_cerveja <- anti_join(df_cerveja,df_chopp,by=c("IDNFE","DET_NITEM"))
############################################

############################ PADROES Q IDENTIFICAM A QTDE REAL ENCONTRADOS EM PROD_UCOM ############################

#### PADROES Q IDENTIFICAM UMA DUZIA DE UNIDADES ####
id_dz <- grep("^dz|^du",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja_padrao <- df_cerveja[id_dz,]
df_cerveja_padrao$FATOR_MULTIPLICADOR <- 12
df_cerveja_padrao$QTE_SEFAZ <- df_cerveja_padrao$PROD_QCOM * df_cerveja_padrao$FATOR_MULTIPLICADOR
df_cerveja <- anti_join(df_cerveja,df_cerveja_padrao,by=c("IDNFE","DET_NITEM"))
#### FIM PADRAO DUZIA ####

#### PADRAO COM UN EM PROD_UCOM - ACEITA Q ESTA IDENTIFICANDO UNIDADE
id_un <- grep("^un$",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja_padrao <- rbind(df_cerveja_padrao,df_cerveja[id_un,])
df_cerveja_padrao$FATOR_MULTIPLICADOR <- 1
df_cerveja_padrao$QTE_SEFAZ <- df_cerveja_padrao$PROD_QCOM
df_cerveja <- anti_join(df_cerveja,df_cerveja_padrao,by=c("IDNFE","DET_NITEM"))
#### FIM PADRAO UNIDADE ####

#### PADRAO COM QTDE EM XPROD -> C/DD
id_cdd <- grep("\\sc\\/\\d{1,4}",df_cerveja$PROD_XPROD,ignore.case = T)
df_cdd <- df_cerveja[id_cdd,]
df_cerveja <- anti_join(df_cerveja,df_cdd,by=c("IDNFE","DET_NITEM"))
df_cdd <- fn_ajusta_colunas(df_cdd,"\\sc\\/\\d{1,4}","\\d{1,4}")
df_cerveja_padrao <- rbind(df_cerveja_padrao,df_cdd)
rm(df_cdd,id_cdd,id_dz,id_un)
######################################################

#### PADRAO PARA PROD_UCOM == "cxdd" onde dd representa o  numero de unidades na embalagem
id_cx<-grep("c(x)?\\s?\\d{1,4}",df_cerveja$PROD_UCOM, ignore.case = T)
df_cx <- df_cerveja[id_cx,]
df_cerveja <- anti_join(df_cerveja,df_cx,by=c("IDNFE","DET_NITEM"))
df_cx$QTE_SEFAZ<- as.integer(str_extract(str_extract(tolower(df_cx$PROD_UCOM),"c[x]\\s?\\d{1,5}"),"[0-9]{1,5}")) * df_cx$PROD_QCOM
df_cx$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cx$PROD_UCOM),"c[x]\\s?\\d{1,5}"),"[0-9]{1,5}"))
df_cerveja_padrao <- rbind(df_cerveja_padrao,df_cx)
#### FIM PADRAO CXdd ####

#### PADRAO PARA PROD_UCOM == SX -> IDENTIFICA EMBALAGEM CO SEIS (SiX) UNIDADES ####
id_sx <- grep("sx", df_cerveja$PROD_UCOM,ignore.case = T)
df_sx <- df_cerveja[id_sx,]
df_cerveja <- anti_join(df_cerveja,df_sx,by=c("IDNFE","DET_NITEM"))
df_sx$FATOR_MULTIPLICADOR <- 6
df_sx$QTE_SEFAZ <- df_sx$PROD_QCOM * df_sx$FATOR_MULTIPLICADOR
df_cerveja_padrao <- rbind(df_cerveja_padrao,df_sx)
#### FIM PADRAO SX ####

############################ FIM PADRAO COM QTDE IDENTIFICADA EM PROD_UCOM ############################
rm(id_cx,id_sx,df_cx,df_sx)
gc(reset = T)
####

############################ PADROES Q IDENTIFICAM A QTDE REAL ENCONTRADOS EM PROD_XPROD ############################

#### PADRAO EM PROD_XPROD ONDE A QTE VEM SEGUIDO DO VOLUME ####
id_qte <- grep("\\s\\d{1,2}x\\d{3,4}(\\s|$|m)",df_cerveja$PROD_XPROD,ignore.case = T)
df_qte <- df_cerveja[id_qte,]
df_cerveja <- anti_join(df_cerveja,df_qte,by=c("IDNFE","DET_NITEM"))

#### PADRAO EM PROD_XPROD ONDE A QTE SUCEDE O TERMO "CX" ####
id_cx <- grep("cx\\s?\\d{1,2}",df_cerveja$PROD_XPROD,ignore.case = T)
df_cx <- df_cerveja[id_cx,]
df_cerveja <- anti_join(df_cerveja,df_cx,by=c("IDNFE","DET_NITEM"))

#### PADROES DIVERSOS PARA QTE EM XPROD ####
id_pdr <- grep("(pc(t)?(\\s)?\\d{2}$|\\d{1,2}\\s?u(n)?|c(x)?[[:punct:]]\\d{1,2})",df_cerveja$PROD_XPROD,ignore.case = T)
df_pdr <- df_cerveja[id_pdr,]
df_cerveja <- anti_join(df_cerveja,df_pdr,by=c("IDNFE","DET_NITEM"))

#### PADRAO EM PROD_XPROD ONDE A QTE UNITARIA ESTA JUNTO A QTE DE EMBALAGENS ####
id_emb <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_cerveja$PROD_XPROD,ignore.case = T)
df_emb <- df_cerveja[id_emb,]
df_cerveja <- anti_join(df_cerveja,df_emb,by=c("IDNFE","DET_NITEM"))

#### AJUSTA COLUNAS ####
df_qte <- fn_ajusta_colunas(df_qte,"\\s\\d{1,2}x","\\d{1,2}")
df_cx <- fn_ajusta_colunas(df_cx,"cx\\s?\\d{1,2}","\\d{1,2}")
df_pdr <- fn_ajusta_colunas(df_pdr,"(pc(t)?\\d{2}$|\\d{1,2}\\s?u(n)?|c(x)?[[:punct:]]\\d{1,2})","\\d{1,2}")
############################

p1_emb <- str_extract(str_extract(tolower(df_emb$PROD_XPROD),"\\d{1,2}\\s?x"),"\\d{1,2}")
p2_emb <- str_extract(str_extract(tolower(df_emb$PROD_XPROD),"x\\s?\\d{1,2}"),"\\d{1,2}")
df_emb$FATOR_MULTIPLICADOR <- as.numeric(ifelse(p1_emb>p2_emb,p1_emb,p2_emb))
df_emb$QTE_SEFAZ <- df_emb$FATOR_MULTIPLICADOR * df_emb$PROD_QCOM
##
df_cerveja_padrao <- rbind(df_cerveja_padrao,df_qte,df_cx,df_pdr,df_emb)
rm(df_qte,df_cx,df_pdr,df_emb,id_cx,id_emb,id_pdr,id_qte,p1_emb,p2_emb)
gc(reset = T)

## OUTROS PADROES ##
padroes <- c("\\<\\d{1,2}$\\>|\\<\\d{1,2}\\spack\\>|\\<\\(\\d{1,2}\\)$\\>|\\s\\d{2}\\s|\\<\\d{1,2}pack\\>")
id_padroes <- grep(padroes,df_cerveja$PROD_XPROD,ignore.case = T)
df_padroes <- df_cerveja[id_padroes,]
df_cerveja <- anti_join(df_cerveja,df_padroes,by=c("IDNFE","DET_NITEM"))

padroes <- c("\\d{1,2}$|\\d{1,2}\\spack|\\(\\d{1,2}\\)$|\\s\\d{2}\\s|\\d{1,2}pack")

df_padroes <- fn_ajusta_colunas(df_padroes,padroes,"\\d{1,2}")
df_cerveja_padrao <- rbind(df_cerveja_padrao,df_padroes)
df_cerveja <- rbind(df_cerveja,df_cerveja_padrao)
## AJUSTA VLR UNITARIO
df_cerveja$VLR_UNITARIO_SEFAZ <- df_cerveja$PROD_VPROD / df_cerveja$QTE_SEFAZ
##
rm(df_padroes,id_padroes,df_cerveja_padrao,padroes,fn_ajusta_colunas,fn_cria_colunas)
gc(reset = T)
##################### FIM DE AJUSTES DAS COLUNAS FATOR_MULTIPLICADOR/QTE_SEFAZ/VLR_UNITARIO_SEFAZ #####################
source("./SCRIPTS/AJUSTES_VOLUME_UNID_MED_CERVEJA.R")
