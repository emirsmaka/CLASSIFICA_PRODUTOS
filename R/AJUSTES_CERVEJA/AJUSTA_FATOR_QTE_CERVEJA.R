###############################################################################
##  SCRIPT.....: AJUSTES PARA CERVEJAS                                       ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR     ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

## RETIRA PRODUTO TIPO CHOPP
df_chopp <- df_cerveja%>%
  filter(CPROD_CERVEJA_SEFAZ == 4000899 | CPROD_CERVEJA_SEFAZ == -9999999)
df_cerveja <- anti_join(df_cerveja,df_chopp,by=c("IDNFE","DET_NITEM"))
############################################

############################ PADROES Q IDENTIFICAM A QTDE REAL ENCONTRADOS EM PROD_UCOM ############################

#### PADROES Q IDENTIFICAM UMA DUZIA DE UNIDADES ####
id_dz <- grep("^dz|^du",df_cerveja$PROD_UCOM,ignore.case = T)
df_dz <- df_cerveja[id_dz,]
df_dz$FATOR_MULTIPLICADOR <- 12
df_dz$QTE_SEFAZ <- df_dz$PROD_QCOM * df_dz$FATOR_MULTIPLICADOR
df_cerveja <- anti_join(df_cerveja,df_dz,by=c("IDNFE","DET_NITEM"))
#### FIM PADRAO DUZIA ####

#### PADRAO COM UN EM PROD_UCOM - ACEITA Q ESTA IDENTIFICANDO UNIDADE
id_un <- grep("^un$",df_cerveja$PROD_UCOM,ignore.case = T)
df_un <- df_cerveja[id_un,]
df_un$FATOR_MULTIPLICADOR <- 1
df_un$QTE_SEFAZ <- df_un$PROD_QCOM
df_cerveja <- anti_join(df_cerveja,df_un,by=c("IDNFE","DET_NITEM"))
#### FIM PADRAO UNIDADE ####

#### PADRAO COM UNXX EM PROD_UCOM - QTE DE UNIDADES ESTA APOS SIGLA "UN"
id_undd <- grep("un\\d{1,4}",df_cerveja$PROD_UCOM, ignore.case = T)
df_undd <- df_cerveja[id_undd,]
fator <- str_extract(str_extract(tolower(df_undd$PROD_UCOM),"un\\d{1,3}"),"\\d{1,3}")
df_undd$FATOR_MULTIPLICADOR <- as.double(fator)
df_undd$QTE_SEFAZ <- df_undd$FATOR_MULTIPLICADOR * df_undd$PROD_QCOM
df_cerveja <- anti_join(df_cerveja,df_undd,by=c("IDNFE","DET_NITEM"))
#### FIM PADRAO UNXX ####

#### PADRAO PARA PROD_UCOM == "cxdd" onde dd representa o  numero de unidades na embalagem
id_cx<-grep("c(x)?\\s?\\d{1,4}",df_cerveja$PROD_UCOM, ignore.case = T)
df_cx <- df_cerveja[id_cx,]
df_cerveja <- anti_join(df_cerveja,df_cx,by=c("IDNFE","DET_NITEM"))
df_cx$QTE_SEFAZ<- as.integer(str_extract(str_extract(tolower(df_cx$PROD_UCOM),"c(x)?\\s?\\d{1,5}"),"[0-9]{1,5}")) * df_cx$PROD_QCOM
df_cx$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cx$PROD_UCOM),"c(x)?\\s?\\d{1,5}"),"[0-9]{1,5}"))
#### FIM PADRAO CXdd ####

#### PADRAO PARA PROD_UCOM == SX -> IDENTIFICA EMBALAGEM CO SEIS (SiX) UNIDADES ####
id_sx <- grep("sx", df_cerveja$PROD_UCOM,ignore.case = T)
df_sx <- df_cerveja[id_sx,]
df_cerveja <- anti_join(df_cerveja,df_sx,by=c("IDNFE","DET_NITEM"))
df_sx$FATOR_MULTIPLICADOR <- 6
df_sx$QTE_SEFAZ <- df_sx$PROD_QCOM * df_sx$FATOR_MULTIPLICADOR
#### FIM PADRAO SX ####

df_cerv_ajustada <- rbind(df_sx,df_cx,df_undd,df_un,df_dz)

############################ FIM PADRAO COM QTDE IDENTIFICADA EM PROD_UCOM ############################
rm(id_cx,id_sx,df_cx,df_sx,id_undd,id_dz,df_undd,df_un,df_dz,fator)
gc(reset = T)
#######################################################################################################

############################ PADROES Q IDENTIFICAM A QTDE REAL ENCONTRADOS EM PROD_XPROD ############################

#### PADRAO PROD_XPROD == "SIX C/DD OU CXDD ####
id_six <- grep("six.",df_cerveja$PROD_XPROD,ignore.case = T)
df_six <- df_cerveja[id_six,]
df_cerveja <- anti_join(df_cerveja,df_six,by=c("IDNFE","DET_NITEM"))
id_no_sixpack <- grep("sixpoint",df_six$PROD_XPROD,ignore.case = T)
df_no_sixpack <- df_six[id_no_sixpack,]
df_six <- anti_join(df_six,df_no_sixpack,by=c("IDNFE","DET_NITEM"))
patt <- c("c(x)?\\s?[[:punct:]]?\\d{1,2}")
id_sixcx <- grep(patt,df_six$PROD_XPROD,ignore.case = T)
df_sixcx <- df_six[id_sixcx,]
df_six <- anti_join(df_six,df_sixcx,by=c("IDNFE","DET_NITEM"))

df_six$FATOR_MULTIPLICADOR <- 6
df_six$QTE_SEFAZ <- df_six$PROD_QCOM * df_six$FATOR_MULTIPLICADOR

df_sixcx$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_sixcx$PROD_XPROD),patt),"\\d{1,2}"))
df_sixcx$FATOR_MULTIPLICADOR <- df_sixcx$FATOR_MULTIPLICADOR * 6
df_sixcx$QTE_SEFAZ <- df_sixcx$PROD_QCOM * df_sixcx$FATOR_MULTIPLICADOR
#### UNIAO PADRAO SIX ####
df_six <- rbind(df_six,df_sixcx,df_no_sixpack)
############################
rm(id_no_sixpack,id_six,id_sixcx,df_no_sixpack,df_sixcx)

#### PADRAO PROD_XPROD == 99X99UN ####
id_un <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}un",df_cerveja$PROD_XPROD,ignore.case = T)
df_un <- df_cerveja[id_un,]
df_cerveja <- anti_join(df_cerveja,df_un,by=c("IDNFE","DET_NITEM"))
p1 <- as.double(str_extract(str_extract(tolower(df_un$PROD_XPROD),"\\d{1,2}\\s?x"),"\\d{1,2}"))
p2 <- as.double(str_extract(str_extract(tolower(df_un$PROD_XPROD),"x\\s?\\d{1,2}un"),"\\d{1,2}"))
df_un$FATOR_MULTIPLICADOR <- p1*p2
df_un$QTE_SEFAZ <- df_un$FATOR_MULTIPLICADOR * df_un$PROD_QCOM
######################################################

#### PADRAO COM QTDE EM XPROD -> C/DD
id_cdd <- grep("\\sc\\/\\s?\\d{1,4}",df_cerveja$PROD_XPROD,ignore.case = T)
df_cdd <- df_cerveja[id_cdd,]
df_cerveja <- anti_join(df_cerveja,df_cdd,by=c("IDNFE","DET_NITEM"))
df_cdd <- fn_ajusta_colunas(df_cdd,"\\sc\\/\\d{1,4}","\\d{1,4}")
rm(id_cdd,id_un)
######################################################

#### PADRAO EM PROD_XPROD ONDE A QTE SUCEDE O TERMO "CX" ####
id_cx <- grep("cx\\s?\\d{1,2}",df_cerveja$PROD_XPROD,ignore.case = T)
df_cx <- df_cerveja[id_cx,]
df_cerveja <- anti_join(df_cerveja,df_cx,by=c("IDNFE","DET_NITEM"))

#### PADRAO EM PROD_XPROD ONDE A QTE VEM SEGUIDO DO VOLUME ####
id_qte <- grep("\\d{1,2}\\s?x\\s?\\d{3,4}",df_cerveja$PROD_XPROD,ignore.case = T)
df_qte <- df_cerveja[id_qte,]
df_cerveja <- anti_join(df_cerveja,df_qte,by=c("IDNFE","DET_NITEM"))

#### PADROES DIVERSOS PARA QTE EM XPROD ####
id_pdr <- grep("(pc(t)?(\\s)?\\d{2}$|\\d{1,2}\\s?u(n)?|c(x)?[[:punct:]]\\d{1,2})",df_cerveja$PROD_XPROD,ignore.case = T)
df_pdr <- df_cerveja[id_pdr,]
df_cerveja <- anti_join(df_cerveja,df_pdr,by=c("IDNFE","DET_NITEM"))

#### PADRAO EM PROD_XPROD ONDE A QTE UNITARIA ESTA JUNTO A QTE DE EMBALAGENS ####
id_emb <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_cerveja$PROD_XPROD,ignore.case = T)
df_emb <- df_cerveja[id_emb,]
df_cerveja <- anti_join(df_cerveja,df_emb,by=c("IDNFE","DET_NITEM"))

#### AJUSTA COLUNAS ####
df_qte <- fn_ajusta_colunas(df_qte,"\\s?\\d{1,2}\\s?x","\\d{1,2}")
df_cx <- fn_ajusta_colunas(df_cx,"cx\\s?\\d{1,2}","\\d{1,2}")
df_pdr <- fn_ajusta_colunas(df_pdr,"(pc(t)?\\d{2}$|\\d{1,2}\\s?u(n)?|c(x)?[[:punct:]]\\d{1,2})","\\d{1,2}")
############################

p1_emb <- str_extract(str_extract(tolower(df_emb$PROD_XPROD),"\\d{1,2}\\s?x"),"\\d{1,2}")
p2_emb <- str_extract(str_extract(tolower(df_emb$PROD_XPROD),"x\\s?\\d{1,2}"),"\\d{1,2}")
df_emb$FATOR_MULTIPLICADOR <- as.numeric(ifelse(p1_emb>p2_emb,p1_emb,p2_emb))
df_emb$QTE_SEFAZ <- df_emb$FATOR_MULTIPLICADOR * df_emb$PROD_QCOM
##
df_cerv_ajustada <- rbind(df_cerv_ajustada,df_qte,df_cx,df_pdr,df_emb,df_six,df_cdd,df_un)
rm(df_qte,df_cx,df_pdr,df_emb,id_cx,id_emb,id_pdr,id_qte,p1_emb,p2_emb,df_six,patt,df_cdd,df_un,p1,p2)
gc(reset = T)

## OUTROS PADROES ##
padroes <- c("\\<\\d{1,2}$\\>|\\<\\d{1,2}\\spack\\>|\\<\\(\\d{1,2}\\)$\\>|\\s\\d{2}\\s|\\<\\d{1,2}pack\\>")
id_padroes <- grep(padroes,df_cerveja$PROD_XPROD,ignore.case = T)
df_padroes <- df_cerveja[id_padroes,]
df_cerveja <- anti_join(df_cerveja,df_padroes,by=c("IDNFE","DET_NITEM"))

padroes <- c("\\d{1,2}$|\\d{1,2}\\spack|\\(\\d{1,2}\\)$|\\s\\d{2}\\s|\\d{1,2}pack")

df_padroes <- fn_ajusta_colunas(df_padroes,padroes,"\\d{1,2}")
df_cerv_ajustada <- rbind(df_cerv_ajustada,df_padroes)
df_cerveja <- rbind(df_cerveja,df_cerv_ajustada)
## AJUSTA VLR UNITARIO
df_cerveja$VLR_UNITARIO_SEFAZ <- df_cerveja$PROD_VPROD / df_cerveja$QTE_SEFAZ
##
rm(df_padroes,id_padroes,df_cerv_ajustada,padroes,fn_ajusta_colunas,fn_cria_colunas)
gc(reset = T)
##################### FIM DE AJUSTES DAS COLUNAS FATOR_MULTIPLICADOR/QTE_SEFAZ/VLR_UNITARIO_SEFAZ #####################
source("./SCRIPTS/CERVEJA/AJUSTES_VOLUME_UNID_MED_CERVEJA.R")
