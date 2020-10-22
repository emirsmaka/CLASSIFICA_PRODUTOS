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

con_Netezza <- conect_NZ(con_Netezza)

df_cerveja <- dbGetQuery(con_Netezza,"SELECT T0.IDNFE, T0.DET_NITEM,
T1.IDE_DHEMI_PERIODO,T1.PROD_CEANTRIB, T1.prod_cean, T0.IDE_SUBGRUPO, T0.PROD_CPROD_SEFAZ_AJUSTADO, T0.prod_xprod_sefaz_ajustado, T1.prod_cfop,
SQLKIT..regexp_replace(T0.PROD_XPROD, ';','-') AS PROD_XPROD, SQLKIT..regexp_replace(T0.PROD_UCOM, ';','-') AS PROD_UCOM,
T0.PROD_QCOM, T1.PROD_VPROD, SQLKIT..regexp_replace(T1.PROD_UTRIB, ';','-') AS PROD_UTRIB,T1.PROD_QTRIB,T1.PROD_VUNTRIB,T1.PROD_VUNCOM,
T1.EMIT_CON_TIPO, T1.remetente_cnpj_cpf, T1.dest_cnpj_cpf_idestrangeiro, T1.recebedor_cnpj_cpf
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE T0
LEFT JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM
WHERE T0.PROD_CPROD_SEFAZ_AJUSTADO = 10030440008
      AND T1.IDE_DHEMI_PERIODO BETWEEN 201501 AND 202009")

############# CRIA COLUNAS EXTRAS E DE APOIO#############
df_cerveja[,"QTE_TRIB_AJUSTADO"] <- as.character()
df_cerveja[,"VOLUME_TRIB_AJUSTADO"] <- as.character()
df_cerveja[,"VUNTRIB_AJUSTADO"] <- as.double()
df_cerveja[,"FATOR_MULTIPLICADOR"] <- as.double()
df_cerveja[,"UNIDADE_SEFAZ"] <- "UN"
df_cerveja[,"VOLUME_SEFAZ"] <- as.double()
df_cerveja[,"UN_MEDIDA_SEFAZ"] <- as.character()
df_cerveja[,"VLR_UNITARIO_SEFAZ"] <- as.double()
df_cerveja[,"QTE_SEFAZ"] <- as.double()
########################################################

############# SEPARA REGISTROS NAO CLASSIFICADOS COMO CERVEJA
#df_nao_cerveja <- df_cerveja%>%
#  filter(V1 == -9999999 | V1 == 4000899)

#df_cerveja <- setdiff(df_cerveja,df_nao_cerveja)
########################################################

### Seleciona qtrib definidos com duzias
id_dz <- grep("^dz|^du",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja$QTE_TRIB_AJUSTADO[id_dz] <- df_cerveja$PROD_QCOM[id_dz] * 12
df_cerveja$FATOR_MULTIPLICADOR[id_dz] <- 12


### Seleciona Padrao PROD_UCOM descrito como "cxdd" onde dd representa o  numero de unidades na embalagem
id_cx<-grep("c(x)?\\s?\\d{1,4}",df_cerveja$PROD_UCOM, ignore.case = T)
df_cerveja$QTE_TRIB_AJUSTADO[id_cx]<- as.integer(str_extract(str_extract(tolower(df_cerveja$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}")) * df_cerveja$PROD_QCOM[id_cx]
df_cerveja$FATOR_MULTIPLICADOR[id_cx] <- as.double(str_extract(str_extract(tolower(df_cerveja$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}"))


###################### SEPARA REGISTROS JA AJUSTADOS ######################
df_cerveja_ajustada <- rbind(df_cerveja[id_dz,],df_cerveja[id_cx,])
df_cerveja <- setdiff(df_cerveja,df_cerveja_ajustada)
rm(id_cx,id_dz)
###################### FIM SEPARA REGISTROS JA AJUSTADOS ######################

gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################


#### REGEX IDENTIFICAR QTE_TRIB_AJUSTADO (QDO UNIDADE TRIB FOR CAIXA)

id_m1 <- grep("[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d",df_cerveja$PROD_XPROD,ignore.case = T) # cria indice dos padroes no data frame
df_m1 <- df_cerveja[id_m1,]
df_cerveja <- setdiff(df_cerveja,df_m1)
df_m1$QTE_TRIB_AJUSTADO<- as.integer(str_extract(str_extract(tolower(df_m1$PROD_XPROD),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))*df_m1$PROD_QCOM
df_m1$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_m1$PROD_XPROD),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))

###
id_m2 <- grep("\\sx\\s(\\d\\d)\\d?\\s",df_cerveja$PROD_XPROD,ignore.case = T)
df_m2 <- df_cerveja[id_m2,]
df_cerveja <- setdiff(df_cerveja,df_m2)
df_m2$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_m2$PROD_XPROD),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}")) * df_m2$PROD_QCOM
df_m2$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_m2$PROD_XPROD),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}"))
###
id_m3 <- grep("\\scx?(\\s)?\\d{1,4}|\\sc(\\/)?\\d{1,4}",df_cerveja$PROD_XPROD,ignore.case = T)
df_m3 <- df_cerveja[id_m3,]
df_cerveja <- setdiff(df_cerveja,df_m3)
df_m3$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_m3$PROD_XPROD),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}")) * df_m3$PROD_QCOM
df_m3$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_m3$PROD_XPROD),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}"))
###

df_cerveja_ajustada <- rbind(df_cerveja_ajustada, df_m1,df_m2,df_m3)
rm(id_m1,id_m2,id_m3,df_m1,df_m2,df_m3)
####

gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################



#### REGEX PARA PROD_QCOM COM UN + NUMEROS (ESSES INDICAM A QTE DE UNIDADES)
id_un1 <- grep("un(.)?\\d{1,4}",df_cerveja$PROD_UCOM,ignore.case = T)
df_un1 <- df_cerveja[id_un1,]
df_cerveja <- setdiff(df_cerveja,df_un1)
df_un1$QTE_TRIB_AJUSTADO<- as.integer(str_extract(str_extract(tolower(df_un1$PROD_UCOM),"un(.)?\\d{1,4}"),"\\d{1,4}"))*df_un1$PROD_QCOM
df_un1$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_un1$PROD_UCOM),"un(.)?\\d{1,4}"),"\\d{1,4}"))

## SEPARA REGISTROS AJUSTADOS
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_un1)
rm(id_un1,df_un1)
#############################
######################### SEM DUPLICACAO DE REGITROS#########################



#### REGEX PARA PADRAO CAIXA EM PROD_XPROD
id_cx3 <- grep("\\sc(x)?[[:punct:]]?\\d{1,4}",df_cerveja$PROD_XPROD,ignore.case = T)
source("./SCRIPTS/FUNCAO_OUTLIER.R")
id_outlier <- out_superior(df_cerveja$PROD_VUNCOM[id_cx3])

df_cerveja_caixa <- df_cerveja[id_cx3,][id_outlier,]
df_cerveja <- setdiff(df_cerveja,df_cerveja_caixa)

df_cerveja_caixa$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"\\sc(x)?[[:punct:]]?\\d{1,4}"),"\\d{1,4}"))*df_cerveja_caixa$PROD_QCOM
df_cerveja_caixa$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"\\sc(x)?[[:punct:]]?\\d{1,4}"),"\\d{1,4}"))
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja_caixa)
rm(df_cerveja_caixa,id_cx3,id_outlier)
###########################################################################
gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################

#### AJUSTA REGISTROS COM UNIDADE EM PROD_UCOM
id_unidade <- grep("^un",df_cerveja$PROD_UCOM,ignore.case = T)
id_outlier <- out_superior(df_cerveja$PROD_VUNCOM[id_unidade])
## SEPARA REGISTROS COM OUTLIERS EM PROD_VUNPROD
df_cerveja_un_outlier <- df_cerveja[id_unidade,][id_outlier,]
df_cerveja <- setdiff(df_cerveja, df_cerveja_un_outlier)
######
id_unidade <- grep("^un",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja$QTE_TRIB_AJUSTADO[id_unidade] <- df_cerveja$PROD_QCOM[id_unidade]
df_cerveja$FATOR_MULTIPLICADOR[id_unidade] <- 1
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja[id_unidade,])
df_cerveja <- setdiff(df_cerveja,df_cerveja[id_unidade,])
rm(id_outlier,id_unidade)
##################################################
gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################



#### REGEX Q IDENTIFICA CAIXAS EM PROD_XPROD COM PADRAO == 99U(N)
id_caixa <- grep("\\d{1,2}u(n)?",df_cerveja$PROD_XPROD,ignore.case = T)
df_cerveja_caixa <- df_cerveja[id_caixa,]
df_cerveja <- setdiff(df_cerveja,df_cerveja_caixa)
## EXTRAI MULTIPLICADOR DE PROD_XPROD Q INDICA A QTDE DE UNIDADES EM UMA CAIXA - PADRAO 9X9UN
multi_parte1 <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"\\d{1,2}x")
multi_parte2 <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"x\\d{1,2}")
multi_parte1 <- as.integer(str_extract(multi_parte1,"\\d{1,2}"))
multi_parte2 <- as.integer(str_extract(multi_parte2,"\\d{1,2}"))

df_cerveja_caixa$FATOR_MULTIPLICADOR <- ifelse(multi_parte1 >= multi_parte2,multi_parte1,multi_parte2)
df_cerveja_caixa$QTE_TRIB_AJUSTADO <- as.integer(df_cerveja_caixa$FATOR_MULTIPLICADOR) * df_cerveja_caixa$PROD_QCOM
rm(multi_parte1,multi_parte2)

## EXTRAI QTE DE UNIDADES NA EMBALAGEM DO REGISTRO Q ESTA EM PROD_XPROD - PADRAO == 99UN
id_qtde <- ifelse(is.na(df_cerveja_caixa$QTE_TRIB_AJUSTADO),TRUE,FALSE)
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qtde] <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_qtde]),"\\d{1,2}u(n)?")
df_cerveja_caixa$FATOR_MULTIPLICADOR[id_qtde] <- str_extract(df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qtde],"\\d{1,2}")
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qtde] <- as.integer(df_cerveja_caixa$FATOR_MULTIPLICADOR[id_qtde]) * df_cerveja_caixa$PROD_QCOM[id_qtde]

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja_caixa)
rm(df_cerveja_caixa,id_caixa,id_qtde)
########################################################################################

gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################


##### REGEX P/ EXTRAIR QTE DE UNIDADES EM PROD_UCOM == CX #####
id_caixa <- grep("cx",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja_caixa <- df_cerveja[id_caixa,]
df_cerveja <- setdiff(df_cerveja,df_cerveja_caixa)
id_cx1 <- grep("\\(\\d{1,2}\\)",df_cerveja_caixa$PROD_XPROD,ignore.case = T)
id_cx2 <- grep("\\d{1,2}\\s?u|\\d{1,2}\\s?lat|\\d{1,2}\\s?garr",df_cerveja_caixa$PROD_XPROD,ignore.case = T)
id_cx3 <- grep("cx(\\s|\\/|\\-)\\d{1,4}",df_cerveja_caixa$PROD_XPROD,ignore.case = T)
id_cx4 <- grep("lt\\d{2}|\\d{2}ret|\\s\\d{2}$|\\d{2}\\s?(multi)?pack|c\\s?(\\/|\\-)\\d{1,4}|\\d{1,2}\\s?gfs",df_cerveja_caixa$PROD_XPROD,ignore.case = T)
id_cx5 <- grep("\\d{2}\\/\\d{3}\\s?ml|ln\\d{2}\\s|peq\\d{2}",df_cerveja_caixa$PROD_XPROD,ignore.case = T)
id_qte_mult <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_cerveja_caixa$PROD_XPROD,ignore.case = T)

# cx1
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx1] <- str_extract(df_cerveja_caixa$PROD_XPROD[id_cx1],"\\(\\d{1,2}\\)")
df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx1] <- as.integer(str_extract(df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx1],"\\d{1,2}"))
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx1] <- df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx1] * df_cerveja_caixa$PROD_QCOM[id_cx1]
# cx2
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx2] <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_cx2]),"\\d{1,2}\\s?u|\\d{1,2}\\s?lat|\\d{1,2}\\s?garr")
df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx2] <- as.integer(str_extract(df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx2],"\\d{1,2}"))
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx2] <- df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx2] * df_cerveja_caixa$PROD_QCOM[id_cx2]
# cx3
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx3] <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_cx3]),"cx(\\s|\\/|\\-)\\d{1,4}")
df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx3] <- as.integer(str_extract(df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx3],"\\d{1,4}"))
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx3] <- df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx3] * df_cerveja_caixa$PROD_QCOM[id_cx3]
# cx4
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx4] <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_cx4]),"lt\\d{2}|\\d{2}ret|\\s\\d{2}$|\\d{2}\\s?(multi)?pack|c\\s?(\\/|\\-)\\d{1,4}|\\d{1,2}\\s?gfs")
df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx4] <- as.integer(str_extract(df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx4],"\\d{1,2}"))
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx4] <- as.integer(df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx4]) * df_cerveja_caixa$PROD_QCOM[id_cx4]
# cx5
df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx5] <- str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_cx5]),"\\d{2}\\/\\d{3}\\s?ml|ln\\d{2}\\s|peq\\d{2}"),"\\d{2}")
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_cx5] <- as.integer(df_cerveja_caixa$FATOR_MULTIPLICADOR[id_cx5]) * df_cerveja_caixa$PROD_QCOM[id_cx5]

# qte_mult
p1 <- str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_qte_mult]),"\\d{1,2}\\s?x\\s?\\d{1,2}"),"^\\d{1,2}")
p2 <- str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD[id_qte_mult]),"\\d{1,2}\\s?x\\s?\\d{1,2}"),"\\d{1,2}$")

df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qte_mult] <- as.integer(p1)*as.integer(p2)
df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qte_mult] <- ifelse(as.integer(df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qte_mult]) > 99,NA,df_cerveja_caixa$QTE_TRIB_AJUSTADO[id_qte_mult])

i_kit <- grep("kit(s)?",df_cerveja_caixa$PROD_XPROD,ignore.case = T)
df_cerveja_caixa$QTE_TRIB_AJUSTADO[i_kit] <- NA

rm(id_caixa,id_cx1,id_cx2,id_cx3,id_cx4,id_cx5,id_qte_mult,i_kit,p1,p2)
#############################################################################################################
##### SEPARA REGISTROS AJUSTADOS
df_sem_ajuste <- df_cerveja_caixa%>%
  filter(is.na(QTE_TRIB_AJUSTADO))
df_cerveja_caixa <- setdiff(df_cerveja_caixa,df_sem_ajuste)
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja_caixa)
df_cerveja <- rbind(df_cerveja,df_sem_ajuste)
rm(df_cerveja_caixa,df_sem_ajuste)
######################### SEM DUPLICACAO DE REGITROS#########################
gc(reset = T)

###### REGEX P/ EXTRAIR QTE UNIDADES COM PADRAO PCT/PC EM PROD_XPROD
id_pct <- grep("(pctc|pc|pct)\\s?\\d{1,2}",df_cerveja$PROD_XPROD,ignore.case = T)
df_pct <- df_cerveja[id_pct,]
df_cerveja <- setdiff(df_cerveja,df_pct)

df_pct$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_pct$PROD_XPROD),"(pctc|pc|pct)\\s?\\d{1,2}")
df_pct$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_pct$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_pct$QTE_TRIB_AJUSTADO <- df_pct$FATOR_MULTIPLICADOR * df_pct$PROD_QCOM

id_pct2 <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_pct$PROD_XPROD,ignore.case = T)
p1 <- str_extract(str_extract(tolower(df_pct$PROD_XPROD[id_pct2]),"\\d{1,2}\\s?x"),"\\d{1,2}")
p2 <- str_extract(str_extract(tolower(df_pct$PROD_XPROD[id_pct2]),"x\\s?\\d{1,2}"),"\\d{1,2}")
mult <- ifelse(p1>p2,p1,p2)
df_pct$FATOR_MULTIPLICADOR[id_pct2] <- as.integer(mult)
df_pct$QTE_TRIB_AJUSTADO[id_pct2] <- df_pct$FATOR_MULTIPLICADOR[id_pct2] * df_pct$PROD_QCOM[id_pct2]


df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_pct)

rm(df_pct,id_pct,id_pct2,mult,p1,p2)
gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################

####### REGEX EM PADROES ENCONTRADOS EM PROD_UCOM
# GARRAFA
id_grf <- grep("^g",df_cerveja$PROD_UCOM,ignore.case = T)
id_qtde <- grep("^q",df_cerveja$PROD_UCOM,ignore.case = T)
id_lata <- grep("^l(a|t|ta)",df_cerveja$PROD_UCOM,ignore.case = T)

i <- as.numeric(df_cerveja$PROD_UCOM)
id_num <- ifelse(is.na(i),FALSE,TRUE)

df_cerveja$QTE_TRIB_AJUSTADO[id_grf] <- df_cerveja$PROD_QCOM[id_grf]
df_cerveja$QTE_TRIB_AJUSTADO[id_num] <- df_cerveja$PROD_QCOM[id_num]
df_cerveja$QTE_TRIB_AJUSTADO[id_qtde] <- df_cerveja$PROD_QCOM[id_qtde]
df_cerveja$QTE_TRIB_AJUSTADO[id_lata] <- df_cerveja$PROD_QCOM[id_lata]

df_nao_ajusta <- df_cerveja%>%
  filter(is.na(QTE_TRIB_AJUSTADO))

df_ajustado <- setdiff(df_cerveja,df_nao_ajusta)
df_ajustado$FATOR_MULTIPLICADOR <- 1
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_ajustado)

df_cerveja <- df_nao_ajusta
rm(df_nao_ajusta,df_ajustado,id_grf,id_qtde,id_num,id_lata,i)
gc(reset = T)
#######################################################
######################### SEM DUPLICACAO DE REGITROS#########################

####### REGEX PARA "CX" EM PROD_UCOM E COM QTE POR CAIXA == 12 IDENTIFICADO EM PROD_XPROD
id_cx <- grep("cx", df_cerveja$PROD_UCOM,ignore.case = T)
id_qte <- grep("12",df_cerveja$PROD_XPROD[id_cx])
df_cerv_caixa <- df_cerveja[id_cx,][id_qte,]
df_cerveja <- setdiff(df_cerveja,df_cerv_caixa)
#SEPARAR REGISTROS IDENTIFICADOS COM CX EM PROD_UCOM
df_cerv_caixa$QTE_TRIB_AJUSTADO <- str_extract(df_cerv_caixa$PROD_XPROD,"12")
df_cerv_caixa$FATOR_MULTIPLICADOR <- df_cerv_caixa$QTE_TRIB_AJUSTADO
df_cerv_caixa$QTE_TRIB_AJUSTADO <- as.integer(df_cerv_caixa$QTE_TRIB_AJUSTADO) * as.integer(df_cerv_caixa$PROD_QCOM)

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerv_caixa)
rm(df_cerv_caixa,id_cx,id_qte)
#######################################################
######################### SEM DUPLICACAO DE REGITROS#########################

####### REGEX P EXTRAIR QTE DE UNIDADES EM PROD_XPROD
id_unidade <- grep("12\\s?u",df_cerveja$PROD_XPROD,ignore.case = T)
df_cerv_unidade <- df_cerveja[id_unidade,]
df_cerveja <- setdiff(df_cerveja,df_cerv_unidade)
df_cerv_unidade$FATOR_MULTIPLICADOR <- as.integer(str_extract(str_extract(tolower(df_cerv_unidade$PROD_XPROD), "12\\s?u"),"12"))
df_cerv_unidade$QTE_TRIB_AJUSTADO <- df_cerv_unidade$FATOR_MULTIPLICADOR * df_cerv_unidade$PROD_QCOM

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerv_unidade)
rm(df_cerv_unidade,id_unidade)
#######################################################
######################### SEM DUPLICACAO DE REGITROS#########################

####### REGEX IDENTIFICA QTE DE UNIDADES EM PROD_XPROD
id_unidade <- grep("((c|cx)\\d{2})$|\\(\\d\\)",df_cerveja$PROD_XPROD,ignore.case = T)
df_cerv_unidade <- df_cerveja[id_unidade,]
df_cerveja <- setdiff(df_cerveja,df_cerv_unidade)

df_cerv_unidade$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_cerv_unidade$PROD_XPROD),"((c|cx)\\d{2})$|\\(\\d\\)")
df_cerv_unidade$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_cerv_unidade$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_cerv_unidade$QTE_TRIB_AJUSTADO <- df_cerv_unidade$FATOR_MULTIPLICADOR * df_cerv_unidade$PROD_QCOM

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerv_unidade)
rm(df_cerv_unidade,id_unidade)
######################### SEM DUPLICACAO DE REGITROS#########################

############# IDENTIFICA ERROS NOS AJUSTES #############
df_erros_ajuste <- df_cerveja_ajustada%>%
  filter(FATOR_MULTIPLICADOR > 24)

##  SEPARA REGISTROS COM ERROS
df_cerveja_ajustada <- setdiff(df_cerveja_ajustada,df_erros_ajuste)

## REPARAR OS ERROS
id_err_1 <- grep("\\d{1,2}\\s?x\\s?\\d{3}",df_erros_ajuste$PROD_XPROD,ignore.case = T)
df_erros_ajuste$QTE_TRIB_AJUSTADO[id_err_1] <- str_extract( str_extract(tolower(df_erros_ajuste$PROD_XPROD[id_err_1]),"\\d{1,2}\\s?x\\s?\\d{3}"),"\\d{1,2}\\s?x")
df_erros_ajuste$FATOR_MULTIPLICADOR[id_err_1] <- as.integer(str_extract(df_erros_ajuste$QTE_TRIB_AJUSTADO[id_err_1],"\\d{1,2}"))
df_erros_ajuste$FATOR_MULTIPLICADOR[id_err_1] <- ifelse(df_erros_ajuste$FATOR_MULTIPLICADOR[id_err_1] > 24,0,df_erros_ajuste$FATOR_MULTIPLICADOR[id_err_1])
df_erros_ajuste$QTE_TRIB_AJUSTADO[id_err_1] <- as.integer(df_erros_ajuste$FATOR_MULTIPLICADOR[id_err_1]) * df_erros_ajuste$PROD_QCOM[id_err_1]

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_erros_ajuste[id_err_1,])
df_erros_ajuste <- setdiff(df_erros_ajuste,df_erros_ajuste[id_err_1,])

df_erros_ajuste$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_erros_ajuste$PROD_XPROD),"\\d{1,2}\\s?g(fs|rf)")
df_erros_ajuste$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_erros_ajuste$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_erros_ajuste$QTE_TRIB_AJUSTADO <- df_erros_ajuste$FATOR_MULTIPLICADOR * df_erros_ajuste$PROD_QCOM

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_erros_ajuste)
rm(df_erros_ajuste,id_err_1)
##########################################################
gc(reset = T)
######################### SEM DUPLICACAO DE REGITROS#########################

############# AJUSTA REGISTROS COM POSSIVEIS OUTLIERS #############
id_1 <- grep("\\d{1,2}\\s?u[n]",df_cerveja_un_outlier$PROD_XPROD,ignore.case = T)
df_out_1 <- df_cerveja_un_outlier[id_1,]
df_cerveja_un_outlier <- setdiff(df_cerveja_un_outlier,df_out_1)

id_2 <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_cerveja_un_outlier$PROD_XPROD,ignore.case = T)
df_out_2 <- df_cerveja_un_outlier[id_2,]
df_cerveja_un_outlier <- setdiff(df_cerveja_un_outlier,df_out_2)

id_3 <- grep("\\d{1,2}\\s?la",df_cerveja_un_outlier$PROD_XPROD,ignore.case = T)
df_out_3 <- df_cerveja_un_outlier[id_3,]
df_cerveja_un_outlier <- setdiff(df_cerveja_un_outlier,df_out_3)

df_out_1$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_out_1$PROD_XPROD),"\\d{1,2}\\s?u[n]")
df_out_1$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_out_1$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_out_1$FATOR_MULTIPLICADOR <- ifelse(df_out_1$FATOR_MULTIPLICADOR == 0,1,df_out_1$FATOR_MULTIPLICADOR)
df_out_1$QTE_TRIB_AJUSTADO <- df_out_1$FATOR_MULTIPLICADOR * df_out_1$PROD_QCOM
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_out_1)

df_out_2$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_out_2$PROD_XPROD),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- as.integer(str_extract(str_extract(tolower(df_out_2$QTE_TRIB_AJUSTADO),"\\d{1,2}\\s?x"),"\\d{1,2}"))
p2 <- as.integer(str_extract(str_extract(tolower(df_out_2$QTE_TRIB_AJUSTADO),"x\\s?\\d{1,2}"),"\\d{1,2}"))
df_out_2$FATOR_MULTIPLICADOR <- ifelse(p1>=p2,p1,p2)
df_out_2$FATOR_MULTIPLICADOR <- ifelse(df_out_2$FATOR_MULTIPLICADOR > 24,1,df_out_2$FATOR_MULTIPLICADOR)
df_out_2$QTE_TRIB_AJUSTADO <- df_out_2$FATOR_MULTIPLICADOR * df_out_2$PROD_QCOM
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_out_2)

df_out_3$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_out_3$PROD_XPROD),"\\d{1,2}\\s?la")
df_out_3$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_out_3$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_out_3$QTE_TRIB_AJUSTADO <- df_out_3$FATOR_MULTIPLICADOR * df_out_3$PROD_QCOM
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_out_3)

rm(id_1,id_2,id_3,df_out_1,df_out_2,df_out_3,p1,p2,out_inferior,out_superior,out_total)
gc(reset = T)
###########################################################
######################### SEM DUPLICACAO DE REGITROS#########################

source("./AJUSTA_QTE_FATOR_CERVEJA_COMP.R")

