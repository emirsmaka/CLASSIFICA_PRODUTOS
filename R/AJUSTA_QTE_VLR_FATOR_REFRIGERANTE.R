###############################################################################
##  SCRIPT.....: AJUSTES PARA REFRIGERANTES                                  ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR     ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

setwd("C:/DADOS_R/Scripts")
library(dplyr)
library(data.table)
library(bit64)
library(stringr)
library(RODBC)

df_refrigerante <- fread("./refrigerante_2015e2016.csv",dec = ",",stringsAsFactors = T,integer64 = "numeric")

############# CRIA COLUNAS EXTRAS E DE APOIO#############
df_refrigerante[,"QTE_TRIB_AJUSTADO"] <- as.character()
df_refrigerante[,"VOLUME_TRIB_AJUSTADO"] <- as.character()
df_refrigerante[,"VUNTRIB_AJUSTADO"] <- as.double()
df_refrigerante[,"FATOR_MULTIPLICADOR"] <- as.double()
df_refrigerante[,"UNIDADE_SEFAZ"] <- "UN"
df_refrigerante[,"VOLUME_SEFAZ"] <- as.double()
df_refrigerante[,"UN_MEDIDA_SEFAZ"] <- as.character()
df_refrigerante[,"VLR_UNITARIO_SEFAZ"] <- as.double()
df_refrigerante[,"QTE_SEFAZ"] <- as.double()
########################################################


### PADRAO DUZIAS EM PROD_UCOM == "DZ/DU"
id_dz <- grep("^dz|^du",df_refrigerante$PROD_UCOM,ignore.case = T)
df_refrigerante$QTE_TRIB_AJUSTADO[id_dz] <- df_refrigerante$PROD_QCOM[id_dz] * 12
df_refrigerante$FATOR_MULTIPLICADOR[id_dz] <- 12
rm(id_dz)

### Seleciona Padrao PROD_UCOM descrito como "cxdd" onde dd representa o  numero de unidades na embalagem

id_cx<-grep("c[x]\\s?\\d{1,4}",df_refrigerante$PROD_UCOM, ignore.case = T)
df_refrigerante$QTE_TRIB_AJUSTADO[id_cx]<- as.integer(str_extract(str_extract(tolower(df_refrigerante$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}$"),"[0-9]{1,4}")) * df_refrigerante$PROD_QCOM[id_cx]
df_refrigerante$FATOR_MULTIPLICADOR[id_cx] <- as.double(str_extract(str_extract(tolower(df_refrigerante$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}$"),"[0-9]{1,4}"))

### Seleciona qtrib definidos corretamente como unidade
id_utrib<-  grep("^u|^lta|^lt|^lat|^ga|^gr|^gfa|grf|gf|pec|vd|tubo",df_refrigerante$PROD_UCOM,ignore.case = T)
df_refrigerante$QTE_TRIB_AJUSTADO[id_utrib] <- df_refrigerante$PROD_QCOM[id_utrib]
df_refrigerante$FATOR_MULTIPLICADOR[id_utrib] <- 1
rm(id_utrib,id_cx)


### SEPARA REGISTROS AJUSTADOS
id_null <- ifelse(is.na(df_refrigerante$QTE_TRIB_AJUSTADO), TRUE,FALSE)
df_refri_nao_ajustado <- df_refrigerante[id_null,]
df_refrigerante <- setdiff(df_refrigerante,df_refri_nao_ajustado)
rm(id_null)
gc(reset = TRUE)


#### REGEX IDENTIFICAR QTE_TRIB_AJUSTADO (QDO UNIDADE TRIB FOR CAIXA)

id_m1 <- grep("[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T) # cria indice dos padroes no data frame
df_refri_m1 <- df_refri_nao_ajustado[id_m1,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_refri_m1)
df_refri_m1$QTE_TRIB_AJUSTADO<- as.integer(str_extract(str_extract(tolower(df_refri_m1$PROD_XPROD),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))*df_refri_m1$PROD_QCOM
df_refri_m1$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_refri_m1$PROD_XPROD),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))
###
id_m2 <- grep("\\sx\\s(\\d\\d)\\d?\\s",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_refri_m2 <- df_refri_nao_ajustado[id_m2,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_refri_m2)
df_refri_m2$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_refri_m2$PROD_XPROD),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}")) * df_refri_m2$PROD_QCOM
df_refri_m2$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_refri_m2$PROD_XPROD),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}"))
###
id_m3 <- grep("\\sc[x]?(.)?\\d{1,4}",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_refri_m3 <- df_refri_nao_ajustado[id_m3,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_refri_m3)
df_refri_m3$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_refri_m3$PROD_XPROD),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}")) * df_refri_m3$PROD_QCOM
df_refri_m3$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_refri_m3$PROD_XPROD),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}"))
###
df_refri_m <- rbind(df_refri_m1,df_refri_m2,df_refri_m3)
rm(id_m1,id_m2,id_m3,df_refri_m1,df_refri_m2,df_refri_m3)
####


#### UNIFICA AS TABELAS SEPARADAS ACIMA
df_refrigerante<-rbind(df_refrigerante,df_refri_m)
rm(df_refri_m)
####################################


### Ajusta QTE_TRIB para outros padroes identificados (PCT ou PC)
id_m4 <- grep("(pc|pct|pctc)(\\s)?([0-9]){1,3}",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_m4 <- df_refri_nao_ajustado[id_m4,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_m4)
df_m4$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_m4$PROD_XPROD),"(pc|pct)(\\s)?([0-9]){1,3}"),"[0-9]{1,3}")) * df_m4$PROD_QCOM
df_m4$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_m4$PROD_XPROD),"(pc|pct)(\\s)?([0-9]){1,3}"),"[0-9]{1,3}"))

df_refrigerante <- rbind(df_refrigerante,df_m4)
rm(df_m4,id_m4)
####################################

##### PADROES EM PROD_XPROD PARA PROD_UCOM == "CX"
id_cx <- grep("^cx$",df_refri_nao_ajustado$PROD_UCOM,ignore.case = T)
df_cx <- df_refri_nao_ajustado[id_cx,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_cx)

df_cx$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_cx$PROD_XPROD), "\\s\\d{1,2}\\s?un")
df_cx$QTE_TRIB_AJUSTADO <- ifelse(is.na(df_cx$QTE_TRIB_AJUSTADO),str_extract(tolower(df_cx$PROD_XPROD),"\\d{1,2}\\s?pac"),df_cx$QTE_TRIB_AJUSTADO)
df_cx$QTE_TRIB_AJUSTADO <- ifelse(is.na(df_cx$QTE_TRIB_AJUSTADO),str_extract(tolower(df_cx$PROD_XPROD),"\\d{1,2}\\s?x\\s?\\d{1,2}"),df_cx$QTE_TRIB_AJUSTADO)
df_cx$QTE_TRIB_AJUSTADO <- ifelse(is.na(df_cx$QTE_TRIB_AJUSTADO),str_extract(tolower(df_cx$PROD_XPROD),"lt\\s?\\d{1,2}"),df_cx$QTE_TRIB_AJUSTADO)
df_cx$QTE_TRIB_AJUSTADO <- ifelse(is.na(df_cx$QTE_TRIB_AJUSTADO),str_extract(df_cx$PROD_XPROD,"\\s(12|24)\\s"),df_cx$QTE_TRIB_AJUSTADO)

df_cx$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_cx$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_cx$FATOR_MULTIPLICADOR <- ifelse(df_cx$FATOR_MULTIPLICADOR > 24,NA,df_cx$FATOR_MULTIPLICADOR)
df_cx$QTE_TRIB_AJUSTADO <- df_cx$FATOR_MULTIPLICADOR * df_cx$PROD_QCOM

df_cx_sem_ajuste <- df_cx%>%
  filter(is.na(FATOR_MULTIPLICADOR))

df_cx <- setdiff(df_cx,df_cx_sem_ajuste)
df_refrigerante <- rbind(df_refrigerante,df_cx)
df_refri_nao_ajustado <- rbind(df_refri_nao_ajustado,df_cx_sem_ajuste)

rm(df_cx,df_cx_sem_ajuste,id_cx)
gc(reset = T)
####################################


##### PADROES EM PROD_XPROD PARA PROD_UCOM == "CX" (NAO ENCONTRADOS ANTERIORMENTE)
id_cx <- grep("^cx$",df_refri_nao_ajustado$PROD_UCOM,ignore.case = T)
df_cx <- df_refri_nao_ajustado[id_cx,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_cx)

df_cx$QTE_TRIB_AJUSTADO <- str_extract(df_cx$PROD_XPROD,"\\s12|\\s24")
df_cx$FATOR_MULTIPLICADOR <- as.integer(df_cx$QTE_TRIB_AJUSTADO)
df_cx$QTE_TRIB_AJUSTADO <- df_cx$FATOR_MULTIPLICADOR * df_cx$PROD_QCOM

df_cx_sem_ajuste <- df_cx%>%
  filter(is.na(FATOR_MULTIPLICADOR))

df_cx <- setdiff(df_cx,df_cx_sem_ajuste)
df_refrigerante <- rbind(df_refrigerante,df_cx)
df_refri_nao_ajustado <- rbind(df_refri_nao_ajustado,df_cx_sem_ajuste)
rm(df_cx,df_cx_sem_ajuste,id_cx)
gc(reset = T)
####################################

##### PADROES EM PROD_XPROD PARA PROD_UCOM == "SX" (SiX) => 6 unidades
id_sx <- grep("^sx$",df_refri_nao_ajustado$PROD_UCOM,ignore.case = T)
df_sx <- df_refri_nao_ajustado[id_sx,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_sx)

# SEPARA PADRAO 99 x 99 EM PROD_XPROD
df_sx$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_sx$PROD_XPROD),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- str_extract(str_extract(tolower(df_sx$QTE_TRIB_AJUSTADO),"\\d{1,2}\\s?x"),"\\d{1,2}")
p2 <- str_extract(str_extract(tolower(df_sx$QTE_TRIB_AJUSTADO),"x\\s?\\d{1,2}"),"\\d{1,2}")
df_sx$FATOR_MULTIPLICADOR <- as.integer(ifelse(p1 > p2,p1,p2))
df_sx$QTE_TRIB_AJUSTADO <- df_sx$FATOR_MULTIPLICADOR * df_sx$PROD_QCOM

id_un <- grep("\\d{1,2}\\s?un",df_sx$PROD_XPROD,ignore.case = T)
df_sx$QTE_TRIB_AJUSTADO[id_un] <-str_extract(tolower(df_sx$PROD_XPROD[id_un]),"\\d{1,2}\\s?un")
df_sx$FATOR_MULTIPLICADOR[id_un] <- as.integer(str_extract(df_sx$QTE_TRIB_AJUSTADO[id_un],"\\d{1,2}"))
df_sx$QTE_TRIB_AJUSTADO[id_un] <- as.integer(df_sx$FATOR_MULTIPLICADOR[id_un] ) * df_sx$PROD_QCOM[id_un]

df_refrigerante <- rbind(df_refrigerante,df_sx)
rm(id_sx,id_un,df_sx,p1,p2)
gc(reset = T)
####################################

##### PADRAO PARA PROD_XPROD == "99 X 99"
id_xprd <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_xprd <- df_refri_nao_ajustado[id_xprd,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_xprd)

df_xprd$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_xprd$PROD_XPROD),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- str_extract(str_extract(tolower(df_xprd$QTE_TRIB_AJUSTADO),"\\d{1,2}\\s?x"),"\\d{1,2}")
p2 <- str_extract(str_extract(tolower(df_xprd$QTE_TRIB_AJUSTADO),"x\\s?\\d{1,2}"),"\\d{1,2}")
df_xprd$FATOR_MULTIPLICADOR <- as.integer(ifelse(p1 > p2,p1,p2))
df_xprd$QTE_TRIB_AJUSTADO <- df_xprd$FATOR_MULTIPLICADOR * df_xprd$PROD_QCOM

df_refrigerante <- rbind(df_refrigerante,df_xprd)
rm(id_xprd,df_xprd,p1,p2)
gc(reset = T)
####################################

##### PADRAO PARA PROD_XPROD == "99 UN"
id_un <- grep("\\d\\s?un",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_un <- df_refri_nao_ajustado[id_un,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_un)

df_un$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_un$PROD_XPROD),"\\d\\s?un")
df_un$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_un$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_un$QTE_TRIB_AJUSTADO <- df_un$FATOR_MULTIPLICADOR * df_un$PROD_QCOM

df_refrigerante <- rbind(df_refrigerante,df_un)
rm(id_un,df_un)
gc(reset = T)
####################################

##### PADRAO PARA PROD_XPROD == "\\d{1,2}\\s?u"
id_un <- grep("\\d{1,2}\\s?u\\s",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_un <- df_refri_nao_ajustado[id_un,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_un)

df_un$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_un$PROD_XPROD),"\\d{1,2}\\s?u\\s")
df_un$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_un$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_un$FATOR_MULTIPLICADOR <- ifelse(df_un$FATOR_MULTIPLICADOR > 24,NA,df_un$FATOR_MULTIPLICADOR)
df_un$QTE_TRIB_AJUSTADO <- as.integer(df_un$FATOR_MULTIPLICADOR) * df_un$PROD_QCOM

df_refrigerante <- rbind(df_refrigerante,df_un)
rm(df_un,id_un)
gc(reset = T)
####################################

##### PADROES ENCONTRADOS EM XPROD FILTRANDO UCOM = "CX"
df_cx <- df_refri_nao_ajustado%>%
  filter(PROD_UCOM == "CX")
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_cx)

id_cx1 <- grep("\\(\\d{1,2}\\)",df_cx$PROD_XPROD,ignore.case = T)
df_cx$QTE_TRIB_AJUSTADO[id_cx1] <- str_extract(tolower(df_cx$PROD_XPROD[id_cx1]),"\\(\\d{1,2}\\)")
df_cx$FATOR_MULTIPLICADOR[id_cx1] <- as.integer(str_extract(df_cx$QTE_TRIB_AJUSTADO[id_cx1],"\\d{1,2}"))
df_cx$QTE_TRIB_AJUSTADO[id_cx1] <- df_cx$FATOR_MULTIPLICADOR[id_cx1] * df_cx$PROD_QCOM[id_cx1]

id_cx2 <- grep("\\d{1,2}(u|p)",df_cx$PROD_XPROD,ignore.case = T)
df_cx$QTE_TRIB_AJUSTADO[id_cx2] <- str_extract(tolower(df_cx$PROD_XPROD[id_cx2]),"\\d{1,2}(u|p)")
df_cx$FATOR_MULTIPLICADOR[id_cx2] <- as.integer(str_extract(df_cx$QTE_TRIB_AJUSTADO[id_cx2],"\\d{1,2}"))
df_cx$QTE_TRIB_AJUSTADO[id_cx2] <- df_cx$FATOR_MULTIPLICADOR[id_cx2] * df_cx$PROD_QCOM[id_cx2]

df_cx_sem_ajuste <- df_cx%>%
  filter(is.na(FATOR_MULTIPLICADOR))
df_refri_nao_ajustado <- rbind(df_refri_nao_ajustado,df_cx_sem_ajuste)

df_cx <- setdiff(df_cx,df_cx_sem_ajuste)
df_refrigerante <- rbind(df_refrigerante,df_cx)
rm(df_cx,df_cx_sem_ajuste,id_cx1,id_cx2)
gc(reset = T)
####################################

##### PADROES EM Q A QTE ESTA EM PROD_UCOM
id_un <- grep("(pct|fd|am)\\d{1,4}",df_refri_nao_ajustado$PROD_UCOM,ignore.case = T)
df_un <- df_refri_nao_ajustado[id_un,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_un)

df_un$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_un$PROD_UCOM,"\\d{1,4}"))
df_un$FATOR_MULTIPLICADOR <- ifelse(df_un$FATOR_MULTIPLICADOR >= 4,df_un$FATOR_MULTIPLICADOR,NA)
df_un$QTE_TRIB_AJUSTADO <- df_un$FATOR_MULTIPLICADOR * df_un$PROD_QCOM

df_refrigerante <- rbind(df_refrigerante,df_un)
rm(df_un,id_un)
gc(reset = T)
####################################

##### PADROES ENCONTRADOS EM PROD_XPROD
id_1 <- grep("\\(\\d{1,2}\\)",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_1 <- df_refri_nao_ajustado[id_1,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_1)

df_1$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_1$PROD_XPROD),"\\(\\d{1,2}\\)")
df_1$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_1$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_1$QTE_TRIB_AJUSTADO <- df_1$FATOR_MULTIPLICADOR * df_1$PROD_QCOM
df_refrigerante <- rbind(df_refrigerante,df_1)

id_2 <- grep("(fd\\s?|c\\/\\s?)\\d{1,2}",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_2 <- df_refri_nao_ajustado[id_2,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_2)

df_2$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_2$PROD_XPROD),"(fd\\s?|c\\/\\s?)\\d{1,2}")
df_2$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_2$QTE_TRIB_AJUSTADO,"\\d{1,4}"))
df_2$FATOR_MULTIPLICADOR <- ifelse(df_2$FATOR_MULTIPLICADOR < 4 | df_2$FATOR_MULTIPLICADOR > 24, NA,df_2$FATOR_MULTIPLICADOR)
df_2$QTE_TRIB_AJUSTADO <- df_2$FATOR_MULTIPLICADOR * df_2$PROD_QCOM
df_refrigerante <- rbind(df_refrigerante,df_2)

rm(df_2,df_1,id_2,id_1)
gc(reset = T)
####################################

df_refrigerante <- rbind(df_refrigerante,df_refri_nao_ajustado)
rm(df_refri_nao_ajustado)

##### PADROES EM XPROD NAO IDENTIFICADOS ACIMA
df_refri_nao_ajustado <- df_refrigerante%>%
  filter(is.na(FATOR_MULTIPLICADOR))
df_refrigerante <- setdiff(df_refrigerante,df_refri_nao_ajustado)

i1 <- grep("(\\d{1,2}\\s?u|\\(\\d{1,2}\\))",df_refri_nao_ajustado$PROD_XPROD,ignore.case = T)
df_i1 <- df_refri_nao_ajustado[i1,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_i1)
df_i1$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_i1$PROD_XPROD),"(\\d{1,3}\\s?u|\\(\\d{1,2}\\))")
df_i1$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_i1$QTE_TRIB_AJUSTADO,"\\d{1,3}"))
df_i1$FATOR_MULTIPLICADOR <- ifelse(df_i1$FATOR_MULTIPLICADOR > 12 | df_i1$FATOR_MULTIPLICADOR < 1,NA,df_i1$FATOR_MULTIPLICADOR)
df_i1$QTE_TRIB_AJUSTADO <- df_i1$FATOR_MULTIPLICADOR * df_i1$PROD_QCOM

i2 <- grep("\\d{1,2}\\*",df_refri_nao_ajustado$PROD_XPROD)
df_i2 <- df_refri_nao_ajustado[i2,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_i2)
df_i2$QTE_TRIB_AJUSTADO <- str_extract(df_i2$PROD_XPROD,"\\d{1,2}\\*")
df_i2$FATOR_MULTIPLICADOR <- as.integer(str_extract(df_i2$QTE_TRIB_AJUSTADO,"\\d{1,2}"))
df_i2$FATOR_MULTIPLICADOR <-ifelse(df_i2$FATOR_MULTIPLICADOR > 24 | df_i2$FATOR_MULTIPLICADOR < 1,NA,df_i2$FATOR_MULTIPLICADOR)
df_i2$QTE_TRIB_AJUSTADO <- df_i2$FATOR_MULTIPLICADOR * df_i2$PROD_QCOM

df_refrigerante <- rbind(df_refrigerante,df_i1,df_i2)
rm(i1,i2,df_i1,df_i2)
gc(reset = T)
####################################


##### PADROES COM PROD_VUNCOM / PROD_VUNTRIB > 1 E COM RESTO == 0 => FATOR_MULTIPLICADOR
df_vlr <- df_refri_nao_ajustado%>%
  filter(PROD_VUNTRIB < PROD_VUNCOM)
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_vlr)

df_vlr$FATOR_MULTIPLICADOR <- ifelse(df_vlr$PROD_VUNCOM%%df_vlr$PROD_VUNTRIB != 0 ,NA,df_vlr$PROD_VUNCOM/df_vlr$PROD_VUNTRIB)

df_vlr_sem_ajuste <- df_vlr%>%
  filter(is.na(df_vlr$FATOR_MULTIPLICADOR))

df_vlr <- setdiff(df_vlr,df_vlr_sem_ajuste)
df_vlr$QTE_TRIB_AJUSTADO <- df_vlr$FATOR_MULTIPLICADOR * df_vlr$PROD_QCOM

df_refri_nao_ajustado <- rbind(df_refri_nao_ajustado,df_vlr_sem_ajuste)
df_refrigerante <- rbind(df_refrigerante,df_vlr)

rm(df_vlr,df_vlr_sem_ajuste)
gc(reset = T)
####################################

##### PADRAO "SX" EM PROD_UCOM - SX = "SIX" 6 UNIDADES POR EMBALAGEM
id_sx <- grep("sx",df_refri_nao_ajustado$PROD_UCOM,ignore.case = T)
df_sx <- df_refri_nao_ajustado[id_sx,]
df_refri_nao_ajustado <- setdiff(df_refri_nao_ajustado,df_sx)

df_sx$FATOR_MULTIPLICADOR <- 6
df_sx$QTE_TRIB_AJUSTADO <- df_sx$FATOR_MULTIPLICADOR * df_sx$PROD_QCOM
df_refrigerante <- rbind(df_refrigerante,df_sx)

rm(id_sx,df_sx)
gc(reset = T)
####################################

##### AJUSTA COLUNAS QTE_SEFAZ E VLR_UNITARIO_SEFAZ
df_refrigerante$QTE_SEFAZ <- as.double(df_refrigerante$QTE_TRIB_AJUSTADO)
df_refrigerante$VLR_UNITARIO_SEFAZ <- df_refrigerante$PROD_VPROD / df_refrigerante$QTE_SEFAZ
df_refrigerante$QTE_TRIB_AJUSTADO <- NULL
df_refrigerante$VUNTRIB_AJUSTADO <- NULL

df_refri_nao_ajustado$QTE_TRIB_AJUSTADO <- NULL
df_refri_nao_ajustado$VUNTRIB_AJUSTADO <- NULL

df_refrigerante <- rbind(df_refrigerante,df_refri_nao_ajustado)
rm(df_refri_nao_ajustado)
