
df_cerveja_2019 <-  fread("./CERVEJA2019_CLASS.csv",sep = ";")



df_cerveja_2019[,"QTE_TRIB_AJUSTADO"] <- as.character()
df_cerveja_2019[,"VOLUME_TRIB_AJUSTADO"] <- as.character()
df_cerveja_2019[,"VUNTRIB_AJUSTADO"] <- as.double()
df_cerveja_2019[,"FATOR_MULTIPLICADOR"] <- as.double()
df_cerveja_2019[,"UNIDADE_SEFAZ"] <- "UN"
df_cerveja_2019[,"VOLUME_SEFAZ"] <- as.double()
df_cerveja_2019[,"UN_MEDIDA_SEFAZ"] <- as.character()
########################################################

df_nao_cerveja <- df_cerveja_2019%>%
  filter(V1 == -9999999 | V1 == 4000899)

df_cerveja_2019 <- setdiff(df_cerveja_2019,df_nao_cerveja)


### Seleciona qtrib definidos com duzias
id_dz <- grep("^dz|^du",df_cerveja_2019$PROD_UCOM,ignore.case = T)
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_dz] <- df_cerveja_2019$PROD_QCOM[id_dz] * 12
df_cerveja_2019$FATOR_MULTIPLICADOR[id_dz] <- 12


### Seleciona Padrao PROD_UCOM descrito como "cxdd" onde dd representa o  numero de unidades na embalagem
id_cx<-grep("c(x)?\\s?\\d{1,4}",df_cerveja_2019$PROD_UCOM, ignore.case = T)
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_cx]<- as.integer(str_extract(str_extract(tolower(df_cerveja_2019$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}")) * df_cerveja_2019$PROD_QCOM[id_cx]
df_cerveja_2019$FATOR_MULTIPLICADOR[id_cx] <- as.double(str_extract(str_extract(tolower(df_cerveja_2019$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}"))


###################### SEPARA REGISTROS JÁ AJUSTADOS ######################
df_cerveja_ajustada <- rbind(df_cerveja_2019[id_dz],df_cerveja_2019[id_cx])
df_cerveja_2019 <- setdiff(df_cerveja_2019,df_cerveja_ajustada)
rm(id_cx,id_dz)
###################### FIM SEPARA REGISTROS JÁ AJUSTADOS ######################


#### REGEX IDENTIFICAR QTE_TRIB_AJUSTADO (QDO UNIDADE TRIB FOR CAIXA)

id_m1 <- grep("[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d",df_cerveja_2019$PROD_XPROD,ignore.case = T) # cria indice dos padroes no data frame
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_m1]<- as.integer(str_extract(str_extract(tolower(df_cerveja_2019$PROD_XPROD[id_m1]),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))*df_cerveja_2019$PROD_QCOM[id_m1]
df_cerveja_2019$FATOR_MULTIPLICADOR[id_m1] <- as.double(str_extract(str_extract(tolower(df_cerveja_2019$PROD_XPROD[id_m1]),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))
###
id_m2 <- grep("\\sx\\s(\\d\\d)\\d?\\s",df_cerveja_2019$PROD_XPROD,ignore.case = T)
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_m2] <- as.integer(str_extract(str_extract(tolower(df_cerveja_2019$PROD_XPROD[id_m2]),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}")) * df_cerveja_2019$PROD_QCOM[id_m2]
df_cerveja_2019$FATOR_MULTIPLICADOR[id_m2] <- as.double(str_extract(str_extract(tolower(df_cerveja_2019$PROD_XPROD[id_m2]),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}"))
###
id_m3 <- grep("\\scx?(\\s)?\\d{1,4}|\\sc(\\/)?\\d{1,4}",df_cerveja_2019$PROD_XPROD,ignore.case = T)
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_m3] <- as.integer(str_extract(str_extract(tolower(df_cerveja_2019$PROD_XPROD[id_m3]),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}")) * df_cerveja_2019$PROD_QCOM[id_m3]
df_cerveja_2019$FATOR_MULTIPLICADOR[id_m3] <- as.double(str_extract(str_extract(tolower(df_cerveja_2019$PROD_XPROD[id_m3]),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}"))
###

df_cerveja_ajustada <- rbind(df_cerveja_ajustada, df_cerveja_2019[id_m1],df_cerveja_2019[id_m2],df_cerveja_2019[id_m3])
df_cerveja_2019 <- setdiff(df_cerveja_2019,df_cerveja_ajustada)
rm(id_m1,id_m2,id_m3)
####

#### REGEX PARA PROD_QCOM COM UN + NUMEROS (ESSES INDICAM A QTE DE UNIDADES)
id_un1 <- grep("un(.)?\\d{1,4}",df_cerveja_2019$PROD_UCOM,ignore.case = T)
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_un1]<- as.integer(str_extract(str_extract(tolower(df_cerveja_2019$PROD_UCOM[id_un1]),"un(.)?\\d{1,4}"),"\\d{1,4}"))*df_cerveja_2019$PROD_QCOM[id_un1]
df_cerveja_2019$FATOR_MULTIPLICADOR[id_un1] <- as.double(str_extract(str_extract(tolower(df_cerveja_2019$PROD_UCOM[id_un1]),"un(.)?\\d{1,4}"),"\\d{1,4}"))

## SEPARA REGISTROS AJUSTADOS
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja_2019[id_un1])
df_cerveja_2019 <- setdiff(df_cerveja_2019,df_cerveja_ajustada)
rm(id_un1)
#############################

#### REGEX PARA PADRAO CAIXA EM PROD_XPROD
id_cx3 <- grep("\\sc(x)?[[:punct:]]?\\d{1,4}",df_cerveja_2019$PROD_XPROD,ignore.case = T)
source("./SCRIPTS/FUNCAO_OUTLIER")
id_outlier <- out_superior(df_cerveja_2019$PROD_VUNCOM[id_cx3])
df_cerveja_caixa <- df_cerveja_2019[id_cx3,][id_outlier]

df_cerveja_caixa$QTE_TRIB_AJUSTADO <- as.integer(str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"\\sc(x)?[[:punct:]]?\\d{1,4}"),"\\d{1,4}"))*df_cerveja_caixa$PROD_QCOM
df_cerveja_caixa$FATOR_MULTIPLICADOR <- as.double(str_extract(str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"\\sc(x)?[[:punct:]]?\\d{1,4}"),"\\d{1,4}"))
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja_caixa)
df_cerveja_caixa$QTE_TRIB_AJUSTADO <- as.character(df_cerveja_caixa$QTE_TRIB_AJUSTADO)
## SEPARA REGISTROS AJUSTADOS
df_cerveja_2019 <- setdiff(df_cerveja_2019,df_cerveja_caixa)
rm(df_cerveja_caixa,id_cx3,id_outlier,out_superior)
###########################################################################333

#### AJUSTA REGISTROS COM UNIDADE EM PROD_UCOM
id_unidade <- grep("^un",df_cerveja_2019$PROD_UCOM,ignore.case = T)
id_outlier <- out_superior(df_cerveja_2019$PROD_VUNCOM[id_unidade])
## SEPARA REGISTROS COM OUTLIERS EM PROD_VUNPROD
df_cerveja_un_outlier <- df_cerveja_2019[id_unidade,][id_outlier]
df_cerveja_2019 <- setdiff(df_cerveja_2019, df_cerveja_un_outlier)
######
id_unidade <- grep("^un",df_cerveja_2019$PROD_UCOM,ignore.case = T)
df_cerveja_2019$QTE_TRIB_AJUSTADO[id_unidade] <- df_cerveja_2019$PROD_QCOM[id_unidade]
df_cerveja_2019$FATOR_MULTIPLICADOR[id_unidade] <- 1
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cerveja_2019[id_unidade])
df_cerveja_2019 <- setdiff(df_cerveja_2019,df_cerveja_2019[id_unidade])
rm(id_outlier,id_unidade)
##################################################

#### REGEX Q IDENTIFICA CAIXAS EM PROD_XPROD COM PADRAO == 99U(N)
id_caixa <- grep("\\d{1,2}u(n)?",df_cerveja_2019$PROD_XPROD,ignore.case = T)
df_cerveja_caixa <- df_cerveja_2019[id_caixa]
## EXTRAI MULTIPLICADOR DE PROD_XPROD Q INDICA A QTDE DE UNIDADES EM UMA CAIXA - PADRAO 9X9UN
multi_caixa <- str_extract(tolower(df_cerveja_caixa$PROD_XPROD),"\\d{1,2}x")
multi_caixa <- str_extract(multi_caixa,"\\d{1,2}")












































