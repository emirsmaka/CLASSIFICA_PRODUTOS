###############################################################################
##  SCRIPT.....: AJUSTES PARA CERVEJAS                                       ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR     ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################




### Seleciona qtrib definidos com duzias
id_dz <- grep("^dz|^du",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja$QTE_TRIB_AJUSTADO[id_dz] <- df_cerveja$PROD_QCOM[id_dz] * 12
df_cerveja$FATOR_MULTIPLICADOR[id_dz] <- 12
rm(id_dz)

### Seleciona Padrao PROD_UCOM descrito como "cxdd" onde dd representa o  numero de unidades na embalagem

id_cx<-grep("c[x]\\s?\\d{1,4}",df_cerveja$PROD_UCOM, ignore.case = T)
df_cerveja$QTE_TRIB_AJUSTADO[id_cx]<- as.integer(str_extract(str_extract(tolower(df_cerveja$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}")) * df_cerveja$PROD_QCOM[id_cx]
df_cerveja$FATOR_MULTIPLICADOR[id_cx] <- as.double(str_extract(str_extract(tolower(df_cerveja$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}"))

### Seleciona qtrib definidos corretamente como unidade
id_utrib<-  grep("^u|^lta|^lt|^lat|^ga|^gr|^gfa|grf|gf|pec|vd|tubo",df_cerveja$PROD_UCOM,ignore.case = T)
df_cerveja$QTE_TRIB_AJUSTADO[id_utrib] <- df_cerveja$PROD_QCOM[id_utrib]
df_cerveja$FATOR_MULTIPLICADOR[id_utrib] <- 1
rm(id_utrib,id_cx)


### SEPARA REGISTROS AJUSTADOS
id_null <- ifelse(is.na(df_cerveja$QTE_TRIB_AJUSTADO), TRUE,FALSE)
df_bebidas_null <- df_cerveja[id_null,]
df_bebidas_notnull <- setdiff(df_cerveja,df_cerveja[id_null,])
rm(id_null)

gc(reset = TRUE)


#### REGEX IDENTIFICAR QTE_TRIB_AJUSTADO (QDO UNIDADE TRIB FOR CAIXA)

id_m1 <- grep("[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d",df_bebidas_null$PROD_XPROD,ignore.case = T) # cria indice dos padroes no data frame
df_bebidas_null$QTE_TRIB_AJUSTADO[id_m1]<- as.integer(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m1]),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))*df_bebidas_null$PROD_QCOM[id_m1]
df_bebidas_null$FATOR_MULTIPLICADOR[id_m1] <- as.double(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m1]),"[0-9]{1,3}(\\s)?x(\\s)?\\d\\d\\d"),"\\d{1,3}"))
###
id_m2 <- grep("\\sx\\s(\\d\\d)\\d?\\s",df_bebidas_null$PROD_XPROD,ignore.case = T)
df_bebidas_null$QTE_TRIB_AJUSTADO[id_m2] <- as.integer(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m2]),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}")) * df_bebidas_null$PROD_QCOM[id_m2]
df_bebidas_null$FATOR_MULTIPLICADOR[id_m2] <- as.double(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m2]),"\\sx\\s(\\d\\d)\\d?\\s"),"[0-9]{2}"))
###
id_m3 <- grep("\\sc[x]?(.)?\\d{1,4}",df_bebidas_null$PROD_XPROD,ignore.case = T)
df_bebidas_null$QTE_TRIB_AJUSTADO[id_m3] <- as.integer(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m3]),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}")) * df_bebidas_null$PROD_QCOM[id_m3]
df_bebidas_null$FATOR_MULTIPLICADOR[id_m3] <- as.double(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m3]),"\\sc[x]?(.)?\\d{1,4}"),"[0-9]{1,4}"))
###
rm(id_m1,id_m2,id_m3)
####


#### UNIFICA AS TABELAS SEPARADAS ACIMA
rm(df_cerveja)
df_cerveja<-rbind(df_bebidas_notnull,df_bebidas_null)
rm(df_bebidas_notnull,df_bebidas_null)


####################################

#### SEPARA NOVAMENTE REGISTROS AJUSTADOS DE NAO AJUSTADOS

id_bco_qte_aj<-ifelse(is.na(df_cerveja$QTE_TRIB_AJUSTADO),TRUE,FALSE)
df_bebidas_null<-df_cerveja[id_bco_qte_aj,]
df_bebidas_notnull <- setdiff(df_cerveja,df_cerveja[id_bco_qte_aj,])
rm(id_bco_qte_aj)
rm(df_cerveja)
####

### Ajusta QTE_TRIB para outros padroes identificados (PCT ou PC)
id_m4 <- grep("(pc|pct|pctc)(\\s)?([0-9]){1,3}",df_bebidas_null$PROD_XPROD,ignore.case = T)
df_bebidas_null$QTE_TRIB_AJUSTADO[id_m4] <- as.integer(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m4]),"(pc|pct)(\\s)?([0-9]){1,3}"),"[0-9]{1,3}")) * df_bebidas_null$PROD_QCOM[id_m4]
df_bebidas_null$FATOR_MULTIPLICADOR[id_m4] <- as.double(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_m4]),"(pc|pct)(\\s)?([0-9]){1,3}"),"[0-9]{1,3}"))

## AJUSTA padrao ex: PCT|PC 1 X 12
id_pct <- grep("(pc|pct)(\\s)?([0-9]){1,3}(\\s)?x(\\s)?([0-9]){1,3}",df_bebidas_null$PROD_XPROD,ignore.case = T)
x <- as.integer(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_pct]),"(pc|pct)(\\s)?([0-9]){1,3}"),"[0-9]{1,3}"))
y <- as.integer(str_extract(str_extract(tolower(df_bebidas_null$PROD_XPROD[id_pct]),"x(\\s)?[0-9]{1,3}"),"[0-9]{1,3}"))
df_bebidas_null$FATOR_MULTIPLICADOR[id_pct] <- ifelse(y>x,y,x)
df_bebidas_null$QTE_TRIB_AJUSTADO[id_pct] <- df_bebidas_null$FATOR_MULTIPLICADOR[id_pct] * df_bebidas_null$PROD_QCOM[id_pct]
rm(id_m4,id_pct,x,y)
####

#id_m5 <- grep("([0-9]){1,2}(\\s)?(u|un)",df_qte_bco$PROD_XPROD,ignore.case = T)
#df_qte_bco$QTE_TRIB_AJUSTADO[id_m5] <- as.integer(str_extract(str_extract(tolower(df_qte_bco$PROD_XPROD[id_m5]),"([0-9]){1,2}(\\s)?(u|un)"),"[0-9]{1,3}")) * df_qte_bco$PROD_QCOM[id_m5]

####
###################

#### SEPARA REGISTROS AJUSTADOS DE NAO AJUSTADOS

id_null2<-ifelse(is.na(df_bebidas_null$QTE_TRIB_AJUSTADO),TRUE,FALSE)
df_null <- df_bebidas_null[id_null2,]
df_notnull<-setdiff(df_bebidas_null,df_bebidas_null[id_null2,])
rm(df_bebidas_null)
id_m5 <- grep("\\d{1,2}\\s?(un)",df_null$PROD_XPROD,ignore.case = T)
df_null$QTE_TRIB_AJUSTADO[id_m5]<-as.integer(str_extract(str_extract(tolower(df_null$PROD_XPROD[id_m5]),"\\d{1,2}\\s?(un)"),"[0-9]{1,2}")) * df_null$PROD_QCOM[id_m5]
df_null$FATOR_MULTIPLICADOR[id_m5] <- as.double(str_extract(str_extract(tolower(df_null$PROD_XPROD[id_m5]),"\\d{1,2}\\s?(un)"),"[0-9]{1,2}"))
df_bebidas_null<-rbind(df_notnull,df_null)
rm(df_notnull,df_null,id_m5,id_null2)
df_cerveja<-rbind(df_bebidas_notnull,df_bebidas_null)
rm(df_bebidas_notnull,df_bebidas_null)

####

#### SEPARA REGISTROS NAO AJUSTADOS PELOS PADROES ACIMA
df_cerveja_sem_ajuste <- df_cerveja%>%
  filter(is.na(FATOR_MULTIPLICADOR))
df_cerveja <- setdiff(df_cerveja,df_cerveja_sem_ajuste)
#### AJSUTA COLUNAS PARA PADRAO DE QUANTIDADE 6,12,24 ENCONTRADOS NO PROD_XPROD
id <- grep("\\s6\\s|\\s12|\\s24",df_cerveja_sem_ajuste$PROD_XPROD,ignore.case = T)
df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id] <- str_extract(df_cerveja_sem_ajuste$PROD_XPROD[id],"\\s6\\s|\\s12|\\s24")
df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR <- as.double(df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR)
df_cerveja_sem_ajuste$QTE_TRIB_AJUSTADO[id] <- as.double(df_cerveja_sem_ajuste$PROD_QCOM[id]*df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id])
df_cerveja_sem_ajuste$VUNTRIB_AJUSTADO[id] <- df_cerveja_sem_ajuste$PROD_VUNCOM[id] / df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id]
####
df_cerveja <- rbind(df_cerveja,df_cerveja_sem_ajuste)
rm(df_cerveja_sem_ajuste,id)
####

#### SEPARA REGISTROS NAO AJUSTADOS PELOS PADROES ACIMA
df_cerveja_sem_ajuste <- df_cerveja%>%
  filter(is.na(FATOR_MULTIPLICADOR))
df_cerveja <- setdiff(df_cerveja,df_cerveja_sem_ajuste)

#### AJUSTA COLUNAS PARA PADRAO "06$|06pac|\\d{1,2}\\sgf" ENCONTRADO EM PORD_XPROD
id <- grep("06$|06pac|\\d{1,2}\\sgf",df_cerveja_sem_ajuste$PROD_XPROD,ignore.case = T)
df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id] <- str_extract(df_cerveja_sem_ajuste$PROD_XPROD[id],"06$|06pac|\\d{1,2}\\sgf")
df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id] <-str_extract(df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id],"\\d{1,2}")
df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR <- as.double(df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR)
df_cerveja_sem_ajuste$QTE_TRIB_AJUSTADO[id] <- as.double(df_cerveja_sem_ajuste$PROD_QCOM[id]*df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id])
df_cerveja_sem_ajuste$VUNTRIB_AJUSTADO[id] <- df_cerveja_sem_ajuste$PROD_VUNCOM[id] / df_cerveja_sem_ajuste$FATOR_MULTIPLICADOR[id]
####
df_cerveja <- rbind(df_cerveja,df_cerveja_sem_ajuste)
rm(df_cerveja_sem_ajuste,id)
####
df_cerveja <- rbind(df_cerveja,df_nao_cerveja)

#### AJUSTA CAMPO PROD_QCOM COM CASAS DECIMAIS
df_cerveja$QTE_SEFAZ<-as.double(df_cerveja$QTE_TRIB_AJUSTADO)
df_cerveja$QTE_SEFAZ<-ifelse(df_cerveja$QTE_SEFAZ%%1 != 0,0,df_cerveja$QTE_SEFAZ)
df_cerveja$QTE_TRIB_AJUSTADO<-NULL

#### AJUSTA VALOR TRIB UNITARIO
df_cerveja$VLR_UNITARIO_SEFAZ<-ifelse(df_cerveja$QTE_SEFAZ < 1,0,as.double(df_cerveja$PROD_VPROD/df_cerveja$QTE_SEFAZ))
df_cerveja$VUNTRIB_AJUSTADO<-NULL
df_cerveja$VLR_UNITARIO_SEFAZ<-round(df_cerveja$VLR_UNITARIO_SEFAZ,10)

###################

tb_final <- df_cerveja%>%
  select(IDNFE,DET_NITEM,CPROD_CERVEJA_SEFAZ,FATOR_MULTIPLICADOR,UNIDADE_SEFAZ,VOLUME_SEFAZ,UN_MEDIDA_SEFAZ,QTE_SEFAZ,VLR_UNITARIO_SEFAZ)

fwrite(tb_final, "./CERVEJA2020_FINAL.csv", sep = ";")

rm(df_cerveja,tb_final,df_nao_cerveja)
gc(reset = T)
