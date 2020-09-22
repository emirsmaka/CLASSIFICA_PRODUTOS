###############################################################################
##  SCRIPT.....: AJUSTES PARA REFRIGERANTES                                  ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR     ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################




### Seleciona qtrib definidos com duzias
id_dz <- grep("^dz|^du",df_refrigerante$PROD_UCOM,ignore.case = T)
df_refrigerante$QTE_TRIB_AJUSTADO[id_dz] <- df_refrigerante$PROD_QCOM[id_dz] * 12
df_refrigerante$FATOR_MULTIPLICADOR[id_dz] <- 12
rm(id_dz)

### Seleciona Padrao PROD_UCOM descrito como "cxdd" onde dd representa o  numero de unidades na embalagem

id_cx<-grep("c[x]\\s?\\d{1,4}",df_refrigerante$PROD_UCOM, ignore.case = T)
df_refrigerante$QTE_TRIB_AJUSTADO[id_cx]<- as.integer(str_extract(str_extract(tolower(df_refrigerante$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}")) * df_refrigerante$PROD_QCOM[id_cx] 
df_refrigerante$FATOR_MULTIPLICADOR[id_cx] <- as.double(str_extract(str_extract(tolower(df_refrigerante$PROD_UCOM[id_cx]),"c[x]\\s?\\d{1,4}"),"[0-9]{1,4}"))

### Seleciona qtrib definidos corretamente como unidade
id_utrib<-  grep("^u|^lta|^lt|^lat|^ga|^gr|^gfa|grf|gf|pec|vd|tubo",df_refrigerante$PROD_UCOM,ignore.case = T)
df_refrigerante$QTE_TRIB_AJUSTADO[id_utrib] <- df_refrigerante$PROD_QCOM[id_utrib]
df_refrigerante$FATOR_MULTIPLICADOR[id_utrib] <- 1
rm(id_utrib,id_cx)


### SEPARA REGISTROS AJUSTADOS
id_null <- ifelse(df_refrigerante$QTE_TRIB_AJUSTADO == "", TRUE,FALSE)
df_bebidas_null <- df_refrigerante[id_null,]
df_bebidas_notnull <- setdiff(df_refrigerante,df_refrigerante[id_null,])
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
rm(df_refrigerante)
df_refrigerante<-rbind(df_bebidas_notnull,df_bebidas_null)
rm(df_bebidas_notnull,df_bebidas_null)


####################################

#### SEPARA NOVAMENTE REGISTROS AJUSTADOS DE NAO AJUSTADOS

id_bco_qte_aj<-ifelse(df_refrigerante$QTE_TRIB_AJUSTADO == "",TRUE,FALSE)
df_bebidas_null<-df_refrigerante[id_bco_qte_aj,]
df_bebidas_notnull <- setdiff(df_refrigerante,df_refrigerante[id_bco_qte_aj,])
rm(id_bco_qte_aj)
rm(df_refrigerante)
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

id_null2<-ifelse(df_bebidas_null$QTE_TRIB_AJUSTADO == "",TRUE,FALSE)
df_null <- df_bebidas_null[id_null2,]
df_notnull<-setdiff(df_bebidas_null,df_bebidas_null[id_null2,])
rm(df_bebidas_null)
id_m5 <- grep("\\d{1,2}\\s?(un)",df_null$PROD_XPROD,ignore.case = T)
df_null$QTE_TRIB_AJUSTADO[id_m5]<-as.integer(str_extract(str_extract(tolower(df_null$PROD_XPROD[id_m5]),"\\d{1,2}\\s?(un)"),"[0-9]{1,2}")) * df_null$PROD_QCOM[id_m5]
df_null$FATOR_MULTIPLICADOR[id_m5] <- as.double(str_extract(str_extract(tolower(df_null$PROD_XPROD[id_m5]),"\\d{1,2}\\s?(un)"),"[0-9]{1,2}"))
df_bebidas_null<-rbind(df_notnull,df_null)
rm(df_notnull,df_null,id_m5,id_null2)
df_refrigerante<-rbind(df_bebidas_notnull,df_bebidas_null)
rm(df_bebidas_notnull,df_bebidas_null)

####

#### AJUSTA CAMPO PROD_QCOM COM CASAS DECIMAIS
df_refrigerante$QTE_SEFAZ<-as.double(df_refrigerante$QTE_TRIB_AJUSTADO)
df_refrigerante$QTE_SEFAZ<-ifelse(df_refrigerante$QTE_SEFAZ%%1 != 0,0,df_refrigerante$QTE_SEFAZ)
df_refrigerante$QTE_TRIB_AJUSTADO<-NULL

#### AJUSTA VALOR TRIB UNITARIO
df_refrigerante$VLR_UNITARIO_SEFAZ<-ifelse(df_refrigerante$QTE_SEFAZ < 1,0,as.double(df_refrigerante$PROD_VPROD/df_refrigerante$QTE_SEFAZ)) 
df_refrigerante$VUNTRIB_AJUSTADO<-NULL
df_refrigerante$VLR_UNITARIO_SEFAZ<-round(df_refrigerante$VLR_UNITARIO_SEFAZ,10)

###################

### SALVA ARQUIVO CLASSIFICADO E AJUSTADO EM FORMATO CSV
data <- str_replace_all(Sys.time(),"\\s|\\:","_")
path <- "./"
arquivo <- "REFRIGERANTE_2017.csv"

gravar <- paste(path,data,arquivo,sep = "")

fwrite(df_refrigerante,gravar,sep = ";", append = FALSE)

rm(df_refrigerante,data,path,gravar,arquivo)
gc(reset = T)


























