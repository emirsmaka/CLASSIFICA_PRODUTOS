###############################################################################
##  SCRIPT.....: AJUSTES PARA REFRIGERANTES                                  ##
##  DETALHE....: AJUSTA VOLUME EM VAORES E UNIDADE MEDIDA EM L e ML          ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

#### CARREGA BIBLIOTECAS

######## SEPARAR REFRIGERANTES (EXCLUI REGISTROS COM CPROD_REFRIGERANTE_SEFAZ == -9999999)
df_nao_refrigerante <- df_refrigerante%>%
  filter(CPROD_REFRIGERANTE_SEFAZ == -9999999)

df_refrigerante <- anti_join(df_refrigerante,df_nao_refrigerante,by=c("IDNFE","DET_NITEM"))
########################################################

### SEPARA REGISTROS COM VLAORES INTEIROS E ML EM XPROD
id_ml <- grep("ml",df_refrigerante$PROD_XPROD,ignore.case = T)
refri_ml <- df_refrigerante[id_ml,]

df_refrigerante <- anti_join(df_refrigerante,refri_ml,by=c("IDNFE","DET_NITEM"))
### AJUSTA VOLUME_SEFAZ COM ML E L CONFORME O CASO
## VALOR ML INTEIRO NO XPROD
refri_ml$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(refri_ml$PROD_XPROD),"(\\d{3}\\s?ml|\\d{4}\\s?ml|\\d([[:punct:]]|\\s)\\d{3}\\s?ml)")
refri_ml$VOLUME_TRIB_AJUSTADO <- str_replace(refri_ml$VOLUME_TRIB_AJUSTADO,"\\.","")
refri_ml$VOLUME_SEFAZ <-as.double(str_extract(refri_ml$VOLUME_TRIB_AJUSTADO, "(\\d{4}|\\d{3})"))
refri_ml$VOLUME_SEFAZ <- ifelse(refri_ml$VOLUME_SEFAZ > 999, refri_ml$VOLUME_SEFAZ/1000,refri_ml$VOLUME_SEFAZ)
refri_ml$UN_MEDIDA_SEFAZ <- ifelse(refri_ml$VOLUME_SEFAZ > 99,"ML","L")
rm(id_ml)
########################################################

### SEPARA REGISTROS COM VALORES SIMILIARES A LITRO E ML EM XPROD
id_litro <- grep("\\d\\s?l",df_refrigerante$PROD_XPROD,ignore.case = T)
refri_litro <- df_refrigerante[id_litro,]
df_refrigerante <- anti_join(df_refrigerante,refri_litro,by=c("IDNFE","DET_NITEM"))

refri_litro$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(refri_litro$PROD_XPROD),"(\\d(\\,|\\.)?\\d{1,3}\\s?(l(\\s|$)|lt|litro)|\\d\\s?(l(\\s|$)|lt|litro))")
refri_litro$VOLUME_TRIB_AJUSTADO <- str_replace(refri_litro$VOLUME_TRIB_AJUSTADO,",",".")

refri_litro$VOLUME_SEFAZ <- as.double(str_extract(tolower(refri_litro$VOLUME_TRIB_AJUSTADO),"(\\d(\\.)?\\d{1,3}|\\d)"))
refri_litro$VOLUME_SEFAZ <- ifelse(refri_litro$VOLUME_SEFAZ < 1, refri_litro$VOLUME_SEFAZ*1000,refri_litro$VOLUME_SEFAZ)
refri_litro$UN_MEDIDA_SEFAZ <- ifelse(refri_litro$VOLUME_SEFAZ > 99,"ML","L")
########################################################

### SEPARA REGISTROS NAO AJUSTADOS
ml_sem_ajuste <- refri_ml%>%
  filter(is.na(VOLUME_SEFAZ))
refri_ml <- anti_join(refri_ml,ml_sem_ajuste,by=c("IDNFE","DET_NITEM"))

lt_sem_ajuste <- refri_litro%>%
  filter(is.na(VOLUME_SEFAZ))
refri_litro <- anti_join(refri_litro,lt_sem_ajuste,by=c("IDNFE","DET_NITEM"))

refri_sem_ajuste <- rbind(ml_sem_ajuste,lt_sem_ajuste,df_refrigerante)

df_refri_ajustado <- rbind(refri_litro,refri_ml)

rm(ml_sem_ajuste,lt_sem_ajuste,refri_litro,refri_ml,df_refrigerante,id_litro)
gc(reset = T)
########################################################

##### AJUSTA REGISTROS QUE ESCAPARAM AOS PADROES ANTERIORES
i1 <- grep("\\d{1,2}x\\d\\d\\d",refri_sem_ajuste$PROD_XPROD,ignore.case = T)
df_i1 <- refri_sem_ajuste[i1,]
refri_sem_ajuste <- anti_join(refri_sem_ajuste,df_i1,by=c("IDNFE","DET_NITEM"))

df_i1$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_i1$PROD_XPROD),"x\\d{3,4}g?")
df_i1$VOLUME_SEFAZ <- as.double(str_extract(df_i1$VOLUME_TRIB_AJUSTADO ,"\\d{3,4}"))
df_i1$UN_MEDIDA_SEFAZ <- ifelse(df_i1$VOLUME_SEFAZ > 999,"L","ML")
df_i1$VOLUME_SEFAZ <- ifelse(df_i1$VOLUME_SEFAZ > 999,df_i1$VOLUME_SEFAZ/1000,df_i1$VOLUME_SEFAZ)
df_i1$UN_MEDIDA_SEFAZ <- ifelse(grepl("g",df_i1$VOLUME_TRIB_AJUSTADO),"GR",df_i1$UN_MEDIDA_SEFAZ)

df_refri_ajustado <- rbind(df_refri_ajustado,df_i1)

i2 <- grep("\\d(\\,|\\.)?\\d{1,2}l",refri_sem_ajuste$PROD_XPROD,ignore.case = T)
df_i2 <- refri_sem_ajuste[i2,]
refri_sem_ajuste <- anti_join(refri_sem_ajuste,df_i2,by=c("IDNFE","DET_NITEM"))

df_i2$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_i2$PROD_XPROD),"\\d{1,2}(\\,|\\.)?\\d{1,5}l(u)?")
df_i2$VOLUME_TRIB_AJUSTADO <- str_replace(df_i2$VOLUME_TRIB_AJUSTADO,",",".")
df_i2$VOLUME_SEFAZ <- as.double(str_extract(df_i2$VOLUME_TRIB_AJUSTADO,"\\d{1,2}(\\,|\\.)?\\d{1,5}"))
df_i2$VOLUME_SEFAZ <- ifelse(df_i2$VOLUME_SEFAZ < 1,df_i2$VOLUME_SEFAZ*1000,df_i2$VOLUME_SEFAZ)
df_i2$VOLUME_SEFAZ <- ifelse(df_i2$VOLUME_SEFAZ > 999,df_i2$VOLUME_SEFAZ/1000,df_i2$VOLUME_SEFAZ)
df_i2$UN_MEDIDA_SEFAZ <- ifelse(df_i2$VOLUME_SEFAZ > 99,"ML","L")

df_refri_ajustado <- rbind(df_refri_ajustado,df_i2)

rm(i1,i2,df_i1,df_i2)
gc(reset = T)
########################################################


### AJUSTA REGISTROS QUE ESCAPARAM AOS PADROES ANTERIORES
refri_sem_ajuste$PROD_XPROD_LIMPO <- refri_sem_ajuste$PROD_XPROD
refri_sem_ajuste$PROD_XPROD_LIMPO <- str_replace_all(tolower(refri_sem_ajuste$PROD_XPROD_LIMPO),"\\(|\\)"," ")

### VOLUMES PADROES PARA LITRO E ML
volume_ml <- c(250,290,269,510,500,600,350,355,310,237,300,660)
volume_litro <- c(1.5,2.5,3.0,2.0)
###################################

#### IDENTIFICA PADROES NUMERICOS EM XPROD QUE PODEM SER VOLUME
i1 <- grep("\\s\\d{3}$",refri_sem_ajuste$PROD_XPROD_LIMPO,ignore.case = T)
df_i1 <- refri_sem_ajuste[i1,]
refri_sem_ajuste <- anti_join(refri_sem_ajuste,df_i1,by=c("IDNFE","DET_NITEM"))
#
i2 <- grep("\\s\\d{3}(x|\\s)\\d{2}",refri_sem_ajuste$PROD_XPROD_LIMPO,ignore.case = T)
df_i2 <- refri_sem_ajuste[i2,]
refri_sem_ajuste <- anti_join(refri_sem_ajuste,df_i2,by=c("IDNFE","DET_NITEM"))
#
i3 <- grep("\\s\\d{3}",refri_sem_ajuste$PROD_XPROD_LIMPO,ignore.case = T)
df_i3 <- refri_sem_ajuste[i3,]
refri_sem_ajuste <- anti_join(refri_sem_ajuste,df_i3,by=c("IDNFE","DET_NITEM"))
#
i4 <- grep("\\d\\,\\d",refri_sem_ajuste$PROD_XPROD_LIMPO,ignore.case = T)
df_i4 <- refri_sem_ajuste[i4,]
refri_sem_ajuste <- anti_join(refri_sem_ajuste,df_i4,by=c("IDNFE","DET_NITEM"))
########################################################################


df_i1$VOLUME_TRIB_AJUSTADO <- str_extract(df_i1$PROD_XPROD,"\\s\\d{3}$")
df_i2$VOLUME_TRIB_AJUSTADO <- str_extract(str_extract(tolower(df_i2$PROD_XPROD),"\\s\\d{3}(x|\\s)\\d{2}"),"\\d{3}")
df_i3$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_i3$PROD_XPROD),"\\s\\d{3}")
df_i4$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_i4$PROD_XPROD),"\\d\\,\\d")
## CONCATENA DATAFRAMES
df_i <- rbind(df_i1,df_i2,df_i3,df_i4)
rm(df_i1,df_i2,df_i3,df_i4,i1,i2,i3,i4)

df_i$VOLUME_TRIB_AJUSTADO <- str_replace(df_i$VOLUME_TRIB_AJUSTADO,",",".")
df_i$VOLUME_TRIB_AJUSTADO <- as.double(df_i$VOLUME_TRIB_AJUSTADO)

df_i$VOLUME_SEFAZ <- ifelse(df_i$VOLUME_TRIB_AJUSTADO %in% volume_ml | df_i$VOLUME_TRIB_AJUSTADO %in% volume_litro , df_i$VOLUME_TRIB_AJUSTADO,NA)
df_i$UN_MEDIDA_SEFAZ <- ifelse(df_i$VOLUME_SEFAZ > 99,"ML","L")
########################################################
df_i$PROD_XPROD_LIMPO <- NULL
refri_sem_ajuste$PROD_XPROD_LIMPO <- NULL
df_refrigerante <- rbind(df_refri_ajustado,df_i)
## ATRIBUI VALORES PARA COLUNAS VOLUME E UNID MEDIDA NAO IDENTIFICADOS
refri_sem_ajuste$VOLUME_SEFAZ <- ifelse(is.na(refri_sem_ajuste$VOLUME_SEFAZ),"SEM VOLUME",refri_sem_ajuste$VOLUME_SEFAZ)
refri_sem_ajuste$UN_MEDIDA_SEFAZ <- ifelse(is.na(refri_sem_ajuste$UN_MEDIDA_SEFAZ),"SEM UNIDADE MEDIDA",refri_sem_ajuste$UN_MEDIDA_SEFAZ)
##
df_refrigerante <- rbind(df_refrigerante,refri_sem_ajuste,df_nao_refrigerante)
df_refrigerante$VOLUME_TRIB_AJUSTADO <- NULL
df_refrigerante$QTE_TRIB_AJUSTADO <- NULL
df_refrigerante$VUNTRIB_AJUSTADO <- NULL
rm(df_nao_refrigerante,df_i,volume_litro,volume_ml,df_refri_ajustado,refri_sem_ajuste)
gc(reset = T)
######## FIM AJUSTES VOLUME E UNIDADE DE MEDIDA ########

