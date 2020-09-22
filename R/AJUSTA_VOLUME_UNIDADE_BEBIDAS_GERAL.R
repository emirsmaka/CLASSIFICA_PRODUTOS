#################################################################################################
##  SCRIPT.....: AJUSTES VOLUME E UNIDADE DE BEBIDAS EM GERAL, EXCETO REFRIGERANTES E CERVEJAS ##
##  DETALHE....: AJUSTA VOLUME EM VALORES E UNIDADE MEDIDA EM L, ML e G                        ##
##  DATA.......: 15 setembro 2020                                                              ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                                               ##
##  ANALISTA...: EMIR MANSUR SMAKA                                                             ##
##  SETOR......: COTIN/UGDT                                                                    ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                                        ##
#################################################################################################

df_bebidas_nfe <- fread("./BEBIDAS2018.csv")

df_bebidas_nfe <- df_bebidas_nfe%>%
  filter(PROD_CPROD_SEFAZ_AJUSTADO != 10030440006 & PROD_CPROD_SEFAZ_AJUSTADO != 10030440008)


#### CRIA COLUNAS AJUSTADAS
df_bebidas_nfe$QTE_TRIB_AJUSTADO <- as.character("")
df_bebidas_nfe$VOLUME_TRIB_AJUSTADO <- as.character("")
df_bebidas_nfe$VUNTRIB_AJUSTADO <- as.double(0.0000)
df_bebidas_nfe$FATOR_MULTIPLICADOR <- as.double(0.0)
df_bebidas_nfe$UNIDADE_SEFAZ <- as.character("UND")
df_bebidas_nfe$VOLUME_SEFAZ <- as.double(0)
df_bebidas_nfe$UN_MEDIDA_SEFAZ <- as.character("")

#### SEPARA VOLUMES 
### Volumes ML
id_ml <- grep("ml",df_bebidas_nfe$PROD_XPROD,ignore.case = T)

df_bebidas_ml <- df_bebidas_nfe[id_ml]
df_bebidas_ml$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_bebidas_ml$PROD_XPROD),"(\\s|x)?\\d{3,5}(\\s)?ml")

df_ml_sem_ajuste <- df_bebidas_ml%>%
  filter(is.na(VOLUME_TRIB_AJUSTADO))

df_bebidas_ml <- anti_join(df_bebidas_ml,df_ml_sem_ajuste, by = c("IDNFE","DET_NITEM"))
df_bebidas_sem_ajuste <- anti_join(df_bebidas_nfe,df_bebidas_ml, by = c("IDNFE","DET_NITEM"))

rm(df_ml_sem_ajuste,id_ml)
###############


### Volumes L
id_litro <- grep("(\\d{1,2}(\\.|\\,|\\s)?\\d*?(\\s)?(l|lt|lts))|(\\d(\\.)?\\d\\d\\d(\\s)?ml)",df_bebidas_sem_ajuste$PROD_XPROD,ignore.case = T)
df_bebidas_litro <- df_bebidas_sem_ajuste[id_litro]
df_bebidas_litro$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_bebidas_litro$PROD_XPROD),"(\\d{1,2}(\\.|\\,|\\s)?\\d*?(\\s)?(l|lt|lts))|(\\d(\\.)?\\d\\d\\d(\\s)?ml)")
df_bebidas_litro$UN_MEDIDA_SEFAZ <- "L"

df_litro_sem_ajuste <- df_bebidas_litro%>%
  filter(VOLUME_TRIB_AJUSTADO == "0  l" | is.na(VOLUME_TRIB_AJUSTADO))

df_bebidas_litro <- anti_join(df_bebidas_litro,df_litro_sem_ajuste, by = c("IDNFE","DET_NITEM"))
rm(id_litro,df_litro_sem_ajuste)
###############

df_bebidas_ajustas <- rbind(df_bebidas_litro,df_bebidas_ml)
df_bebidas_sem_ajuste  <- anti_join(df_bebidas_nfe,df_bebidas_ajustas, by = c("IDNFE","DET_NITEM"))

rm(df_bebidas_litro,df_bebidas_ml)
###############

###### AJUSTA VOLUME E UNIDADE QUE ESCAPARAM AOS PADROES ANTERIORES (ULTIMO AJUSTE)
p1 <- ("\\d{1,4}\\s?g")
p2 <- ("\\s?x\\s?\\d{3,4}")
p3 <- ("(\\d{3}m)|(1,5|1\\.5)") 
p4 <- ("500|510|300|250|505|750|330|350")

df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(df_bebidas_sem_ajuste$PROD_XPROD),p1)
df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO <- ifelse(is.na(df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO),
                                                     str_extract(tolower(df_bebidas_sem_ajuste$PROD_XPROD),p2),df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO)
df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO <- ifelse(is.na(df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO),
                                                     str_extract(tolower(df_bebidas_sem_ajuste$PROD_XPROD),p3),df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO)
df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO <- ifelse(is.na(df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO),
                                                     str_extract(tolower(df_bebidas_sem_ajuste$PROD_XPROD),p4),df_bebidas_sem_ajuste$VOLUME_TRIB_AJUSTADO)                                                     


df_semi_ajuste <- df_bebidas_sem_ajuste%>%
  filter(VOLUME_TRIB_AJUSTADO != is.na(VOLUME_TRIB_AJUSTADO))

df_bebidas_sem_ajuste <- anti_join(df_bebidas_sem_ajuste,df_semi_ajuste, by = c("IDNFE","DET_NITEM"))

i <- grep("g",df_semi_ajuste$VOLUME_TRIB_AJUSTADO,ignore.case = T)

df_semi_ajuste$VOLUME_SEFAZ <- as.double(str_extract(df_semi_ajuste$VOLUME_TRIB_AJUSTADO,"\\d{2,4}"))
df_semi_ajuste$VOLUME_SEFAZ <- ifelse(df_semi_ajuste$VOLUME_SEFAZ < 10, NA, df_semi_ajuste$VOLUME_SEFAZ)
df_semi_ajuste$VOLUME_SEFAZ <- ifelse(grepl("\\d{1,2}\\s?gfa",df_semi_ajuste$PROD_XPROD,ignore.case = T),NA,df_semi_ajuste$VOLUME_SEFAZ)
df_semi_ajuste$UN_MEDIDA_SEFAZ <- ifelse(df_semi_ajuste$UN_MEDIDA_SEFAZ != "G" & df_semi_ajuste$VOLUME_SEFAZ < 1000,"ML","L")
df_semi_ajuste$UN_MEDIDA_SEFAZ[i] <- "G"
rm(p1,p2,p3,p4,i)
########################################################################################

#### GERA TABELA FINAL COM AJUSTES
df_bebidas_ajustas <- rbind(df_bebidas_ajustas,df_semi_ajuste)
df_bebidas_ajustas <- rbind(df_bebidas_ajustas,df_bebidas_sem_ajuste)

tab_bebidas_ajustadas <- df_bebidas_ajustas %>%
  select(IDNFE,DET_NITEM,FATOR_MULTIPLICADOR,UNIDADE_SEFAZ,VOLUME_SEFAZ,UN_MEDIDA_SEFAZ)

rm(df_bebidas_ajustas,df_bebidas_sem_ajuste,df_semi_ajuste)













