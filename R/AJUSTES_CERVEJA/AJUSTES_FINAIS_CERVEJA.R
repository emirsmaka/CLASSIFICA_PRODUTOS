######################################################################################

#### RENOMEAR COLUNAS PARA PADRAO SEFAZ
df_cerveja <- rename(df_cerveja,PROD_CPROD_SEFAZ_DETALHADO = 'CPROD_CERVEJA_SEFAZ')
df_cerveja <- rename(df_cerveja,PROD_XPROD_SEFAZ_DETALHADO = 'XPROD_CERVEJAS_SEFAZ')
df_cerveja <- rename(df_cerveja,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO = 'UN_MEDIDA_SEFAZ')
df_cerveja <- rename(df_cerveja,PROD_VOLUME_SEFAZ_AJUSTADO = 'VOLUME_SEFAZ')
df_cerveja <- rename(df_cerveja,PROD_FATOR_MULTIPLICADOR = 'FATOR_MULTIPLICADOR')
df_cerveja <- rename(df_cerveja,PROD_USEFAZ_AJUSTADO = 'UNIDADE_SEFAZ')
df_cerveja <- rename(df_cerveja,PROD_QSEFAZ_AJUSTADO = 'QTE_SEFAZ')
df_cerveja <- rename(df_cerveja,PROD_VUNSEFAZ_AJUSTADO = 'VLR_UNITARIO_SEFAZ')
df_cerveja[,"PROD_EMBALAGEM_AJUSTADO"] <- as.character("SEM AJUSTE")
######################################################################################

#### CONVERTER UNIDADE MEDIDA DE L PARA ML BEM COMO O VOLUME
df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO <-  ifelse(df_cerveja$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO == 'L'
                                                      ,df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO * 1000
                                                      ,df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO)

df_cerveja$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO <-  ifelse(df_cerveja$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO == 'L'
                                                              ,'ML',df_cerveja$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO)
######################################################################################

#### IDENTIFICA TIPO DE EMBALAGEM

## CERVEJA GARRAFA
id_garrafa <- grep("gfa|gf|garrafa",df_cerveja$PROD_XPROD,ignore.case = T)
df_garrafa <- df_cerveja[id_garrafa,]
df_cerveja <- anti_join(df_cerveja,df_garrafa,by=c("IDNFE","DET_NITEM"))

## EMBALAGEM = GARRAFA LONG NECK
id_ln <- grep("ln|long neck|longneck|long n|long",df_cerveja$PROD_XPROD,ignore.case = T)
df_longneck <- df_cerveja[id_ln,]
df_cerveja <- anti_join(df_cerveja,df_longneck,by=c("IDNFE","DET_NITEM"))

## CERVEJA ML
df_cerveja_ml <- df_cerveja%>%
filter(PROD_VOLUME_SEFAZ_AJUSTADO < 1000)
df_cerveja <- anti_join(df_cerveja,df_cerveja_ml,by=c("IDNFE","DET_NITEM"))

## EMBALAGEM = LATA
id_lata <- grep("\\<lt\\>|\\dlt\\s|\\slt\\d|\\<lata\\>|\\<lta\\>|ltao",df_cerveja_ml$PROD_XPROD,ignore.case = T)
df_lata <- df_cerveja_ml[id_lata,]
df_cerveja_ml <- anti_join(df_cerveja_ml,df_lata,by=c("IDNFE","DET_NITEM"))

## REMONTA TABELA DF_CERVEJA
df_cerveja <- rbind(df_cerveja,df_cerveja_ml)
rm(df_cerveja_ml)
gc(reset = T)

id_barril <- grep("\\<barril\\>|\\<keg\\>",df_cerveja$PROD_XPROD,ignore.case = T)
df_barril <- df_cerveja[id_barril,]
df_cerveja <- anti_join(df_cerveja,df_barril,by=c("IDNFE","DET_NITEM"))

## ATRIBUI DESCRICAO DE EMBALAGENS
df_lata$PROD_EMBALAGEM_AJUSTADO <- "LATA"
df_barril$PROD_EMBALAGEM_AJUSTADO <- "BARRIL"
df_garrafa$PROD_EMBALAGEM_AJUSTADO <- "GARRAFA"
df_longneck$PROD_EMBALAGEM_AJUSTADO <- "LONG NECK"

df_cerveja <- rbind(df_barril,df_cerveja,df_garrafa,df_lata,df_longneck)
rm(df_barril,df_garrafa,df_lata,df_longneck,id_barril,id_garrafa,id_lata,id_ln)
df_cerveja <- as.data.table(df_cerveja)
gc(reset = T)

#### RENOMEAR COLUNAS PARA PADRAO SEFAZ - DF_CHOPP
df_chopp <- rename(df_chopp,PROD_CPROD_SEFAZ_DETALHADO = 'CPROD_CERVEJA_SEFAZ')
df_chopp <- rename(df_chopp,PROD_XPROD_SEFAZ_DETALHADO = 'XPROD_CERVEJAS_SEFAZ')
df_chopp <- rename(df_chopp,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO = 'UN_MEDIDA_SEFAZ')
df_chopp <- rename(df_chopp,PROD_VOLUME_SEFAZ_AJUSTADO = 'VOLUME_SEFAZ')
df_chopp <- rename(df_chopp,PROD_FATOR_MULTIPLICADOR = 'FATOR_MULTIPLICADOR')
df_chopp <- rename(df_chopp,PROD_USEFAZ_AJUSTADO = 'UNIDADE_SEFAZ')
df_chopp <- rename(df_chopp,PROD_QSEFAZ_AJUSTADO = 'QTE_SEFAZ')
df_chopp <- rename(df_chopp,PROD_VUNSEFAZ_AJUSTADO = 'VLR_UNITARIO_SEFAZ')
df_chopp[,"PROD_EMBALAGEM_AJUSTADO"] <- as.character("SEM AJUSTE")
## EXCLUI COLUNAS EM DF_CHOPP NAO UTILIZADAS
df_chopp$QTE_TRIB_AJUSTADO <- NULL
df_chopp$VOLUME_TRIB_AJUSTADO <- NULL
df_chopp$VUNTRIB_AJUSTADO <- NULL
###

#### INSERE DF_CHOPP EM DF_CERVEJA
df_cerveja <- rbind(df_cerveja,df_chopp)

#### COLUNAS NAO AJUSTADAS
df_cerveja$PROD_FATOR_MULTIPLICADOR <- ifelse(df_cerveja$PROD_FATOR_MULTIPLICADOR == 0|is.na(df_cerveja$PROD_FATOR_MULTIPLICADOR),
                                              -1.11,df_cerveja$PROD_FATOR_MULTIPLICADOR)
df_cerveja$PROD_QSEFAZ_AJUSTADO <- ifelse(df_cerveja$PROD_QSEFAZ_AJUSTADO == 0|is.na(df_cerveja$PROD_QSEFAZ_AJUSTADO),
                                          -1.11,df_cerveja$PROD_QSEFAZ_AJUSTADO)
df_cerveja$PROD_VUNSEFAZ_AJUSTADO <- ifelse(df_cerveja$PROD_VUNSEFAZ_AJUSTADO == 0|is.na(df_cerveja$PROD_VUNSEFAZ_AJUSTADO),
                                            -1.11,df_cerveja$PROD_VUNSEFAZ_AJUSTADO)
df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO <- ifelse(df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO == 0|is.na(df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO),
                                                -1.11,df_cerveja$PROD_VOLUME_SEFAZ_AJUSTADO)

tb_cerveja <- df_cerveja%>%
  select(IDNFE,DET_NITEM,PROD_CPROD_SEFAZ_DETALHADO,PROD_XPROD_SEFAZ_DETALHADO,PROD_QSEFAZ_AJUSTADO
         ,PROD_USEFAZ_AJUSTADO,PROD_VUNSEFAZ_AJUSTADO,PROD_FATOR_MULTIPLICADOR,PROD_VOLUME_SEFAZ_AJUSTADO
         ,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO,PROD_EMBALAGEM_AJUSTADO)

#### AJUSTA CODIGO SEFAZ DO PRODUTO DETALHADO
tb_999 <- tb_cerveja%>%
  filter(PROD_CPROD_SEFAZ_DETALHADO == -9999999)

tb_cerveja <- anti_join(tb_cerveja,tb_999,by=c("IDNFE","DET_NITEM"))

tb_cerveja$PROD_CPROD_VAR <- as.character(paste("10030440008","000",
                                                     substring(tb_cerveja$PROD_CPROD_SEFAZ_DETALHADO,6,7),sep = ""))

tb_cerveja$PROD_CPROD_SEFAZ_DETALHADO <- tb_cerveja$PROD_CPROD_VAR
tb_cerveja$PROD_CPROD_VAR <- NULL
tb_999$PROD_CPROD_VAR <- "-9999999999999999"
tb_999$PROD_CPROD_SEFAZ_DETALHADO <- tb_999$PROD_CPROD_VAR
tb_999$PROD_CPROD_VAR <- NULL
tb_cerveja <- rbind(tb_cerveja,tb_999)
rm(tb_999)
##################################################3

fwrite(tb_cerveja,"./DADOS/TB_CERVEJA.csv",sep = ";")

rm(list = ls())
gc(reset = T)
######################## FIM AJUSTES FINAIS REFRIGERANTES ######################## 

