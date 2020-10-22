###############################################################################
##  SCRIPT.....: AJUSTES PARA CERVEJAS                                       ##
##  DETALHE....: AJUSTA VOLUME EM VAORES E UNIDADE MEDIDA EM L e ML          ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################


######## SEPARAR CERVEJA (EXCLUI REGISTROS COM CPROD_CERVEJA_SEFAZ == -9999999)
df_cerveja_ajuste <- df_cerveja%>%
  filter(CPROD_CERVEJA_SEFAZ != -9999999)

df_nao_cerveja <- setdiff(df_cerveja,df_cerveja_ajuste)
##rm(df_cerveja)
########################################################

### SEPARA REGISTROS COM VLAORES INTEIROS E ML EM XPROD
id_ml <- grep("ml",df_cerveja$PROD_XPROD,ignore.case = T)
cerveja_ml <- df_cerveja[id_ml,]
df_cerveja <- setdiff(df_cerveja,cerveja_ml)

### AJUSTA VOLUME_SEFAZ COM ML E L CONFORME O CASO
## VALOR ML INTEIRO NO XPROD
cerveja_ml$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(cerveja_ml$PROD_XPROD),"(\\d{3}\\s+?ml|\\d{4}\\s+?ml|\\d{5}\\s+?ml|\\d\\.\\d{3}\\s+?ml)")
cerveja_ml$VOLUME_TRIB_AJUSTADO <- str_replace(cerveja_ml$VOLUME_TRIB_AJUSTADO,"\\.","")
cerveja_ml$VOLUME_SEFAZ <-as.double(str_extract(cerveja_ml$VOLUME_TRIB_AJUSTADO, "(\\d{4}|\\d{3})"))
cerveja_ml$VOLUME_SEFAZ <- ifelse(cerveja_ml$VOLUME_SEFAZ > 999, cerveja_ml$VOLUME_SEFAZ/1000,cerveja_ml$VOLUME_SEFAZ)
cerveja_ml$UN_MEDIDA_SEFAZ <- ifelse(cerveja_ml$VOLUME_SEFAZ > 99,"ML","L")
rm(id_ml)
########################################################

### SEPARA REGISTROS COM VALORES SIMILIARES A LITRO E ML EM XPROD
id_litro <- grep("\\d{1,3}\\s?l",df_cerveja$PROD_XPROD,ignore.case = T)
cerveja_litro <- df_cerveja[id_litro,]
df_cerveja <- setdiff(df_cerveja,cerveja_litro)

cerveja_litro$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(cerveja_litro$PROD_XPROD),"\\d\\,\\d{1,3}\\s?l|\\d{1,3}\\s?l")
cerveja_litro$VOLUME_TRIB_AJUSTADO <- str_extract(cerveja_litro$VOLUME_TRIB_AJUSTADO,"\\d\\,\\d{1,3}|\\d{1,3}")
cerveja_litro$VOLUME_TRIB_AJUSTADO <- str_replace(cerveja_litro$VOLUME_TRIB_AJUSTADO,",",".")
cerveja_litro$VOLUME_TRIB_AJUSTADO <- as.double(cerveja_litro$VOLUME_TRIB_AJUSTADO)
cerveja_litro$VOLUME_TRIB_AJUSTADO <- ifelse(cerveja_litro$VOLUME_TRIB_AJUSTADO < 1,cerveja_litro$VOLUME_TRIB_AJUSTADO*1000,cerveja_litro$VOLUME_TRIB_AJUSTADO)
cerveja_litro$VOLUME_SEFAZ <- cerveja_litro$VOLUME_TRIB_AJUSTADO
cerveja_litro$UN_MEDIDA_SEFAZ <- ifelse(cerveja_litro$VOLUME_SEFAZ > 99,"ML","L")
########################################################

### REGISTROS CLASSIFICADOS
df_cerveja_ajustada <- rbind(cerveja_litro,cerveja_ml)
rm(cerveja_litro,cerveja_ml,id_litro)
gc(reset = T)
########################################################


id <- grep("litro|litrao|litrão",df_cerveja$PROD_XPROD,ignore.case = T)
df_cerveja$VOLUME_SEFAZ[id] <- 1
df_cerveja$UN_MEDIDA_SEFAZ[id] <- "L"

df_cerveja <- rbind(df_cerveja,df_cerveja_ajustada)
rm(id,df_cerveja_ajustada)
gc(reset = T)

### EXCLUI COLUNAS DE APOIO ###
df_cerveja$QTE_TRIB_AJUSTADO <- NULL
df_cerveja$VOLUME_TRIB_AJUSTADO <- NULL
df_cerveja$VUNTRIB_AJUSTADO <- NULL
#######################################################################
##                          FINAL DOS AJUSTES                        ##
#######################################################################
