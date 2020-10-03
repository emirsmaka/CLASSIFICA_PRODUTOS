###############################################################################
##  SCRIPT.....: AJUSTES PARA cervejaS                                  ##
##  DETALHE....: AJUSTA VOLUME EM VAORES E UNIDADE MEDIDA EM L e ML          ##
##  DATA.......: 15 setembro 2020                                            ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

#### CARREGA BIBLIOTECAS
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)

### CARREGA DADOS

df_cerveja <- fread("./CERVEJA2020_CLASS.csv",quote = "",sep = ";")

#### CRIA COLUNAS AJUSTADAS
df_cerveja[,"QTE_TRIB_AJUSTADO"] <- as.character()
df_cerveja[,"VOLUME_TRIB_AJUSTADO"] <- as.character()
df_cerveja[,"VUNTRIB_AJUSTADO"] <- as.double()
df_cerveja[,"FATOR_MULTIPLICADOR"] <- as.double()
df_cerveja[,"UNIDADE_SEFAZ"] <- "UN"
df_cerveja[,"VOLUME_SEFAZ"] <- as.double()
df_cerveja[,"UN_MEDIDA_SEFAZ"] <- as.character()
########################################################

  ######## SEPARAR CERVEJA (EXCLUI REGISTROS COM CPROD_CERVEJA_SEFAZ == -9999999)
  df_cerveja_ajuste <- df_cerveja%>%
    filter(CPROD_CERVEJA_SEFAZ != -9999999)

  df_nao_cerveja <- setdiff(df_cerveja,df_cerveja_ajuste)
  ##rm(df_cerveja)
  ########################################################

  ### SEPARA REGISTROS COM VLAORES INTEIROS E ML EM XPROD
  id_ml <- grep("ml",df_cerveja_ajuste$PROD_XPROD,ignore.case = T)
  cerveja_ml <- df_cerveja_ajuste[id_ml]

  df_cerveja_ajuste <- setdiff(df_cerveja_ajuste,cerveja_ml)
  ### AJUSTA VOLUME_SEFAZ COM ML E L CONFORME O CASO
  ## VALOR ML INTEIRO NO XPROD
  cerveja_ml$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(cerveja_ml$PROD_XPROD),"(\\d{3}\\s?ml|\\d{4}\\s?ml|\\d{5}\\s?ml|\\d\\.\\d{3}\\s?ml)")
  cerveja_ml$VOLUME_TRIB_AJUSTADO <- str_replace(cerveja_ml$VOLUME_TRIB_AJUSTADO,"\\.","")
  cerveja_ml$VOLUME_SEFAZ <-as.double(str_extract(cerveja_ml$VOLUME_TRIB_AJUSTADO, "(\\d{4}|\\d{3})"))
  cerveja_ml$VOLUME_SEFAZ <- ifelse(cerveja_ml$VOLUME_SEFAZ > 999, cerveja_ml$VOLUME_SEFAZ/1000,cerveja_ml$VOLUME_SEFAZ)
  cerveja_ml$UN_MEDIDA_SEFAZ <- ifelse(cerveja_ml$VOLUME_SEFAZ > 99,"ML","L")
  rm(id_ml)
  ########################################################

  ### SEPARA REGISTROS COM VALORES SIMILIARES A LITRO E ML EM XPROD
  id_litro <- grep("\\d{1,3}\\s?l",df_cerveja_ajuste$PROD_XPROD,ignore.case = T)
  cerveja_litro <- df_cerveja_ajuste[id_litro]
  df_cerveja_ajuste <- setdiff(df_cerveja_ajuste,cerveja_litro)

  cerveja_litro$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(cerveja_litro$PROD_XPROD),"\\d\\,\\d{1,3}\\s?l|\\d{1,3}\\s?l")
  cerveja_litro$VOLUME_TRIB_AJUSTADO <- str_extract(cerveja_litro$VOLUME_TRIB_AJUSTADO,"\\d\\,\\d{1,3}|\\d{1,3}")
  cerveja_litro$VOLUME_TRIB_AJUSTADO <- str_replace(cerveja_litro$VOLUME_TRIB_AJUSTADO,",",".")
  cerveja_litro$VOLUME_TRIB_AJUSTADO <- as.double(cerveja_litro$VOLUME_TRIB_AJUSTADO)
  cerveja_litro$VOLUME_TRIB_AJUSTADO <- ifelse(cerveja_litro$VOLUME_TRIB_AJUSTADO < 1,cerveja_litro$VOLUME_TRIB_AJUSTADO*1000,cerveja_litro$VOLUME_TRIB_AJUSTADO)
  cerveja_litro$VOLUME_SEFAZ <- cerveja_litro$VOLUME_TRIB_AJUSTADO
  cerveja_litro$UN_MEDIDA_SEFAZ <- ifelse(cerveja_litro$VOLUME_SEFAZ > 99,"ML","L")
  ########################################################

  ### SEPARA REGISTROS NAO AJUSTADOS
  cerveja_sem_ajuste <- cerveja_litro%>%
    filter(VOLUME_SEFAZ == 0)
  cerveja_litro <- setdiff(cerveja_litro,cerveja_sem_ajuste)
  ########################################################

  ### REGISTROS CLASSIFICADOS
  df_cerveja <- rbind(cerveja_litro,cerveja_ml)
  rm(cerveja_litro,cerveja_ml,id_litro)
  ########################################################

  ### AJUSTA REGISTROS QUE ESCAPARAM AOS PADROES ANTERIORES
  df_cerveja_sem_ajuste <- rbind(cerveja_sem_ajuste,df_cerveja_ajuste)
  rm(cerveja_sem_ajuste,df_cerveja_ajuste)
  ########################################################
  gc(reset = T)

  ### AJUSTA REGISTROS QUE ESCAPARAM AOS PADROES ANTERIORES
  id <- grep("(x|\\s)\\d{3,4}\\s",df_cerveja_sem_ajuste$PROD_XPROD,ignore.case = T)
  df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id] <- str_extract(tolower(df_cerveja_sem_ajuste$PROD_XPROD[id]),"\\d{3,4}")
  df_cerveja_sem_ajuste$VOLUME_SEFAZ <- as.double(df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO)
  df_cerveja_sem_ajuste$VOLUME_SEFAZ <- ifelse(df_cerveja_sem_ajuste$VOLUME_SEFAZ > 1000 | df_cerveja_sem_ajuste$VOLUME_SEFAZ < 200,NA,df_cerveja_sem_ajuste$VOLUME_SEFAZ)
  df_cerveja <- rbind(df_cerveja,df_cerveja_sem_ajuste)
  ##
  df_cerveja_sem_ajuste <- df_cerveja%>%
    filter(is.na(VOLUME_SEFAZ))
  df_cerveja <- setdiff(df_cerveja,df_cerveja_sem_ajuste,id)
  ##
  df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO <- NA
  df_cerveja_sem_ajuste$VOLUME_SEFAZ <- NA
  id <- grep("\\d\\,\\dg|(\\s|x)\\d{3}$|(\\s|x)\\d{3}m",df_cerveja_sem_ajuste$PROD_XPROD,ignore.case = T)
  df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id] <- str_extract(tolower(df_cerveja_sem_ajuste$PROD_XPROD[id]),"\\d\\,\\dg|\\d{3}$|\\d{3}m")
  df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id] <- gsub(",",".",df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id])
  df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id] <- str_extract(df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id],"\\d{3}|\\d.\\d")
  df_cerveja_sem_ajuste$VOLUME_SEFAZ[id] <- as.double(df_cerveja_sem_ajuste$VOLUME_TRIB_AJUSTADO[id])

  ##
  df_cerveja <- rbind(df_cerveja,df_cerveja_sem_ajuste)
  df_cerveja$VOLUME_SEFAZ <- ifelse(df_cerveja$VOLUME_SEFAZ < 1,df_cerveja$VOLUME_SEFAZ*1000,df_cerveja$VOLUME_SEFAZ)
  df_cerveja$VOLUME_SEFAZ <- ifelse(df_cerveja$VOLUME_SEFAZ == 0,NA,df_cerveja$VOLUME_SEFAZ)
  df_cerveja$UN_MEDIDA_SEFAZ <- ifelse(is.na(df_cerveja$UN_MEDIDA_SEFAZ) & df_cerveja$VOLUME_SEFAZ < 1000,"ML",df_cerveja$UN_MEDIDA_SEFAZ)
  df_cerveja$UN_MEDIDA_SEFAZ <- ifelse(is.na(df_cerveja$VOLUME_SEFAZ),NA,df_cerveja$UN_MEDIDA_SEFAZ)
  rm(df_cerveja_sem_ajuste,id)
  ##

  df_cerveja_sem_ajuste <- df_cerveja%>%
    filter(is.na(VOLUME_SEFAZ))
  df_cerveja <- setdiff(df_cerveja,df_cerveja_sem_ajuste)

  id <- grep("litro|litrao|litrão",df_cerveja_sem_ajuste$PROD_XPROD,ignore.case = T)
  df_cerveja_sem_ajuste$VOLUME_SEFAZ[id] <- as.double(1)
  df_cerveja_sem_ajuste$UN_MEDIDA_SEFAZ[id] <- "L"
  df_cerveja <- rbind(df_cerveja,df_cerveja_sem_ajuste)
  rm(df_cerveja_sem_ajuste,id)
  gc(reset = T)



  ####### CHAMADA PARA SCRIPT AJUSTES VALOR,QTE E FATOR MULTIPLICADOR
  source("./AJUSTA_QTE_VLR_FATOR_CERVEJA.R")























