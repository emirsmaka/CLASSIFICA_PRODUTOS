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
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)

### CARREGA DADOS

df_bebidas_nfe <- fread("./TB_REFRI_2017_CLASS.csv",quote = "",sep = ";")

#### CRIA COLUNAS AJUSTADAS
df_bebidas_nfe$QTE_TRIB_AJUSTADO <- as.character("")
df_bebidas_nfe$VOLUME_TRIB_AJUSTADO <- as.character("")
df_bebidas_nfe$VUNTRIB_AJUSTADO <- as.double(0.0000)
df_bebidas_nfe$FATOR_MULTIPLICADOR <- as.double(0.0)
df_bebidas_nfe$UNIDADE_SEFAZ <- as.character("UND")
df_bebidas_nfe$VOLUME_SEFAZ <- as.double(0.0)
df_bebidas_nfe$UN_MEDIDA_SEFAZ <- as.character("")

########################################################



######## SEPARAR REFRIGERANTES (EXCLUI REGISTROS COM CPROD_REFRIGERANTE_SEFAZ == -9999999)
df_refri <- df_bebidas_nfe%>%
  filter(CPROD_REFRIGERANTE_SEFAZ != -9999999)

df_bebidas_nfe <- setdiff(df_bebidas_nfe,df_refri)
########################################################

### SEPARA REGISTROS COM VLAORES INTEIROS E ML EM XPROD
id_ml <- grep("ml",df_refri$PROD_XPROD,ignore.case = T)
refri_ml <- df_refri[id_ml]

df_refri <- setdiff(df_refri,refri_ml)
### AJUSTA VOLUME_SEFAZ COM ML E L CONFORME O CASO
## VALOR ML INTEIRO NO XPROD
refri_ml$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(refri_ml$PROD_XPROD),"(\\d{3}\\s?ml|\\d{4}\\s?ml|\\d\\.\\d{3}\\s?ml)")
refri_ml$VOLUME_TRIB_AJUSTADO <- str_replace(refri_ml$VOLUME_TRIB_AJUSTADO,"\\.","")
refri_ml$VOLUME_SEFAZ <-as.double(str_extract(refri_ml$VOLUME_TRIB_AJUSTADO, "(\\d{4}|\\d{3})"))
refri_ml$VOLUME_SEFAZ <- ifelse(refri_ml$VOLUME_SEFAZ > 999, refri_ml$VOLUME_SEFAZ/1000,refri_ml$VOLUME_SEFAZ)
refri_ml$UN_MEDIDA_SEFAZ <- ifelse(refri_ml$VOLUME_SEFAZ > 99,"ML","L")
rm(id_ml)
########################################################

### SEPARA REGISTROS COM VALORES SIMILIARES A LITRO E ML EM XPROD
id_litro <- grep("\\d\\s?l",df_refri$PROD_XPROD,ignore.case = T)
refri_litro <- df_refri[id_litro]
df_refri <- setdiff(df_refri,refri_litro)

refri_litro$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(refri_litro$PROD_XPROD),"(\\d(\\,|\\.)\\d{1,3}\\s?l|\\d\\s?l)")
refri_litro$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(refri_litro$VOLUME_TRIB_AJUSTADO),"(\\d(\\,|\\.)\\d{1,3}|\\d)")
refri_litro$VOLUME_TRIB_AJUSTADO <- str_replace(refri_litro$VOLUME_TRIB_AJUSTADO,",",".")
refri_litro$VOLUME_TRIB_AJUSTADO <- as.double(refri_litro$VOLUME_TRIB_AJUSTADO)
refri_litro$VOLUME_TRIB_AJUSTADO <- ifelse(refri_litro$VOLUME_TRIB_AJUSTADO < 1, refri_litro$VOLUME_TRIB_AJUSTADO*1000,refri_litro$VOLUME_TRIB_AJUSTADO)
refri_litro$VOLUME_SEFAZ <- refri_litro$VOLUME_TRIB_AJUSTADO
refri_litro$UN_MEDIDA_SEFAZ <- ifelse(refri_litro$VOLUME_SEFAZ > 99,"ML","L")
########################################################

### SEPARA REGISTROS NAO AJUSTADOS
refri_sem_ajuste <- refri_litro%>%
  filter(VOLUME_SEFAZ == 0)
refri_litro <- setdiff(refri_litro,refri_sem_ajuste)
########################################################

### REGISTROS CLASSIFICADOS
df_refrigerante <- rbind(refri_litro,refri_ml)
rm(refri_litro,refri_ml,id_litro)
########################################################

### AJUSTA REGISTROS QUE ESCAPARAM AOS PADROES ANTERIORES

ind <- grep("\\d{4}\\s?l|\\d{3}\\s?l",refri_sem_ajuste$PROD_XPROD,ignore.case = T)
refri_sem_ajuste$VOLUME_TRIB_AJUSTADO <- str_extract(tolower(refri_sem_ajuste$PROD_XPROD),"\\d{4}\\s?l|\\d{3}\\s?l")
refri_sem_ajuste$VOLUME_SEFAZ <- as.double(str_extract(refri_sem_ajuste$VOLUME_TRIB_AJUSTADO,"\\d{1,4}"))
refri_sem_ajuste$UN_MEDIDA_SEFAZ <- ifelse(refri_sem_ajuste$VOLUME_SEFAZ > 999,"L","ML")
refri_sem_ajuste$VOLUME_SEFAZ <- ifelse(refri_sem_ajuste$VOLUME_SEFAZ > 999, refri_sem_ajuste$VOLUME_SEFAZ/1000,refri_sem_ajuste$VOLUME_SEFAZ)

### ANEXA REGISTROS AJUSTADOS A TABELA GERAL
df_refrigerante <- rbind(df_refrigerante,refri_sem_ajuste)
rm(refri_sem_ajuste,ind)

########################################################

### AJUSTA REGISTROS QUE ESCAPARAM AOS PADROES ANTERIORES
df_refri$PROD_XPROD <- str_replace_all(tolower(df_refri$PROD_XPROD),"\\(|\\)"," ")
i <- grep("\\d{3}$",df_refri$PROD_XPROD,ignore.case = T)
i2 <- grep("\\d{3}(x|\\s)\\d{2}",df_refri$PROD_XPROD,ignore.case = T)
i3 <- grep("\\d{3}",df_refri$PROD_XPROD,ignore.case = T)
i4 <- grep("\\d\\,\\d",df_refri$PROD_XPROD,ignore.case = T)

volume_ml <- c(250,290,269,510,500,600,350,355,310,237,300,660)
volume_litro <- c(1.5,2.5)
df_refri$VOLUME_TRIB_AJUSTADO[i] <- str_extract(tolower(df_refri$PROD_XPROD[i]),"\\d{3}$")
df_refri$VOLUME_TRIB_AJUSTADO[i2] <- str_extract(tolower(df_refri$PROD_XPROD[i2]),"\\d{3}(x|\\s)\\d{2}")
df_refri$VOLUME_TRIB_AJUSTADO <- str_extract(df_refri$VOLUME_TRIB_AJUSTADO,"\\d{3}")


df_refri$VOLUME_TRIB_AJUSTADO[i3] <- str_extract(tolower(df_refri$PROD_XPROD[i3]),"\\d{3}")
df_refri$VOLUME_TRIB_AJUSTADO[i4] <- str_extract(tolower(df_refri$PROD_XPROD[i4]),"\\d\\,\\d")

df_refri$VOLUME_TRIB_AJUSTADO <- str_replace(df_refri$VOLUME_TRIB_AJUSTADO,",",".")
df_refri$VOLUME_TRIB_AJUSTADO <- as.double(df_refri$VOLUME_TRIB_AJUSTADO)

df_refri$VOLUME_SEFAZ <- ifelse(df_refri$VOLUME_TRIB_AJUSTADO %in% volume_ml | df_refri$VOLUME_TRIB_AJUSTADO %in% volume_litro , df_refri$VOLUME_TRIB_AJUSTADO,NA)
df_refri$UN_MEDIDA_SEFAZ <- ifelse(df_refri$VOLUME_SEFAZ > 99,"ML","L")
########################################################

df_refrigerante <- rbind(df_refrigerante,df_refri)
df_refrigerante <- rbind(df_refrigerante,df_bebidas_nfe)
df_refrigerante$VOLUME_TRIB_AJUSTADO <- NULL

######## FIM AJUSTES VOLUME E UNIDADE DE MEDIDA ########
rm(i,i2,i3,i4,volume_litro,volume_ml,df_refri,df_bebidas_nfe)
gc(reset = T)

####### CHAMADA PARA SCRIPT AJUSTES VALOR,QTE E FATOR MULTIPLICADOR
source("./AJUSTA_QTE_VLR_FATOR_REFRIGERANTE.R")





















