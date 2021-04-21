library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(stringi)
library(data.table)
library(RODBC)
library(DBI)
library(openxlsx)

df_1902 <- dbGetQuery(con_s1670_SQL,"SELECT * FROM TB_TREINAMENTO_CAPS_11_17_19
 WHERE PROD_NCM_POSICAO = '1902'")


df_alimentos$PROD_XPROD <- gsub(";","-",df_alimentos$PROD_XPROD)
fwrite(df_alimentos,"C:/DADOS_R/Scripts/ALIMENTOS/ALIMENTOS_CAP_10_11_19.csv",sep = ";")

df_alimentos <- dbGetQuery(con,"SELECT * FROM TB_ROTULO_ALIMENTOS")

df_alimentos_grp <- df_alimentos%>%
  group_by(PROD_NCM_POSICAO)

df_alimentos_grp <- fn_limpa_xprod_alim(df_alimentos_grp)


df_bigram <- df1902  %>%
  unnest_tokens(XPROD_BIGRAM, PROD_XPROD, token = "ngrams", n = 2) %>%
  separate(XPROD_BIGRAM,c("TERMO_1","TERMO_2"), sep = " ")

df_bigram_count <- df_bigram %>%
  dplyr::count(TERMO_1,TERMO_2, sort = TRUE)
