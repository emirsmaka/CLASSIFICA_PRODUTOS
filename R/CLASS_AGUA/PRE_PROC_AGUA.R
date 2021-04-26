library(stringi)
library(stringr)
library(tidytext)
library(dplyr)
library(tidyr)
library(openxlsx)

### ESTABELECE CONEXAO COM NETEZZA-DB
source("./SCRIPTS/FUNCOES/FN_CONEXAO_NETEZZA.R")
con_Netezza <- conect_NZ(con_Netezza)

## CARREGA DADOS PARA CLASSIFICAR
df_agua <- dbGetQuery(con_Netezza,"SELECT T0.IDNFE, T0.DET_NITEM,T1.PROD_NCM,T1.PROD_NCM_POSICAO,
T1.IDE_DHEMI_PERIODO,T1.PROD_CEANTRIB, T1.prod_cean, T0.IDE_SUBGRUPO, T0.PROD_CPROD_SEFAZ_AJUSTADO, T0.prod_xprod_sefaz_ajustado, 
SQLKIT..regexp_replace(T0.PROD_XPROD, ';','-') AS PROD_XPROD,T1.remetente_cnpj_cpf, T1.recebedor_cnpj_cpf           
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE T0 
LEFT JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM 
WHERE T1.PROD_NCM_POSICAO = '2201'
      AND T1.IDE_DHEMI_PERIODO = 201910")

## LIMPA XPROD ##
source("./SCRIPTS/FUNCOES/FN_LIMPA_XPROD.R")
df_agua <- fn_limpa_xprod(df_agua)
stp_word <- c("\\<[a-z]{1}\\>|\\<tb\\>|\\<kg\\>|\\<lt\\>|\\<cp\\>|\\<do\\>|\\<pet\\>|\\<grf\\>|\\<lc\\>|\\<sc\\>|\\<cf\\>|\\<vcp\\>")
df_agua$PROD_XPROD_LIMPO <- gsub(stp_word,"",df_agua$PROD_XPROD_LIMPO,ignore.case = T)
df_agua$PROD_XPROD_LIMPO <- str_squish(df_agua$PROD_XPROD_LIMPO)

## RETIRA TOKENS INDESEJAVEIS
source("./SCRIPTS/FUNCOES/FN_TOKENS_INDESEJAVEIS.R")
tk_indesejaveis <- read.xlsx("./DADOS/TK_INDESEJAVEIS_AGUA.xlsx",sheet = "TOKENS_INDESEJ")
num <- length(tk_indesejaveis$TERMOS.IDESEJAVEIS)
tokens <- c(tk_indesejaveis$TERMOS.IDESEJAVEIS[1:num])
tokens <- fn_token_indesejado(tokens)
tokens <- paste(tokens,collapse = "|")
tokens <- paste("(",tokens,")",sep = "")
id_indesej <- grep(tokens,df_agua$PROD_XPROD_LIMPO,ignore.case = T)
df_nao_agua <- df_agua[id_indesej,]
df_agua <- setdiff(df_agua,df_nao_agua)

#### VERIFICA TERMOS MAIS COMUNS ATRAVES DE BI-GRAM ####
xprod_agua <- df_agua%>%
  select(PROD_XPROD_LIMPO)

df_bigram <- xprod_agua %>%
  unnest_tokens(XPROD_BIGRAM, PROD_XPROD_LIMPO, token = "ngrams", n = 2) %>%
  separate(XPROD_BIGRAM,c("TERMO_1","TERMO_2"), sep = " ")


df_bigram_count <- df_bigram %>%
  count(TERMO_1,TERMO_2, sort = TRUE)





