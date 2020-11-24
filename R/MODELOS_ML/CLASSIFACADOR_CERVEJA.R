#setwd("C:/DADOS_R/Scripts")
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

##
tab_cerveja <- read.xlsx("./DADOS/CERVEJAS_ROTULADAS.xlsx",sheet = "TAB_CERVEJAS")
tab_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(tab_cerveja$CPROD_CERVEJA_SEFAZ)
##
### ESTABELECE CONEXAO COM NETEZZA-DB
source("./SCRIPTS/CONEXAO_NETEZZA.R")
con_Netezza <- conect_NZ(con_Netezza)

## CARREGA DADOS PARA CLASSIFICAR
df_cerveja <- dbGetQuery(con_Netezza,"SELECT T0.IDNFE, T0.DET_NITEM,
T1.IDE_DHEMI_PERIODO,T1.PROD_CEANTRIB, T1.prod_cean, T0.IDE_SUBGRUPO, T0.PROD_CPROD_SEFAZ_AJUSTADO, T0.prod_xprod_sefaz_ajustado, T1.prod_cfop,
SQLKIT..regexp_replace(T0.PROD_XPROD, ';','-') AS PROD_XPROD, SQLKIT..regexp_replace(T0.PROD_UCOM, ';','-') AS PROD_UCOM,
T0.PROD_QCOM, T1.PROD_VPROD, SQLKIT..regexp_replace(T1.PROD_UTRIB, ';','-') AS PROD_UTRIB,T1.PROD_QTRIB,T1.PROD_VUNTRIB,T1.PROD_VUNCOM,
T1.EMIT_CON_TIPO, T1.remetente_cnpj_cpf, T1.dest_cnpj_cpf_idestrangeiro, T1.recebedor_cnpj_cpf
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE T0
LEFT JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM
WHERE T0.PROD_CPROD_SEFAZ_AJUSTADO = 10030440008
      AND T1.IDE_DHEMI_PERIODO BETWEEN 201901 AND 202010")

source("./SCRIPTS/LIMPA_XPROD_V3.R")

df_cerveja[,"CPROD_CERVEJA_SEFAZ"] <- as.integer()
df_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(df_cerveja$CPROD_CERVEJA_SEFAZ)

cerveja_class <- limpa_xprod(df_cerveja)

cerveja.hex <- as.h2o(cerveja_class)
gbm_cerv <- h2o.upload_model("./MODELOS/GBM_CERVEJA_NFE")

cerv_class <- h2o.predict(gbm_cerv,cerveja.hex)


df_cerveja_class <- as.data.frame(cerv_class$predict)

df_cerveja <- cbind(df_cerveja,df_cerveja_class)
df_cerveja$CPROD_CERVEJA_SEFAZ <- df_cerveja$predict
df_cerveja$predict <- NULL

## INSERE DESCRICAO PADRONIZADA DO PRODUTO CERVEJA
df_cerveja <- left_join(df_cerveja,tab_cerveja, by = "CPROD_CERVEJA_SEFAZ")

rm(cerv_class,cerveja.hex,df_cerveja_class,gbm_cerv,cerveja_class)
gc(reset = TRUE)

source("./SCRIPTS/AJUSTA_FATOR_QTE_CERVEJA_V2.R")

fwrite(df_cerveja,"./DADOS/nfe_cerveja.csv",sep = ";")


h2o.shutdown(prompt = FALSE)























