setwd("C:/DADOS_R/Scripts")
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

con_Netezza <- odbcConnect(dsn = "nz1436", uid = "esmaka", pwd = "@q158850i")

## CARREGA DADOS PARA CLASSIFICAR
df_cerveja <- sqlQuery(con_Netezza,"SELECT T0.IDNFE, T0.DET_NITEM,
T1.IDE_DHEMI_PERIODO,T1.PROD_CEANTRIB, T1.prod_cean, T0.IDE_SUBGRUPO, T0.PROD_CPROD_SEFAZ_AJUSTADO, T0.prod_xprod_sefaz_ajustado, T1.prod_cfop,
SQLKIT..regexp_replace(T0.PROD_XPROD, ';','-') AS PROD_XPROD, SQLKIT..regexp_replace(T0.PROD_UCOM, ';','-') AS PROD_UCOM,
T0.PROD_QCOM, T1.PROD_VPROD, SQLKIT..regexp_replace(T1.PROD_UTRIB, ';','-') AS PROD_UTRIB,T1.PROD_QTRIB,T1.PROD_VUNTRIB,T1.PROD_VUNCOM,
T1.EMIT_CON_TIPO, T1.remetente_cnpj_cpf, T1.dest_cnpj_cpf_idestrangeiro, T1.recebedor_cnpj_cpf
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE T0
LEFT JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM
WHERE T0.PROD_CPROD_SEFAZ_AJUSTADO = 10030440008
      AND T1.IDE_DHEMI_PERIODO BETWEEN 202001 AND 202008",believeNRows=FALSE)
##


cerveja_class <- df_cerveja%>%
  select(PROD_XPROD)

cerveja_class[,"CPROD_CERVEJA_SEFAZ"] <- as.integer()
cerveja_class$CPROD_CERVEJA_SEFAZ <- as.factor(cerveja_class$CPROD_CERVEJA_SEFAZ)

cerveja_class$PROD_XPROD_LIMPO <- limpa_xprod_2(cerveja_class$PROD_XPROD)
cerveja_class$PROD_XPROD_LIMPO <- as.factor(cerveja_class$PROD_XPROD_LIMPO)



cerveja.hex <- as.h2o(cerveja_class)

gbm_cerv <- h2o.upload_model("./GBM_CERVEJA_NFE")

tb_cerv_class <- h2o.predict(gbm_cerv,cerveja.hex)



df_cerveja_class <- as.data.frame(tb_cerv_class$predict)

df_cerveja_class <- cbind(df_cerveja_class$predict,df_cerveja)


df_cerveja_class <- rename(df_cerveja_class, CPROD_CERVEJA_SEFAZ = "df_cerveja_class$predict")

h2o.shutdown(prompt = FALSE)
df_cerveja <- cbind(df_cerveja, df_cerveja_class$predict)
rm(cerv_class,cerveja.hex,df_cerveja,gbm_cerv)
gc(reset = TRUE)
























