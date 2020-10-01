### CARREGA BIBLIOTECAS
library(dplyr)
library(openxlsx)
library(data.table)
library(h2o)
library(bit64)
library(RODBC)

### CRIA CONEXAO COM SQL SERVER
con_Netezza <- odbcConnect(dsn = "nz1436", uid = "esmaka", pwd = "@q158850i")

## CARREGA DADOS PARA CLASSIFICAR
df_refri <- sqlQuery(con_Netezza,"SELECT T0.IDNFE, T0.DET_NITEM,
T1.IDE_DHEMI_PERIODO,T1.PROD_CEANTRIB, T1.prod_cean, T0.IDE_SUBGRUPO, T0.PROD_CPROD_SEFAZ_AJUSTADO, T0.prod_xprod_sefaz_ajustado, T1.prod_cfop,
SQLKIT..regexp_replace(T0.PROD_XPROD, ';','-') AS PROD_XPROD, SQLKIT..regexp_replace(T0.PROD_UCOM, ';','-') AS PROD_UCOM,
T0.PROD_QCOM, T1.PROD_VPROD, SQLKIT..regexp_replace(T1.PROD_UTRIB, ';','-') AS PROD_UTRIB,T1.PROD_QTRIB,T1.PROD_VUNTRIB,T1.PROD_VUNCOM,
T1.EMIT_CON_TIPO, T1.remetente_cnpj_cpf, T1.dest_cnpj_cpf_idestrangeiro, T1.recebedor_cnpj_cpf
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE T0
LEFT JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM
WHERE T0.PROD_CPROD_SEFAZ_AJUSTADO = 10030440006
      AND T1.IDE_DHEMI_PERIODO BETWEEN 202001 AND 202008",believeNRows=FALSE)
##

#### AJUSTA DADOS PARA CLASSIFICAR
refri_class <- df_refri%>%
  select(PROD_XPROD)

refri_class[,"CPROD_CERVEJA_SEFAZ"] <- as.integer()
refri_class$CPROD_CERVEJA_SEFAZ <- as.factor(refri_class$CPROD_CERVEJA_SEFAZ)

refri_class$PROD_XPROD_LIMPO <- limpa_xprod_2(refri_class$PROD_XPROD)
refri_class$PROD_XPROD_LIMPO <- as.factor(refri_class$PROD_XPROD_LIMPO)




### CLASSIFICA DADOS
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

refri_dia.hex <- as.h2o(refri_class)
gbm_nfe <- h2o.upload_mojo("./GBM_REFRI_NFE.zip")
resultado <- h2o.predict(gbm_nfe,refri_dia.hex)


tb_resultado <- as.data.frame(resultado$predict)
df_refri_class <- cbind(df_refri,tb_resultado$predict)
df_refri_class$CPROD_REFRIGERANTE_SEFAZ <- df_refri_class$V2
df_refri_class$V2 <- NULL



fwrite(df_refri_class, "./REFRIGERANTE2020_CLASS.csv", sep = ";")

h2o.shutdown(prompt = FALSE)
rm(df_refri,tb_resultado,refri_dia.hex,resultado,gbm_nfe,df_refri_class,refri_class)
gc(reset = TRUE)

