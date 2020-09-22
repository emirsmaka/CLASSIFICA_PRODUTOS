### CARREGA BIBLIOTECAS
library(dplyr)
library(openxlsx)
library(data.table)
library(h2o)
library(bit64)
library(RODBC)

### CRIA CONEXAO COM SQL SERVER
dbconect1 <- odbcDriverConnect('driver={SQL Server};server=localhost;database=MODELOS_ML;trusted_connection=true')


### SELECIONA DADOS PARA CLASSIFICACAO
df_refri_dia <- fread("./REFRIGERANTE.csv",dec = ",",stringsAsFactors = T,integer64 = "numeric")
df_refri_dia <- df_refri_dia %>%
  filter(IDE_DHEMI_PERIODO >= 201701 & IDE_DHEMI_PERIODO <= 201712)

df_refri_dia$CPROD_REFRIGERANTE_SEFAZ <- NA
df_refri_dia$CPROD_REFRIGERANTE_SEFAZ <- base::as.factor(df_refri_dia$CPROD_REFRIGERANTE_SEFAZ)
df_refri_class <- df_refri_dia%>%
  select(CPROD_REFRIGERANTE_SEFAZ,PROD_XPROD)
df_refri_class$CPROD_REFRIGERANTE_SEFAZ <- as.factor(df_refri_class$CPROD_REFRIGERANTE_SEFAZ)

df_refri_class$PROD_XPROD_LIMPO <- limpa_xprod_2(df_refri_class$PROD_XPROD)
df_refri_class$PROD_XPROD_LIMPO <- as.factor(df_refri_class$PROD_XPROD_LIMPO)

### CLASSIFICA DADOS
h2o.init(nthreads = -1, max_mem_size = "10G")

refri_dia.hex <- as.h2o(df_refri_class)
gbm_nfe <- h2o.upload_mojo("./GBM_REFRI_NFE.zip")
resultado <- h2o.predict(gbm_nfe,refri_dia.hex)


tb_resultado <- as.data.frame(resultado$predict)
df_refri_dia_class <- cbind(df_refri_dia,tb_resultado$predict)
df_refri_dia_class$CPROD_REFRIGERANTE_SEFAZ <- df_refri_dia_class$V2
df_refri_dia_class$V2 <- NULL



fwrite(df_refri_dia_class, "./TB_REFRI_2017_CLASS.csv", sep = ";")

h2o.shutdown(prompt = FALSE)
rm(df_refri_dia,tb_resultado,refri_dia.hex,resultado,gbm_nfe,df_refri_class,df_refri_dia_class)
gc(reset = TRUE)