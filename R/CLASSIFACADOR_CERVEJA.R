setwd("C:/DADOS_R/Scripts")
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

## CARREGA DADOS PARA CLASSIFICAR
df_cerveja <- fread("./CERVEJAS_2019.csv",dec = ",",stringsAsFactors = T,integer64 = "numeric")
df_cerveja_class <- df_cerveja[,9]
df_cerveja_class$CPROD_CERVEJA_SEFAZ <- as.factor("")

## 

cerveja.hex <- as.h2o(df_cerveja_class)
gbm_cerv <- h2o.upload_mojo("./GBM_CERVEJA_NFE.zip")
cerv_class <- h2o.predict(gbm_cerv,cerveja.hex)

df_cerveja_class <- as.data.frame(cerv_class$predict)

df_cerveja_class <- cbind(df_cerveja_class$predict,df_cerveja)

df_cerveja_class <- rename(df_cerveja_class, CPROD_CERVEJA_SEFAZ = "V1")

h2o.shutdown(prompt = FALSE)
df_cerveja <- cbind(df_cerveja, df_cerveja_class$predict)
rm(cerv_class,cerveja.hex,df_cerveja,gbm_cerv)
gc(reset = TRUE)
























