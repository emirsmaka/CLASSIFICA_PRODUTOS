h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()
## 
tab_cerveja <- read.xlsx("./DADOS/CERVEJAS_ROTULADAS.xlsx",sheet = "TAB_CERVEJAS")
tab_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(tab_cerveja$CPROD_CERVEJA_SEFAZ)
##
df_cerveja[,"CPROD_CERVEJA_SEFAZ"] <- as.integer()
df_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(df_cerveja$CPROD_CERVEJA_SEFAZ)

cerveja_class <- df_cerveja%>%
  select(PROD_XPROD,CPROD_CERVEJA_SEFAZ)

cerveja_class <- fn_limpa_xprod(cerveja_class)

cerveja.hex <- as.h2o(cerveja_class)
gbm_cerv <- h2o.upload_mojo("./MODELOS/GBM_CERVEJA_NFE.zip")

cerv_class <- h2o.predict(gbm_cerv,cerveja.hex)

df_cerveja_class <- as.data.frame(cerv_class$predict)

df_cerveja <- cbind(df_cerveja,df_cerveja_class)
df_cerveja$CPROD_CERVEJA_SEFAZ <- df_cerveja$predict
df_cerveja$predict <- NULL

## INSERE DESCRICAO PADRONIZADA DO PRODUTO CERVEJA
df_cerveja <- left_join(df_cerveja,tab_cerveja, by = "CPROD_CERVEJA_SEFAZ")

rm(cerv_class,cerveja.hex,df_cerveja_class,gbm_cerv,cerveja_class)
gc(reset = TRUE)

h2o.shutdown(prompt = FALSE)

####################################### FIM CLASSIFICADOR CERVEJA #######################################