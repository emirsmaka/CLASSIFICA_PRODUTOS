
df_refri_class <- refri_agrupa%>%
  select(PROD_XPROD)

df_refri_class[,"CPROD_REFRIGERANTE_SEFAZ"] <- as.integer()
df_refri_class$CPROD_REFRIGERANTE_SEFAZ <- as.factor(df_refri_class$CPROD_REFRIGERANTE_SEFAZ)

df_refri_class <- fn_limpa_xprod(df_refri_class)
df_refri_class$PROD_XPROD_LIMPO <- as.factor(df_refri_class$PROD_XPROD_LIMPO)

### CLASSIFICA DADOS
#### INICIAR O H2O MANUALMENTE PELO PROMPT DO DOS EM:
#### CD\Users\Avell\Documents\R\win-library\4.0\h2o\java
#### USANDO O COMANDO java - jar h2o.jar

h2o.init(nthreads = -1, max_mem_size = "10G",startH2O = FALSE)
h2o.removeAll()

refri_dia.hex <- as.h2o(df_refri_class)
gbm_refri_nfe <- h2o.upload_mojo("./R/MODELOS_ML/GBM_REFRI_NFE.zip")

resultado <- h2o.predict(gbm_refri_nfe,refri_dia.hex)

tb_resultado <- as.data.frame(resultado$predict)
df_refri_dia_class <- cbind(df_refrigerante,tb_resultado$predict)
df_refri_dia_class <- rename(df_refri_dia_class,CPROD_REFRIGERANTE_SEFAZ = `tb_resultado$predict`)

## CARREGA TABELA DE REFRIGERANTES
tb_refrigerante <- read.xlsx("C:/DADOS_R/Scripts/REFRIGERANTE_ROTULADO.xlsx", sheet = "TB_REFRI")
tb_refrigerante$CPROD_REFRIGERANTE_SEFAZ <- as.factor(tb_refrigerante$CPROD_REFRIGERANTE_SEFAZ)
####################################
## MESCLA TB_REFRIGERANTE COM REFRIGERANTE CLASSIFICADO P/ INSERIR A DESCRICAO DO PRODUTO
df_refrigerante <- left_join(df_refri_dia_class,tb_refrigerante,by = "CPROD_REFRIGERANTE_SEFAZ")

h2o.shutdown(prompt = FALSE)
rm(df_refri_dia,tb_resultado,refri_dia.hex,resultado,df_refri_class,df_refri_dia_class,tab_cerveja,tb_refrigerante,gbm_refri_nfe,gbm_cerv)
gc(reset = TRUE)
