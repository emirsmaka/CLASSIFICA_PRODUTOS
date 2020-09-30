setwd("C:/DADOS_R/Scripts")
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(h2o)


## CARREGA DADOS PARA TREINAR O MODELO
tb_cerveja <- read.xlsx("./CERVEJAS_ROTULADAS.xlsx", sheet = "CERVEJAS_ROTULOS")
tb_cerveja$PROD_XPROD <- as.factor(tb_cerveja$PROD_XPROD)
tb_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(tb_cerveja$CPROD_CERVEJA_SEFAZ)
df_nfe_cerveja2018 <- fread("./CERVEJAS_2018.csv",dec = ",",stringsAsFactors = T,integer64 = "numeric")



df_cerveja <- df_nfe_cerveja2018%>%
  filter(IDE_DHEMI_PERIODO >= 201801 & IDE_DHEMI_PERIODO <= 201809)%>%
  select(PROD_XPROD)


## PREPARA CONJUNTO DE DADOS DE TREINAMENTO
df_cerv_treino <- full_join(df_cerveja,tb_cerveja,by = "PROD_XPROD")
df_cerv_treino$PROD_XPROD_LIMPO <- limpa_xprod_2(df_cerv_treino$PROD_XPROD)
df_cerv_treino$PROD_XPROD_LIMPO <- as.factor(df_cerv_treino$PROD_XPROD_LIMPO)


rm(df_cerveja,df_nfe_cerveja2018,tb_cerveja)
gc(reset = T)
################################## FIM PREPARACAO DOS DADOS PARA TREINAMENTO MODELO ##################################

## INICIA TREINAMENTO DO MODELO
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

cerveja.hex <- as.h2o(df_cerv_treino)

cerveja_split.hex <- h2o.splitFrame(data = cerveja.hex,
                                    ratios = 0.7,
                                    destination_frames = c("cerveja_treino.hex",
                                                           "cerveja_valida.hex"),
                                    seed = 12345)
cerveja_treino.hex <- cerveja_split.hex[[1]]
cerveja_valida.hex <- cerveja_split.hex[[2]]

gbm_cerv <- h2o.gbm(model_id = "GBM_CERVEJA_NFE",
                    x = "PROD_XPROD_LIMPO", 
                    y = "CPROD_CERVEJA_SEFAZ",
                    distribution = "multinomial",
                    ntrees = 350,
                    max_depth = 5,
                    nfolds = 5,
                    seed = 11111,
                    training_frame = cerveja_treino.hex,
                    validation_frame = cerveja_valida.hex)


h2o.confusionMatrix(gbm_cerv,valid = TRUE)

h2o.saveModel(gbm_cerv,path = "./",force = TRUE)


h2o.shutdown(prompt = FALSE)














### AJUSTA OS PESOS
prop.table(table(df_refri_train_2$CPROD_REFRIGERANTE_SEFAZ))
y <- df_cerv_rotulado$CPROD_CERVEJA_SEFAZ
wts <- 1/table(y)



















covtype <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/covtype/covtype.20k.data")