setwd("./home/local/FAZENDA/PROJETOS/CLASS_PROD")
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(h2o)

con_Netezza <- conect_NZ(con_Netezza)

## CARREGA DADOS PARA TREINAR O MODELO
tb_cerveja <- read.xlsx("./DADOS/CERVEJAS_ROTULADAS.xlsx", sheet = "CERVEJAS_ROTULOS")
tb_cerveja$PROD_XPROD <- as.factor(tb_cerveja$PROD_XPROD)
tb_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(tb_cerveja$CPROD_CERVEJA_SEFAZ)
df_nfe_cerveja2018 <- dbGetQuery(con_Netezza,"SELECT PROD_XPROD FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE
                               WHERE IDE_DHEMI_PERIODO BETWEEN 201801 AND 201809 AND
                               PROD_CPROD_SEFAZ_AJUSTADO = 10030440008",believeNRows=FALSE)


## PREPARA CONJUNTO DE DADOS DE TREINAMENTO
df_cerv_treino <- full_join(df_nfe_cerveja2018,tb_cerveja,by = "PROD_XPROD")
df_cerv_treino$PROD_XPROD_LIMPO <- limpa_xprod_2(df_cerv_treino$PROD_XPROD)
df_cerv_treino$PROD_XPROD_LIMPO <- as.factor(df_cerv_treino$PROD_XPROD_LIMPO)


rm(df_nfe_cerveja2018,tb_cerveja)
gc(reset = T)
################################## FIM PREPARACAO DOS DADOS PARA TREINAMENTO MODELO ##################################

######## PREPARA A SAMPLE_RATE_PER_CLASS ########
x <- df_cerv_treino%>%
  group_by(CPROD_CERVEJA_SEFAZ)%>%
  summarise(TOTAL = n())

list_rate <- c(ifelse(x$TOTAL < 300000, 1, 0.4))

rm(x)
######## FIM PREPARA A SAMPLE_RATE_PER_CLASS ########


## INICIA TREINAMENTO DO MODELO
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

cerveja.hex <- as.h2o(df_cerv_treino)

cerveja_split.hex <- h2o.splitFrame(data = cerveja.hex,
                                    ratios = 0.7,
                                    seed = 12345)
cerveja_treino.hex <- cerveja_split.hex[[1]]
cerveja_valida.hex <- cerveja_split.hex[[2]]

resposta <- 'CPROD_CERVEJA_SEFAZ'

h2o.table(cerveja_treino.hex[resposta])



gbm_cerv <- h2o.gbm(model_id = "GBM_CERVEJA_NFE",
                    x = "PROD_XPROD_LIMPO",
                    y = "CPROD_CERVEJA_SEFAZ",
                    distribution = "multinomial",
                    ntrees = 350,
                    max_depth = 5,
                    nfolds = 5,
                    seed = 11111,
                    sample_rate_per_class = list_rate,
                    training_frame = cerveja_treino.hex,
                    validation_frame = cerveja_valida.hex)


h2o.confusionMatrix(gbm_cerv,valid = TRUE)

h2o.saveModel(gbm_cerv,path = "./MODELOS",force = TRUE)


h2o.shutdown(prompt = FALSE)














### AJUSTA OS PESOS
#prop.table(table(df_refri_train_2$CPROD_REFRIGERANTE_SEFAZ))
#y <- df_cerv_rotulado$CPROD_CERVEJA_SEFAZ
#wts <- 1/table(y)



