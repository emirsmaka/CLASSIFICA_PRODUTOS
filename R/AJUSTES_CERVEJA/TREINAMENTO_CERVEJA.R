source("./SCRIPTS/FUNCOES/FN_CONEXAO_NETEZZA.R")
NZ_TribRef <- fn_conect_NZ(NZ_TribRef)

## CARREGA DADOS PARA TREINAR O MODELO
tb_cerveja <- read.xlsx("./DADOS/CERVEJAS_ROTULADAS.xlsx", sheet = "CERVEJAS_ROTULOS")
tb_cerveja$PROD_XPROD <- as.factor(tb_cerveja$PROD_XPROD)
tb_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(tb_cerveja$CPROD_CERVEJA_SEFAZ)

df_nfe_cerveja <- dbGetQuery(NZ_TribRef,"SELECT PROD_XPROD FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE
                               WHERE IDE_DHEMI_PERIODO BETWEEN 202001 AND 202012 AND
                               PROD_CPROD_SEFAZ_AJUSTADO = 10030440008",believeNRows=FALSE)


## PREPARA CONJUNTO DE DADOS DE TREINAMENTO
cerveja_treino <- full_join(df_nfe_cerveja,tb_cerveja,by = "PROD_XPROD")

######## LIMPA XPROD  ########
source("./SCRIPTS/FUNCOES/FN_LIMPA_XPROD.R")
cerveja_treino <- fn_limpa_xprod(cerveja_treino)
cerveja_treino$CPROD_CERVEJA_SEFAZ <- as.factor(cerveja_treino$CPROD_CERVEJA_SEFAZ)
######## FIM LIMPA XPROD ######## 

################################## FIM PREPARACAO DOS DADOS PARA TREINAMENTO MODELO ##################################
cerveja_treino <- cerveja_treino%>%
  arrange(CPROD_CERVEJA_SEFAZ)
######## PREPARA A SAMPLE_RATE_PER_CLASS ########
tb_rate <- cerveja_treino%>%
  group_by(CPROD_CERVEJA_SEFAZ)%>%
  summarise(TOTAL = n())%>%
  arrange(CPROD_CERVEJA_SEFAZ)
tb_rate <- as.data.frame(tb_rate)

tb_rate <- tb_rate%>%
  filter(CPROD_CERVEJA_SEFAZ != is.na(CPROD_CERVEJA_SEFAZ))

tb_rate$RATE <- ifelse(tb_rate$TOTAL < 50000,1,0.6)
list_rate <- c(tb_rate$RATE)
######## FIM PREPARA A SAMPLE_RATE_PER_CLASS ######## 

## INICIA TREINAMENTO DO MODELO
h2o.init(nthreads = -1, max_mem_size = "10G")
h2o.removeAll()

cerveja.hex <- as.h2o(cerveja_treino)

cerveja_split.hex <- h2o.splitFrame(data = cerveja.hex,
                                    ratios = 0.7,
                                    seed = 12345)
cerveja_treino.hex <- cerveja_split.hex[[1]]
cerveja_valida.hex <- cerveja_split.hex[[2]]


gbm_cerv <- h2o.gbm(model_id = "GBM_CERVEJA_NFE",
                    x = "PROD_XPROD_LIMPO",
                    y = "CPROD_CERVEJA_SEFAZ",
                    distribution = "multinomial",
                    ntrees = 450,
                    max_depth = 6,
                    nfolds = 6,
                    seed = 11111,
                    sample_rate_per_class = list_rate,
                    training_frame = cerveja_treino.hex,
                    validation_frame = cerveja_valida.hex)


matriz_conf_cerveja <- h2o.confusionMatrix(gbm_cerv,valid = TRUE)
fwrite(matriz_conf_cerveja,"./MODELOS/matriz_conf_cerveja.csv")

h2o.save_mojo(gbm_cerv,path = "./MODELOS",force = TRUE)

rm(list = ls())
gc(reset = T)
h2o.shutdown(prompt = FALSE)