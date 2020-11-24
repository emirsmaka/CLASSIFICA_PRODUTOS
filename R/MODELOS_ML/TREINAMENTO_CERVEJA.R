setwd("./home/local/FAZENDA/PROJETOS/CLASS_PROD")
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(h2o)
library(stringr)

con_Netezza <- conect_NZ(con_Netezza)

## CARREGA DADOS PARA TREINAR O MODELO
cerveja_rotulos <- read.xlsx("./DADOS/CERVEJAS_ROTULADAS.xlsx", sheet = "CERVEJAS_ROTULOS")
tb_cerveja$PROD_XPROD <- as.factor(tb_cerveja$PROD_XPROD)

tb_cerveja$CPROD_CERVEJA_SEFAZ <- as.factor(tb_cerveja$CPROD_CERVEJA_SEFAZ)
df_nfe_cerveja2018 <- dbGetQuery(con_Netezza,"SELECT PROD_CEAN, PROD_XPROD FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE
                               WHERE IDE_DHEMI_PERIODO BETWEEN 201801 AND 201809 AND
                               PROD_CPROD_SEFAZ_AJUSTADO = 10030440008",believeNRows=FALSE)


## PREPARA CONJUNTO DE DADOS DE TREINAMENTO
df_cerv_treino <- full_join(df_cerveja,cerveja_rotulos,by = "PROD_XPROD")

################################## FIM PREPARACAO DOS DADOS PARA TREINAMENTO MODELO ##################################


######## LIMPA XPROD  ########
cerveja_treino <- df_cerv_treino%>%
  select(PROD_XPROD,CPROD_CERVEJA_SEFAZ)

cerveja_treino <- rename(cerveja_treino,CPROD_CERVEJA_SEFAZ = CPROD_CERVEJA_SEFAZ.y)

cerveja_treino$PROD_XPROD_LIMPO <- gsub("[[:digit:]]|[[:punct:]]"," ",cerveja_treino$PROD_XPROD)
cerveja_treino$PROD_XPROD_LIMPO <- str_trim(cerveja_treino$PROD_XPROD_LIMPO,side = "left")

remove_words <- c("cx|ml|cartao|six\\s?pack|cxa|\\ssh\\s|l?gfa|\\sl\\s|\\slt(\\s)?|vd|\\sx\\s|ln|\\s(c)\\s|npal|un(id)?|\\scom\\s|ttc|pct?|\\sc\\s")
remove_words2 <- c("lata(s|o)?|pack|alcool|\\sc\\s|sleek|\\scom\\s|\\sp\\s")
remove_words3 <- c("\\scom(\\s)?|\\sfi(\\s)?|\\sf\\s|pe\\sec\\srd|\\sprec\\s|\\sret\\s|\\spbr(\\s)?|\\su\\s|\\sgfs(\\s)?|^i\\s|\\sn$|\\sow|\\sd(\\s|$)|fora de linha|
                   garrfa|garrafa|long neck|caixa|cart\\s|\\spap|fridge")
cerveja_treino$PROD_XPROD_LIMPO2 <- gsub(remove_words,"",cerveja_treino$PROD_XPROD_LIMPO,ignore.case = T)
cerveja_treino$PROD_XPROD_LIMPO3 <- gsub(remove_words2,"",cerveja_treino$PROD_XPROD_LIMPO2,ignore.case = T)
cerveja_treino$PROD_XPROD_LIMPO <- NULL
cerveja_treino$PROD_XPROD_LIMPO2 <- NULL

cerveja_treino$PROD_XPROD_LIMPO4 <- gsub(remove_words3,"",cerveja_treino$PROD_XPROD_LIMPO3,ignore.case = T)
cerveja_treino$PROD_XPROD_LIMPO3 <- NULL
cerveja_treino <- rename(cerveja_treino,PROD_XPROD_LIMPO = PROD_XPROD_LIMPO4)

cerveja_treino$PROD_XPROD_LIMPO <- str_trim(cerveja_treino$PROD_XPROD_LIMPO,side = "left")
cerveja_treino$PROD_XPROD_LIMPO <- str_trim(cerveja_treino$PROD_XPROD_LIMPO,side = "right")

cerveja_treino$PROD_XPROD_LIMPO <- ifelse(cerveja_treino$PROD_XPROD_LIMPO == "","NULO",cerveja_treino$PROD_XPROD_LIMPO)
cerveja_treino$PROD_XPROD_LIMPO <- tolower(cerveja_treino$PROD_XPROD_LIMPO)

cerveja_treino$PROD_XPROD_LIMPO <- as.factor(cerveja_treino$PROD_XPROD_LIMPO)
cerveja_treino$CPROD_CERVEJA_SEFAZ <- as.factor(cerveja_treino$CPROD_CERVEJA_SEFAZ)
######## FIM LIMPA XPROD ########

cerveja_treino <- cerveja_treino%>%
  arrange(CPROD_CERVEJA_SEFAZ)
######## PREPARA A SAMPLE_RATE_PER_CLASS ########

tb_rate <- df_cerv_treino%>%
  group_by(CPROD_CERVEJA_SEFAZ)%>%
  summarise(TOTAL = n())%>%
  arrange(CPROD_CERVEJA_SEFAZ)
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
                    ntrees = 350,
                    max_depth = 6,
                    nfolds = 6,
                    seed = 11111,
                    sample_rate_per_class = list_rate,
                    training_frame = cerveja_treino.hex,
                    validation_frame = cerveja_valida.hex)


h2o.confusionMatrix(gbm_cerv,valid = TRUE)

h2o.saveModel(gbm_cerv,path = "./MODELOS",force = TRUE)


h2o.shutdown(prompt = FALSE)
gc(reset = T)













### AJUSTA OS PESOS
#prop.table(table(df_refri_train_2$CPROD_REFRIGERANTE_SEFAZ))
#y <- df_cerv_rotulado$CPROD_CERVEJA_SEFAZ
#wts <- 1/table(y)

