## CARREGA BIBLIOTECAS
library(dplyr)
library(openxlsx)
library(data.table)
library(h2o)
library(bit64)

setwd("/home/local/FAZENDA/PROJETOS/CLASS_PROD")
nzodbc <- conect_NZ(nzodbc)

df_refri <- dbGetQuery(nzodbc,"SELECT T0.IDNFE, T0.DET_NITEM,
T1.IDE_DHEMI_PERIODO,T1.PROD_CEANTRIB, T1.prod_cean, T0.IDE_SUBGRUPO, T0.PROD_CPROD_SEFAZ_AJUSTADO, T0.prod_xprod_sefaz_ajustado, T1.prod_cfop,
SQLKIT..regexp_replace(T0.PROD_XPROD, ';','-') AS PROD_XPROD, SQLKIT..regexp_replace(T0.PROD_UCOM, ';','-') AS PROD_UCOM,
T0.PROD_QCOM, T1.PROD_VPROD, SQLKIT..regexp_replace(T1.PROD_UTRIB, ';','-') AS PROD_UTRIB,T1.PROD_QTRIB,T1.PROD_VUNTRIB,T1.PROD_VUNCOM,
T1.EMIT_CON_TIPO, T1.remetente_cnpj_cpf, T1.dest_cnpj_cpf_idestrangeiro, T1.recebedor_cnpj_cpf
FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE T0
LEFT JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM
WHERE T0.PROD_CPROD_SEFAZ_AJUSTADO = 10030440006
      AND T1.IDE_DHEMI_PERIODO BETWEEN 201501 AND 201507")


df_refri <- df_refrigerante%>%
  filter(IDE_DHEMI_PERIODO >= 201801 & IDE_DHEMI_PERIODO <= 201808)%>%
  select(PROD_XPROD)

## PREPARA CONJUNTO DE DADOS PARA TREINAMENTO
tb_rotulado <- read.xlsx("./DADOS/REFRIGERANTE_ROTULADO.xlsx",sheet = "REFRIGERANTE_ROTULADO")
tb_rotulado$TOTAL <- NULL
tb_rotulado <- tb_rotulado%>%
  group_by(PROD_XPROD,CPROD_REFRIGERANTE_SEFAZ)%>%
  summarise(TOTAL = n())



### SELECIONA BASE PARA TREINO DO MODELO
tb_treino2 <- full_join(df_refri,tb_rotulado, by = "PROD_XPROD")
tb_treino2$TOTAL <- NULL

## LIMPRA XPROD
tb_treino2$PROD_XPROD_LIMPO <- limpa_xprod_2(tb_treino2$PROD_XPROD)
tb_treino2$CPROD_REFRIGERANTE_SEFAZ <- as.factor(tb_treino2$CPROD_REFRIGERANTE_SEFAZ)
tb_treino2$PROD_XPROD_LIMPO <- as.factor(tb_treino2$PROD_XPROD_LIMPO)

rm(tb_rotulado,df_refri)
gc(reset = TRUE)

## INICIA h2o
h2o.init(nthreads = -1, max_mem_size = "10G")

refri_treino.hex <- as.h2o(tb_treino2)
refri_treino.hex$CPROD_REFRIGERANTE_SEFAZ <- as.factor(refri_treino.hex$CPROD_REFRIGERANTE_SEFAZ)
refri_treino.hex$PROD_XPROD_LIMPO <- as.factor(refri_treino.hex$PROD_XPROD_LIMPO)
refri_split = h2o.splitFrame(data = refri_treino.hex,
                             ratios = c(0.7),
                             destination_frames = c("refri.train.hex",
                                                    "refri.valida.hex"),
                             seed = 12345)

refri.train = refri_split[[1]]
refri.valida = refri_split[[2]]

t1 <- system.time()
## TREINA E VALIDA MODELO
gbm_3 <- h2o.gbm(model_id = "GBM_REFRI_NFE",
                 x = "PROD_XPROD_LIMPO",
                 y = "CPROD_REFRIGERANTE_SEFAZ",
                 distribution = "multinomial",
                 ntrees = 300,
                 max_depth = 5,
                 nfolds = 5,
                 seed = 1111,
                 training_frame = refri.train,
                 validation_frame = refri.valida)

## ANALISE DE DESEMPENHO
t2 <- system.time()
tmptot <- t2-t1
h2o.confusionMatrix(gbm_3, valid = TRUE)

h2o.save_mojo(gbm_3,path = "./MODELOS",force = TRUE)

h2o.shutdown(prompt = FALSE)





