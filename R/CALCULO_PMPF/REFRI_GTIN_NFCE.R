###############################################################################
##  PROJETO....: CALCULO PMPF REFRIGERANTES PELOS PRECOS NFCE                ##
##  SCRIPT.....: REALIZA CALCULOS DE MEDIA                                   ##
##  DATA.......: 15 fevereiro 2021                                           ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

source("./SCRIPTS/REFRIGERANTE/CALCULO_PMPF/CARGA_DADOS_REFRIGERANTE.R")

tb_bebida_class$PROD_CEANTRIB <- as.character(tb_bebida_class$PROD_CEANTRIB)
tb_bebida_class$PROD_CEANTRIB <- as.factor(tb_bebida_class$PROD_CEANTRIB)

### AGRUPAR POR PROD_XPROD, XPROD_SEFAZ, CEANTRIB, VOLUME_SEFAZ
df_beb_max <- tb_bebida_class%>%
  filter(PROD_XPROD_SEFAZ_DETALHADO != "DEMAIS REFRIGERANTES")

tb_refri_lista <- df_beb_max%>%
  group_by(PROD_CEANTRIB,PROD_CPROD_SEFAZ_DETALHADO,PROD_XPROD_SEFAZ_DETALHADO,
           PROD_VOLUME_SEFAZ_AJUSTADO,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO
           ,PROD_EMBALAGEM_AJUSTADO)%>%
  summarise(TOTAL = n())%>%
  arrange(desc(TOTAL))
  
tb_refri_lista <- as.data.table(tb_refri_lista)

#### FUNCAO ENCONTRAR MAIOR FREQUENCIA POR GTIN
max_total <- function(x){
  return(max(x))
}

lst_refri_class <- list()
lst_refri_class <- split(x=tb_refri_lista,f=tb_refri_lista$PROD_CEANTRIB)
maior_freq <- lapply(lst_refri_class, function(x) max_total(x$TOTAL))
refri_maior_freq <- ldply(maior_freq,data.frame)
refri_maior_freq <-  rename(refri_maior_freq,PROD_CEANTRIB = '.id',TOTAL = 'X..i..')
df_refri_class <- inner_join(refri_maior_freq,tb_refri_lista,by=c("PROD_CEANTRIB","TOTAL"))

rm(lst_refri_class,maior_freq,refri_maior_freq,max_total,tb_refri_lista,tb_bebida_class
   ,df_beb_class,df_beb_max)
gc(reset = T)


#### INSERIR COLUNAS AJUSTADAS
df_beb_class <- df_beb_class %>%
  distinct(PROD_CEANTRIB,PROD_CPROD_SEFAZ_DETALHADO,PROD_XPROD_SEFAZ_DETALHADO,
           PROD_VOLUME_SEFAZ_AJUSTADO,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO
           ,PROD_EMBALAGEM_AJUSTADO)

x <- inner_join(tb_valor_medio,df_refri_class,by=c("CEANTRIB_NFCE" = "PROD_CEANTRIB"))
y <- inner_join(tb_valor_medio_contrib,df_refri_class,c("CEANTRIB_NFCE" = "PROD_CEANTRIB"))

































