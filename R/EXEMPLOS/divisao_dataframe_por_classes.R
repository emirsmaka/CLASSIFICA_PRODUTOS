### IDENTIFICA OUTLIERS EM PROD_VUNCOM PARA PROD_UCOM == UN
library(plyr)


i_un <- grep("^un$",df_cerveja$PROD_UCOM,ignore.case = T)
df_un <- df_cerveja[i_un,]%>%
  filter(CPROD_CERVEJA_SEFAZ != -9999999)%>%
  select(CPROD_CERVEJA_SEFAZ,PROD_XPROD,PROD_UCOM,PROD_QCOM,PROD_VUNCOM,PROD_VPROD)%>%
  arrange(CPROD_CERVEJA_SEFAZ)

## CRIA LISTA PARA SEPARAR DATAFRAME POR CPROD_CERVEJA_SEFAZ
lista <- list()
lista <- split(x=df_un,f=df_un$CPROD_CERVEJA_SEFAZ)
## CALCULA OUTLIAERS POR CPROD_CERVEJA_SEFAZ E COLOCA EM UMA LISTA
out_cerv <- lapply(lista, function(x) out_superior(x$PROD_VUNCOM))

## CONVERTE LISTA DE OUTLIERS EM DATAFRAME
df_out <- ldply (out_cerv, data.frame)

