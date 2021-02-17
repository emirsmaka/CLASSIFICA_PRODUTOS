library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(RODBC)
library(stringr)

df_agua <- dbGetQuery(on_Netezza,"SELECT PROD_XPROD FROM TRIBUTARIO_REFERENCIA.ADMIN.TB_PRODUTO_SEFAZ_NFE
                               WHERE IDE_DHEMI_PERIODO BETWEEN 201701 AND 201912 AND
                               PROD_CPROD_SEFAZ_AJUSTADO = 10030440001",believeNRows=FALSE)

df_agua <- df_agua%>%
  group_by(PROD_XPROD)%>%
  summarise(TOTAL = n())%>%
  arrange(desc(TOTAL))

df_agua$PROD_XPROD <- gsub(";","-",df_agua$PROD_XPROD)

fwrite(df_agua,"C:/DADOS_R/Scripts/AGUA/agua.csv",sep = ";")
