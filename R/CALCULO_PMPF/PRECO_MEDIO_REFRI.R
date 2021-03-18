library(dplyr)
library(data.table)
library(bit64)
library(RODBC)
library(plyr)

df_refri_recorte <- df_refrigerante%>%
  sample_n(5500000)

df_refri_recorte$VLR_UNITARIO_SEFAZ <- ifelse(is.na(df_refri_recorte$VLR_UNITARIO_SEFAZ),-1,df_refri_recorte$VLR_UNITARIO_SEFAZ)

df_refri_vlrun <- df_refri_recorte%>%
  filter(VLR_UNITARIO_SEFAZ != -1 & VOLUME_SEFAZ != -1)%>%
  select(CPROD_REFRIGERANTE_SEFAZ,VOLUME_SEFAZ,VLR_UNITARIO_SEFAZ)

## CRIA LISTA PARA SEPARAR DATAFRAME POR CPROD_REFRIGERANTE_SEFAZ & VOLUME_SEFAZ
lista <- list()
lista <- split(df_refri_vlrun,list(df_refri_vlrun$CPROD_REFRIGERANTE_SEFAZ,df_refri_vlrun$VOLUME_SEFAZ),drop = TRUE)

## CALCULA OUTLIAERS POR CPROD_REFRIGERANTE_SEFAZ & VOLUME_SEFAZ E COLOCA EM UMA LISTA
out_refri <- lapply(lista, function(x) out_total(x$VLR_UNITARIO_SEFAZ))

## CONVERTE LISTA DE OUTLIERS EM DATAFRAME
df_refri_out <- ldply (out_refri, data.frame)
df_refri_split <- ldply (lista, data.frame)

### JUNTA COLUNA OUTLIER SUPERIOR 
df_refri_split <- cbind(df_refri_split,df_refri_out$X..i..)
rm(lista,out_refri,df_refri_out,df_refri_vlrun)
##
### EXCLUI OUTLIERS 
df_refri_vlr <- df_refri_split%>%
  filter(`df_refri_out$X..i..` == FALSE)
### CALCULA MEDIANA E MEDIA VLR_UNITARIO
lista <- split(df_refri_vlr,list(df_refri_vlr$CPROD_REFRIGERANTE_SEFAZ,df_refri_vlr$VOLUME_SEFAZ),drop = TRUE)
median_refri <- lapply(lista, function(x) median(x$VLR_UNITARIO_SEFAZ))
mean_refri <- lapply(lista, function(x) mean(x$VLR_UNITARIO_SEFAZ))
### CONVERTE LISTAS MEDIA E MEDIANA EM DATAFRAME
df_media <- ldply(mean_refri,data.frame)
df_mediana <- ldply(median_refri,data.frame)















