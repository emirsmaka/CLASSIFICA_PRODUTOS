##################################################################################
#         **** AJUSTES DE VOLUME E UNIDADE MEDIDA PARA CERVEJAS ****             #
#
#
#


### AJUSTES DO VOLUME E UNIDADE MEDIDA (ML)
df_sem_volume <- df_bebidas_nfe%>%
  filter(is.na(VOLUME_SEFAZ))

df_bebidas_nfe <- setdiff(df_bebidas_nfe,df_sem_volume)
id_ajuste <- grep("\\d{1,3}(\\s)?l",df_sem_volume$PROD_XPROD,ignore.case = T)

df_sem_volume$VOLUME_SEFAZ[id_ajuste] <- str_extract(tolower(df_sem_volume$PROD_XPROD[id_ajuste]),"\\d{1,3}(,\\d{1,3})?(\\s)?l")
df_sem_volume$VOLUME_SEFAZ[id_ajuste] <- gsub(",",".",df_sem_volume$VOLUME_SEFAZ[id_ajuste] )
df_sem_volume$VOLUME_SEFAZ[id_ajuste] <- as.double(str_extract(df_sem_volume$VOLUME_SEFAZ[id_ajuste],"\\d{1,3}(\\.\\d{1,3})?"))
df_sem_volume$VOLUME_SEFAZ <- as.double(df_sem_volume$VOLUME_SEFAZ)
df_sem_volume$UN_MEDIDA_SEFAZ[id_ajuste] <- ifelse(df_sem_volume$VOLUME_SEFAZ[id_ajuste] < 1,"ML","L")
df_sem_volume$VOLUME_SEFAZ[id_ajuste]  <- ifelse(df_sem_volume$VOLUME_SEFAZ[id_ajuste]  < 1,df_sem_volume$VOLUME_SEFAZ[id_ajuste] *1000,df_sem_volume$VOLUME_SEFAZ[id_ajuste] )
df_bebidas_nfe <- rbind(df_bebidas_nfe,df_sem_volume)
rm(id_ajuste,df_sem_volume)

###

### VOLUME == ML
df_sem_volume <- df_bebidas_nfe%>%
  filter(is.na(VOLUME_SEFAZ))
df_bebidas_nfe <- setdiff(df_bebidas_nfe,df_sem_volume)
id_ml <- grep("210|250|269|275|278|300|310|330|343|350|355|375|440|473|500|550|600|710|740|750",df_sem_volume$PROD_XPROD,ignore.case = T)
df_sem_volume$VOLUME_SEFAZ[id_ml] <- as.double(str_extract(tolower(df_sem_volume$PROD_XPROD[id_ml]),
                                                           "210|250|269|275|278|300|310|330|343|350|355|375|440|473|500|550|600|710|740|750"))
df_sem_volume$UN_MEDIDA_SEFAZ[id_ml] <- "ML"
df_bebidas_nfe <- rbind(df_bebidas_nfe,df_sem_volume)
rm(df_sem_volume,id_ml)

### VOLUME == LITRO
id_litro <- grep("litrao|litro",df_bebidas_nfe$PROD_XPROD,ignore.case = T)
df_bebidas_nfe$VOLUME_SEFAZ[id_litro] <- ifelse(is.na(df_bebidas_nfe$VOLUME_SEFAZ[id_litro]) & 
                                                  df_bebidas_nfe$CPROD_CERVEJA_SEFAZ[id_litro] != 4000802000,1,df_bebidas_nfe$VOLUME_SEFAZ[id_litro])
rm(id_litro)


#############################################
### AJUSTA VOLUMES

df_bebidas_nfe$UN_MEDIDA_SEFAZ <- ifelse(df_bebidas_nfe$UN_MEDIDA_SEFAZ == "L" & df_bebidas_nfe$VOLUME_SEFAZ > 100 & df_bebidas_nfe$VOLUME_SEFAZ < 1000, 
                                         "ML", df_bebidas_nfe$UN_MEDIDA_SEFAZ)


id_l <- ifelse(df_bebidas_nfe$VOLUME_SEFAZ > 999 & df_bebidas_nfe$UN_MEDIDA_SEFAZ == "ML",TRUE,FALSE)
y <- df_bebidas_nfe[id_l]
df_bebidas_nfe <- setdiff(df_bebidas_nfe,y)
y$VOLUME_SEFAZ <- round(y$VOLUME_SEFAZ/1000, digits = 2) 
y$UN_MEDIDA_SEFAZ <- "L"

df_bebidas_nfe <- rbind(df_bebidas_nfe,y)
rm(id_l,y)


### AJUSTA VOLUME == 0
df_sem_volume <- df_bebidas_nfe%>%
  filter(VOLUME_SEFAZ == 0)
df_bebidas_nfe <- setdiff(df_bebidas_nfe,df_sem_volume)
id_ml <- grep("\\d{1},\\d{2,3}(\\s)?l",df_sem_volume$PROD_XPROD,ignore.case = T)
df_sem_volume$VOLUME_SEFAZ[id_ml] <- as.double(gsub(",",".",str_extract(df_sem_volume$PROD_XPROD[id_ml],"\\d{1},\\d{1,3}")))
df_sem_volume$VOLUME_SEFAZ[id_ml] <- ifelse(df_bebidas_nfe$VOLUME_SEFAZ[id_ml] < 1 & df_bebidas_nfe$UN_MEDIDA_SEFAZ[id_ml] == "ML",df_bebidas_nfe$VOLUME_SEFAZ[id_ml] * 1000,
                                            df_bebidas_nfe$VOLUME_SEFAZ[id_ml])

df_bebidas_nfe <- rbind(df_bebidas_nfe,df_sem_volume)
rm(id_ml,df_sem_volume)


###########################
gc(reset = TRUE)
###########################

