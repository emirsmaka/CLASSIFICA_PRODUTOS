###############
#### AJUSTA VLR_UNITARIO_SEFAZ E QTE_SEFAZ NAO AJUSTADOS
df_refrigerante <- df_refrigerante%>%
  filter(is.na(VLR_UNITARIO_SEFAZ) & FATOR_MULTIPLICADOR > 0 & PROD_QCOM > 0)%>%
  mutate(QTE_SEFAZ = PROD_QCOM * FATOR_MULTIPLICADOR)%>%
  mutate(VLR_UNITARIO_SEFAZ = PROD_VPROD / QTE_SEFAZ)

df_refri_bkp <- anti_join(df_refri_bkp,df_refrigerante,by = c("IDNFE","DET_NITEM"))
df_refrigerante <- rbind(df_refrigerante,df_refri_bkp)
rm(df_refri_bkp)
gc(reset = T)
######################################################################################

#### RENOMEAR COLUNAS PARA PADRAO SEFAZ
df_refrigerante <- rename(df_refrigerante,PROD_CPROD_SEFAZ_DETALHADO = 'CPROD_REFRIGERANTE_SEFAZ')
df_refrigerante <- rename(df_refrigerante,PROD_XPROD_SEFAZ_DETALHADO = 'PROD_XPROD_SEFAZ')
df_refrigerante <- rename(df_refrigerante,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO = 'UN_MEDIDA_SEFAZ')
df_refrigerante <- rename(df_refrigerante,PROD_VOLUME_SEFAZ_AJUSTADO = 'VOLUME_SEFAZ')
df_refrigerante <- rename(df_refrigerante,PROD_FATOR_MULTIPLICADOR = 'FATOR_MULTIPLICADOR')
df_refrigerante <- rename(df_refrigerante,PROD_USEFAZ_AJUSTADO = 'UNIDADE_SEFAZ')
df_refrigerante <- rename(df_refrigerante,PROD_QSEFAZ_AJUSTADO = 'QTE_SEFAZ')
df_refrigerante <- rename(df_refrigerante,PROD_VUNSEFAZ_AJUSTADO = 'VLR_UNITARIO_SEFAZ')
df_refrigerante[,"PROD_EMBALAGEM_AJUSTADO"] <- as.character()
######################################################################################

#### CONVERTER UNIDADE MEDIDA DE L PARA ML BEM COMO O VOLUME
df_refrigerante$PROD_VOLUME_SEFAZ_AJUSTADO <-  ifelse(df_refrigerante$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO == 'L'
                                                      ,df_refrigerante$PROD_VOLUME_SEFAZ_AJUSTADO * 1000
                                                      ,df_refrigerante$PROD_VOLUME_SEFAZ_AJUSTADO)

df_refrigerante$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO <-  ifelse(df_refrigerante$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO == 'L'
                                                              ,'ML',df_refrigerante$PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO)
######################################################################################

#### IDENTIFICA TIPO DE EMBALAGEM
i_embalagem <-  grep("\\<pet\\>",df_refrigerante$PROD_XPROD,ignore.case = T)
df_refri_pet <- df_refrigerante[i_embalagem,]
df_refrigerante <- anti_join(df_refrigerante,df_refri_pet,by=c("IDNFE","DET_NITEM"))

i_ml <- ifelse(df_refrigerante$PROD_VOLUME_SEFAZ_AJUSTADO < 1000,TRUE,FALSE)

df_refri_ml <- df_refrigerante[i_ml,]
df_refrigerante <- anti_join(df_refrigerante,df_refri_ml,by=c("IDNFE","DET_NITEM"))
i_ks <- grep("\\<ks\\>",df_refri_ml$PROD_XPROD,ignore.case = T)
i_lata <- grep("\\<lt\\>|\\<lata\\>|\\<lta\\>",df_refri_ml$PROD_XPROD,ignore.case = T)
df_refri_ks <- df_refri_ml[i_ks,]
df_refri_lata <- df_refri_ml[i_lata,]
df_refri_ml <- anti_join(df_refri_ml,df_refri_ks,by=c("IDNFE","DET_NITEM"))
df_refri_ml <- anti_join(df_refri_ml,df_refri_lata,by=c("IDNFE","DET_NITEM"))

df_refri_ks$PROD_EMBALAGEM_AJUSTADO <- "GARRAFA"
df_refri_lata$PROD_EMBALAGEM_AJUSTADO <- "LATA"
df_refri_pet$PROD_EMBALAGEM_AJUSTADO <- "PET"
df_refri_ml$PROD_EMBALAGEM_AJUSTADO <- "SEM AJUSTE"
df_refrigerante$PROD_EMBALAGEM_AJUSTADO <- "SEM AJUSTE"

df_refrigerante <- rbind(df_refri_ks,df_refri_lata,df_refri_ml,df_refri_pet,df_refrigerante)
rm(df_refri_ks,df_refri_lata,df_refri_ml,df_refri_pet,i_ks,i_lata)
gc(reset = T)

tb_refrigerante <- df_refrigerante%>%
  select(IDNFE,DET_NITEM,PROD_CPROD_SEFAZ_DETALHADO,PROD_XPROD_SEFAZ_DETALHADO,PROD_QSEFAZ_AJUSTADO
         ,PROD_USEFAZ_AJUSTADO,PROD_VUNSEFAZ_AJUSTADO,PROD_FATOR_MULTIPLICADOR,PROD_VOLUME_SEFAZ_AJUSTADO
         ,PROD_UNIDADE_MEDIDA_SEFAZ_AJUSTADO,PROD_EMBALAGEM_AJUSTADO)

fwrite(tb_refrigerante,"./DADOS/TB_REFRIGERANTE.csv",sep = ";")

rm(df_refrigerante,tb_refrigerante)
gc(reset = T)

######################## FIM AJUSTES FINAIS REFRIGERANTES ########################

