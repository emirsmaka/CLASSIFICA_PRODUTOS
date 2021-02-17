###############################################################################
##  SCRIPT.....: AJUSTES PARA MILHO                                          ##
##  DATA.......: 13 julho 2020                                               ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  GESTOR.....: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

################# AJUSTE DAS COLUNAS PADRAO SEFAZ ##################

## AJUSTA COLUNAS E VALORES PARA UNIDADE = KG
id_kg <- grep("k(\\.)?g(\\.)?|kil[o|0](s)?",df_milho$PROD_UCOM,ignore.case = T)
df_kg <- df_milho[id_kg,]
#
########### UNIDADE == KG ###########
df_kg$VLR_UNITARIO_SEFAZ <- round(df_kg$PROD_VUNCOM, digits = 6)
df_kg$QTE_SEFAZ <- df_kg$PROD_QCOM
df_kg$UNIDADE_SEFAZ <- "KG"
## INSERE UN == KG EM AJUSTADOS
df_milho_ajustado <- df_kg

### SEPARA UNIDADE KG ###
df_outros <- anti_join(df_milho,df_kg, by = c("IDNFE","DET_NITEM"))

############ FIM UNIDADE == KG ############

############ AJUSTES PARA UNIDADE == TON ############
id_ton <- grep("ton(\\s)?$|^to$|^t$|tn(\\s)?$", df_outros$PROD_UCOM,ignore.case = T)
df_ton <- df_outros[id_ton,]
## CALCULA VALORES COMPLEMENTARES UNIDADE = TONELADA
## Ajusta Colunas
df_ton$QTE_SEFAZ <- df_ton$PROD_QCOM*1000
df_ton$VLR_UNITARIO_SEFAZ <- df_ton$PROD_VUNCOM/1000
df_ton$UNIDADE_SEFAZ <- "KG"

## INSERE UN == TONELADAS EM AJUSTADOS
df_milho_ajustado <- rbind(df_ton,df_milho_ajustado)

############ FIM UNIDADE == TONELADA ############

#########################################

############ CALCULA VALORES SEFAZ EXTRAINDO PESO DE XPROD ############

## RETIRAR TONELADAS DE MILHO_OUTROS
df_outros <- anti_join(df_outros,df_ton, by = c("IDNFE","DET_NITEM"))


# ENCONTRAR PESO EM XPROD
id_peso <- grep("\\d\\d(.)?(\\d)?(\\s)?k",gsub(",",".",df_outros$PROD_XPROD),ignore.case = T)
df_outros$QTE_SEFAZ[id_peso] <- as.double(str_extract(str_extract(toupper(df_outros$PROD_XPROD[id_peso]),"\\d\\d(.)?(\\d)?(\\s)?K"),"\\d\\d")) * df_outros$PROD_QCOM[id_peso]
df_outros$VLR_UNITARIO_SEFAZ[id_peso] <- ifelse(df_outros$QTE_SEFAZ[id_peso] == 0,0,df_outros$PROD_VPROD[id_peso] / df_outros$QTE_SEFAZ[id_peso])
df_milho_peso <- df_outros[id_peso,]

## INSERE PESO(XPROD) EM AJUSTADOS
df_milho_ajustado <- rbind(df_milho_ajustado,df_milho_peso)

## RETIRAR PESO(XPROD) DE MILHO_OUTROS
df_outros <- anti_join(df_outros,df_milho_peso, by = c("IDNFE","DET_NITEM"))

############ FIM CALCULO PESO(XPROD) ############

############ CALCULA VALORES COMPLEMENTARES UNIDADE = SACAS ############
## Ajusta Colunas
id_sc <- grep("sc(.)?[o]?$|^sa(c)?$|scs$|sac[a|o](s)?$|s(c)?(.)?(\\d{1,2})",df_outros$PROD_UCOM,ignore.case = T)
df_sc <- df_outros[id_sc,]
# Retira peso de UCOM
id_peso <- grep("\\d{2}",df_sc$PROD_UCOM)
df_sc$QTE_SEFAZ[id_peso] <- as.double(str_extract(df_sc$PROD_UCOM[id_peso],"\\d{2}")) * df_sc$PROD_QCOM[id_peso]

##############################################################################################
# Apos o ajuste com os pesos encontrados em XPROD observa-se que restaram apenas             #
# PROD_UCOM com as siglas que, segundo a CEPLANFI indicam SACAS de 60KG conforme resultado   #
# da analise abaixo:                                                                         #
#                                                                                            #
# > df_sc%>%                                                                                 #
#  + filter(PROD_QCOM != 0 & QTE_SEFAZ == 0)%>%                                              #
#  + group_by(PROD_UCOM)%>%                                                                  #
#  + summarise(TOTAL=n())%>%                                                                 #
#  + arrange(desc(TOTAL))                                                                    #
#                                                                                            #
#  PROD_UCOM   TOTAL                                                                         #
#  <fct>       <int>                                                                         #
#  1 SC        33761                                                                         #
#  2 sc         2184                                                                         #
#  3 SC1         788                                                                         #
#  4 SCS         249                                                                         #
#  5 Sc           90                                                                         #
#  6 SACO         22                                                                         #
#  7 Scs           9                                                                         #
#  8 SACA          6                                                                         #
#  9 SACAS         6                                                                         #
#  10 SC.          5                                                                         #
#  11 SAC          2                                                                         #
#  12 SC 1         2                                                                         #
#  13 Sacas        2                                                                         #
#  14 SA           1                                                                         #
#  15 Saca         1                                                                         #
##############################################################################################

# Ajusta QTE-SEFAZ
df_sc$QTE_SEFAZ <- ifelse(df_sc$PROD_QCOM != 0 & df_sc$QTE_SEFAZ == 0,df_sc$PROD_QCOM*60,df_sc$QTE_SEFAZ)
# Ajusta VUN-SEFAZ
df_sc$VLR_UNITARIO_SEFAZ <- ifelse(df_sc$QTE_SEFAZ == 0,0,df_sc$PROD_VPROD / df_sc$QTE_SEFAZ)
# Ajusta UNID_SEFAZ
df_sc$UNIDADE_SEFAZ <- "KG"

# Ajusta Valores Unitarios com Infinito - Qando tem-se VPROD dividido por 0s em QTE-SEFAZ
#df_sc$VLR_UNITARIO_SEFAZ <- ifelse(is.infinite(df_sc$PROD_VUNCOM_AJUSTADO) | is.na(df_sc$PROD_VUNCOM_AJUSTADO),0,df_sc$PROD_VUNCOM_AJUSTADO)

## INSERE SACAS EM AJUSTADOS
df_milho_ajustado <- rbind(df_milho_ajustado,df_sc)

## RETIRAR SACAS DE MILHO_OUTROS
df_outros <- anti_join(df_outros,df_sc, by = c("IDNFE","DET_NITEM"))

########### FIM UNIDADE == SACAS ###########

#### LIMPA MEMORIA ####
rm(df_kg,df_milho_peso,df_sc,df_ton,id_kg,id_peso,id_ton,id_sc)
gc(reset = TRUE)
#### FIM LIMPA MEMORIA ####


########### CALCULA MEDIA,MINIMO E MAXIMO ###########
df_1 <- df_milho_ajustado %>%
  filter(EMIT_CON_TIPO == 1)
df_2 <- df_milho_ajustado %>%
  filter(EMIT_CON_TIPO == 2)

id_out_1 <- fn_outlier(df_1$VLR_UNITARIO_SEFAZ)
id_out_2 <- fn_outlier(df_2$VLR_UNITARIO_SEFAZ)

## CALCULA PRECO MEDIO
df_medias_1 <- fn_sve_med(setdiff(df_1,df_1[id_out_1]))
#df_y_out <- anti_join(df_y,df_y[id_out_2])
df_medias_2 <- fn_sve_med(setdiff(df_2,df_2[id_out_2]))

## RECRIA TABELA UCOM = KG RETORNANDO OUTLIERS
df_1 <- fn_ajusta_outliers(df_medias_1,df_1)
df_2 <- fn_ajusta_outliers(df_medias_2,df_2)

## CALCULA PRECOS MIN E MAX
df_1 <- fn_sve_ext(df_1)
df_2 <- fn_sve_ext(df_2)
df_milho_kg_ajustado <- rbind(df_1,df_2)

########### FIM CALCULOS ###########

#### LIMPA MEMORIA ####
rm(df_1,df_2,df_medias_1,df_medias_2,id_out_1,id_out_2)
gc(reset = TRUE)
#### FIM LIMPA MEMORIA ####

