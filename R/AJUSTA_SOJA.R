###############################################################################
##  SCRIPT.....: AJUSTES PARA SOJA                                           ##
##  DATA.......: 21 julho 2020                                               ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  GESTOR.....: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

################# AJUSTE DAS COLUNAS PADRAO SEFAZ ##################

## AJUSTA COLUNAS E VALORES PARA UNIDADE = KG
id_kg <- grep("k(\\.)?g(\\.)?|kil[o|0](s)?",df_soja$PROD_UCOM,ignore.case = T)
df_kg <- df_soja[id_kg,]
#
########### UNIDADE == KG ###########  
df_kg$PROD_VUN_SEFAZ <- round(df_kg$PROD_VUNCOM, digits = 6)
df_kg$PROD_QTE_SEFAZ <- df_kg$PROD_QCOM
df_kg$PROD_UNID_SEFAZ <- "KG"
## INSERE UN == KG EM AJUSTADOS
df_soja_ajustado <- df_kg

### SEPARA UNIDADE KG ###
df_outros <- anti_join(df_soja,df_kg, by = c("IDNFE","DET_NITEM"))

############ FIM UNIDADE == KG ############

############ AJUSTES PARA UNIDADE == TON ############ 
id_ton <- grep("ton(\\s)?$|^to$|^t$|tn(\\s)?$", df_outros$PROD_UCOM,ignore.case = T)
df_ton <- df_outros[id_ton,]
## CALCULA VALORES COMPLEMENTARES UNIDADE = TONELADA
## Ajusta Colunas
df_ton$PROD_QTE_SEFAZ <- df_ton$PROD_QCOM*1000
df_ton$PROD_VUN_SEFAZ <- df_ton$PROD_VUNCOM/1000
df_ton$PROD_UNID_SEFAZ <- "KG"

## INSERE UN == TONELADAS EM AJUSTADOS
df_soja_ajustado <- rbind(df_ton,df_soja_ajustado)

############ FIM UNIDADE == TONELADA ############ 

#########################################

############ CALCULA VALORES SEFAZ EXTRAINDO PESO DE XPROD ############ 

## RETIRAR TONELADAS DE SOJA_OUTROS
df_outros <- anti_join(df_outros,df_ton, by = c("IDNFE","DET_NITEM"))


# ENCONTRAR PESO EM XPROD
id_peso <- grep("\\d\\d(.)?(\\d)?(\\s)?k",gsub(",",".",df_outros$PROD_XPROD),ignore.case = T)
df_outros$PROD_QTE_SEFAZ[id_peso] <- as.double(str_extract(str_extract(toupper(df_outros$PROD_XPROD[id_peso]),"\\d\\d(.)?(\\d)?(\\s)?K"),"\\d\\d")) * df_outros$PROD_QCOM[id_peso]
df_outros$PROD_VUN_SEFAZ[id_peso] <- ifelse(df_outros$PROD_QTE_SEFAZ[id_peso] == 0,0,df_outros$PROD_VPROD[id_peso] / df_outros$PROD_QTE_SEFAZ[id_peso])
df_soja_peso <- df_outros[id_peso,]

## INSERE PESO(XPROD) EM AJUSTADOS
df_soja_ajustado <- rbind(df_soja_ajustado,df_soja_peso)

## RETIRAR PESO(XPROD) DE MILHO_OUTROS
df_outros <- anti_join(df_outros,df_soja_peso, by = c("IDNFE","DET_NITEM"))

############ FIM CALCULO PESO(XPROD) ############ 

############ CALCULA VALORES SEFAZ UNIDADE = SACAS ############ 
## Ajusta Colunas
id_sc <- grep("sc(.)?[o]?$|^sa(c)?$|scs$|sac[a|o](s)?$|s(c)?(.)?(\\d{1,2})",df_outros$PROD_UCOM,ignore.case = T)
df_sc <- df_outros[id_sc,]
# Retira peso de UCOM
id_peso <- grep("\\d{2}",df_sc$PROD_UCOM)
df_sc$PROD_QTE_SEFAZ[id_peso] <- as.double(str_extract(df_sc$PROD_UCOM[id_peso],"\\d{2}")) * df_sc$PROD_QCOM[id_peso]

##############################################################################################
# Apos o ajuste com os pesos encotrados em XPROD observou-se que                             #
# restaram apenas PROD_UCOM com as siglas que, segundo a CEPLANFI indicam SACAS de 60KG      #
# conforme resultado da analise abaixo:                                                      #
#                                                                                            #
# > df_sc%>%                                                                                 #
#  + filter(PROD_QCOM != 0 & PROD_QTE_SEFAZ == 0)%>%                                         #
#  + group_by(PROD_UCOM)%>%                                                                  #
#  + summarise(TOTAL=n())%>%                                                                 #
#  + arrange(desc(TOTAL))                                                                    #
#                                                                                            #
#  PROD_UCOM   TOTAL                                                                         # 
#  <fct>       <int>                                                                         #
#  1 SC         8798                                                                         #
#  2 S60        2981                                                                         #
#  3 SCS          66                                                                         #  
#  4 sc           49                                                                         #
#  5 SACAS         9                                                                         #
#  6 SA            4                                                                         #
#  7 SACA          2                                                                         #
#  8 SC60          1                                                                         #
#  9 SCs           1                                                                         #
#  10 Sc           1                                                                         #
##############################################################################################

# Ajusta QTE-SEFAZ
df_sc$PROD_QTE_SEFAZ <- ifelse(df_sc$PROD_QCOM != 0 & df_sc$PROD_QTE_SEFAZ == 0,df_sc$PROD_QCOM*60,df_sc$PROD_QTE_SEFAZ)
# Ajusta VUN-SEFAZ
df_sc$PROD_VUN_SEFAZ <- ifelse(df_sc$PROD_QTE_SEFAZ == 0,0,df_sc$PROD_VPROD / df_sc$PROD_QTE_SEFAZ)
# Ajusta UNID_SEFAZ
df_sc$PROD_UNID_SEFAZ <- "KG"

# Ajusta Valores Unitarios com Infinito - Qando tem-se VPROD dividido por 0s em QTE-SEFAZ
#df_sc$PROD_VUN_SEFAZ <- ifelse(is.infinite(df_sc$PROD_VUNCOM_AJUSTADO) | is.na(df_sc$PROD_VUNCOM_AJUSTADO),0,df_sc$PROD_VUNCOM_AJUSTADO)

## INSERE SACAS EM AJUSTADOS
df_soja_ajustado <- rbind(df_soja_ajustado,df_sc)

## RETIRAR SACAS DE MILHO_OUTROS
df_outros <- anti_join(df_outros,df_sc, by = c("IDNFE","DET_NITEM"))

########### FIM UNIDADE == SACAS ########### 

#### LIMPA MEMORIA ####
rm(df_kg,df_soja_peso,df_sc,df_ton,df_outros,id_kg,id_peso,id_ton,id_sc)
gc(reset = TRUE)
#### FIM LIMPA MEMORIA ####


########### CALCULA MEDIA,MINIMO E MAXIMO ###########
df_1 <- df_soja_ajustado %>%
  filter(EMIT_CON_TIPO == 1)
df_2 <- df_soja_ajustado %>%
  filter(EMIT_CON_TIPO == 2)

id_out_1 <- fn_outlier(df_1$PROD_VUN_SEFAZ)
id_out_2 <- fn_outlier(df_2$PROD_VUN_SEFAZ)

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
df_soja_ajustado <- rbind(df_1,df_2)

########### FIM CALCULOS ########### 

#### LIMPA MEMORIA ####
rm(df_1,df_2,df_medias_1,df_medias_2,id_out_1,id_out_2)
gc(reset = TRUE)
#### FIM LIMPA MEMORIA ####

