############################################################################################
##  SCRIPT.....: AJUSTES PARA CERVEJAS                                                    ##
##  DETALHE....: AJUSTA VALOR UNITARIO, QUANTIDADE E FATOR MULTIPLICADOR - COMPLEMENTO    ##
##  DATA.......: 15 setembro 2020                                                         ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                                          ##
##  ANALISTA...: EMIR MANSUR SMAKA                                                        ##
##  SETOR......: COTIN/UGDT                                                               ##
##  RESPONSAVEL: GERSON LUIZ DOS SANTOS                                                   ##
############################################################################################

################ PADROES ESPECIFICOS EM PROD_UCOM ################
id_cx <- grep("cx",df_cerveja$PROD_UCOM,ignore.case = T)
df_caixa <- df_cerveja[id_cx,]
df_cerveja <- setdiff(df_cerveja,df_caixa)

i_cx1 <- grep("(\\s|\\/|x)6$",df_caixa$PROD_XPROD,ignore.case = T) # CAIXA COM 6 UN
i_cx2 <-grep("(\\*\\d{1,2}|\\d{1,2}\\*)$",df_caixa$PROD_XPROD) # CAIXAS COM 6,15,18,24 UN
i_cx3 <-grep("(leve|lv)\\d{1,2}",df_caixa$PROD_XPROD,ignore.case = T) # CAIXAS COM PROMOCAO LV X PG Y

df_caixa$QTE_TRIB_AJUSTADO[i_cx1] <- str_extract(tolower(df_caixa$PROD_XPROD[i_cx1]),"(\\s|\\/|x)6$")
df_caixa$FATOR_MULTIPLICADOR[i_cx1] <- str_extract(df_caixa$QTE_TRIB_AJUSTADO[i_cx1],"\\d")
df_caixa$QTE_TRIB_AJUSTADO[i_cx1] <- as.integer(df_caixa$FATOR_MULTIPLICADOR[i_cx1]) * df_caixa$PROD_QCOM[i_cx1]

df_caixa$QTE_TRIB_AJUSTADO[i_cx2] <- str_extract(df_caixa$PROD_XPROD[i_cx2],"(\\*\\d{1,2}|\\d{1,2}\\*)$")
df_caixa$FATOR_MULTIPLICADOR[i_cx2] <- str_extract(df_caixa$QTE_TRIB_AJUSTADO[i_cx2],"\\d{1,2}")
df_caixa$QTE_TRIB_AJUSTADO[i_cx2] <- as.integer(df_caixa$FATOR_MULTIPLICADOR[i_cx2]) * df_caixa$PROD_QCOM[i_cx2]

df_caixa$QTE_TRIB_AJUSTADO[i_cx3] <- str_extract(tolower(df_caixa$PROD_XPROD[i_cx3]),"(leve|lv)\\d{1,2}")
df_caixa$FATOR_MULTIPLICADOR[i_cx3] <- str_extract(df_caixa$QTE_TRIB_AJUSTADO[i_cx3],"\\d{1,2}")
df_caixa$QTE_TRIB_AJUSTADO[i_cx3] <- as.integer(df_caixa$FATOR_MULTIPLICADOR[i_cx3]) * df_caixa$PROD_QCOM[i_cx3]

df_cx_sem_ajuste <- df_caixa%>%
  filter(is.na(FATOR_MULTIPLICADOR))

df_cx_ajuste <- setdiff(df_caixa,df_cx_sem_ajuste)
df_cx_ajuste$QTE_TRIB_AJUSTADO <- ifelse(df_cx_ajuste$FATOR_MULTIPLICADOR >24,NA,df_cx_ajuste$QTE_TRIB_AJUSTADO)
df_cx_ajuste$FATOR_MULTIPLICADOR <- ifelse(df_cx_ajuste$FATOR_MULTIPLICADOR >24,NA,df_cx_ajuste$FATOR_MULTIPLICADOR)


df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cx_ajuste)
df_cerveja <- rbind(df_cerveja,df_cx_sem_ajuste)

rm(df_cx_ajuste,df_cx_sem_ajuste,df_caixa, i_cx1,i_cx2,i_cx3,id_cx)
gc(reset = T)
###########################################




## PADROES COM A QTE EMBUTIDA EM PROD_UCOM
id_gen <- grep("am12|12cx|am24|am3|am6|am8|bd-12|eb12|fd\\d{1,2}",df_cerveja$PROD_UCOM,ignore.case = T)
df_gen <- df_cerveja[id_gen,]
df_cerveja <- setdiff(df_cerveja,df_gen)

df_gen$FATOR_MULTIPLICADOR <- as.integer(str_extract(tolower(df_gen$PROD_UCOM),"\\d{1,4}"))
df_gen$QTE_TRIB_AJUSTADO <- df_gen$FATOR_MULTIPLICADOR * df_gen$PROD_QCOM
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_gen)
rm(df_gen,id_gen)
gc(reset = T)
###########################################

## PADROES QTE UNIDADES EM CAIXAS DENTRO DE PROD_UCOM
id_cx <- grep("cx(a|[[:punct:]])?\\d{1,2}",df_cerveja$PROD_UCOM,ignore.case = T)
df_cx <- df_cerveja[id_cx,]
df_cerveja <- setdiff(df_cerveja,df_cx)

df_cx$FATOR_MULTIPLICADOR <- df_cx$PROD_QTRIB / df_cx$PROD_QCOM
df_cx$QTE_TRIB_AJUSTADO <- df_cx$PROD_QTRIB
df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_cx)
rm(df_cx,id_cx)
gc(reset = T)
###########################################

id_eb <- grep("^eb$",df_cerveja$PROD_UCOM,ignore.case = T)
df_eb <- df_cerveja[id_eb,]
df_cerveja <- setdiff(df_cerveja,df_eb)

i_eb1 <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_eb$PROD_XPROD,ignore.case = T)
i_eb2 <- grep("eb\\d{1,2}",df_eb$PROD_XPROD,ignore.case = T)
i_eb3 <- grep("\\d{1,2}\\s?un",df_eb$PROD_XPROD,ignore.case = T)

df_eb$QTE_TRIB_AJUSTADO[i_eb1] <- str_extract(tolower(df_eb$PROD_XPROD[i_eb1]),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- as.integer(str_extract(str_extract(tolower(df_eb$QTE_TRIB_AJUSTADO[i_eb1]),"\\d{1,2}\\s?x") ,"\\d{1,2}"))
p2 <- as.integer(str_extract(str_extract(tolower(df_eb$QTE_TRIB_AJUSTADO[i_eb1]),"x\\s?\\d{1,2}") ,"\\d{1,2}"))
df_eb$FATOR_MULTIPLICADOR[i_eb1] <- ifelse(p1 > p2,p1,p2)
df_eb$QTE_TRIB_AJUSTADO[i_eb1] <- as.integer(df_eb$FATOR_MULTIPLICADOR[i_eb1]) * df_eb$PROD_QCOM[i_eb1]

df_eb$QTE_TRIB_AJUSTADO[i_eb2] <- str_extract(tolower(df_eb$PROD_XPROD[i_eb2]),"eb\\d{1,2}")
df_eb$FATOR_MULTIPLICADOR[i_eb2] <- str_extract(df_eb$QTE_TRIB_AJUSTADO[i_eb2],"\\d{1,2}")
df_eb$QTE_TRIB_AJUSTADO[i_eb2] <- as.integer(df_eb$FATOR_MULTIPLICADOR[i_eb2]) * df_eb$PROD_QCOM[i_eb2]

df_eb$QTE_TRIB_AJUSTADO[i_eb3] <- str_extract(tolower(df_eb$PROD_XPROD[i_eb3]),"\\d{1,2}\\s?un")
df_eb$FATOR_MULTIPLICADOR[i_eb3] <- str_extract(df_eb$QTE_TRIB_AJUSTADO[i_eb3],"\\d{1,2}")
df_eb$QTE_TRIB_AJUSTADO[i_eb3] <- as.integer(df_eb$FATOR_MULTIPLICADOR[i_eb3]) * df_eb$PROD_QCOM[i_eb3]

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_eb)
rm(df_eb,i_eb1,i_eb2,i_eb3,id_eb,p1,p2)
gc(reset = T)
###########################################

id_emb <- grep("^emb$",df_cerveja$PROD_UCOM,ignore.case = T)
df_emb <- df_cerveja[id_emb,]
df_cerveja <- setdiff(df_cerveja,df_emb)

i_emb <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_emb$PROD_XPROD,ignore.case = T)
df_emb$QTE_TRIB_AJUSTADO[i_emb] <- str_extract(tolower(df_emb$PROD_XPROD[i_emb]),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- as.integer(str_extract(str_extract(tolower(df_emb$QTE_TRIB_AJUSTADO[i_emb]),"\\d{1,2}\\s?x") ,"\\d{1,2}"))
p2 <- as.integer(str_extract(str_extract(tolower(df_emb$QTE_TRIB_AJUSTADO[i_emb]),"x\\s?\\d{1,2}") ,"\\d{1,2}"))
df_emb$FATOR_MULTIPLICADOR[i_emb] <- ifelse(p1 > p2,p1,p2)
df_emb$QTE_TRIB_AJUSTADO[i_emb] <- as.integer(df_emb$FATOR_MULTIPLICADOR[i_emb]) * df_emb$PROD_QCOM[i_emb]

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_emb)
rm(df_emb,i_emb,id_emb,p1,p2)
gc(reset = T)
###########################################

id_fardo <- grep("^fardo$",df_cerveja$PROD_UCOM,ignore.case = T)
df_fardo <- df_cerveja[id_fardo,]
df_cerveja <- setdiff(df_cerveja,df_fardo)

df_fardo$QTE_TRIB_AJUSTADO <- str_extract(df_fardo$PROD_XPROD,"\\*\\s?\\d{1,2}")
df_fardo$FATOR_MULTIPLICADOR <- str_extract(df_fardo$QTE_TRIB_AJUSTADO,"\\d{1,2}")
df_fardo$QTE_TRIB_AJUSTADO <- as.integer(df_fardo$FATOR_MULTIPLICADOR) * df_fardo$PROD_QCOM

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_fardo)
rm(df_fardo,id_fardo)
###########################################

id_fd <- grep("^fd$",df_cerveja$PROD_UCOM,ignore.case = T)
df_fd <- df_cerveja[id_fd,]
df_cerveja <- setdiff(df_cerveja,df_fd)

i_fd1 <- grep("\\d{1,2}\\s?x\\s?\\d{1,2}",df_fd$PROD_XPROD,ignore.case = T)
i_fd2 <- grep("fd\\s\\d{1,2}",df_fd$PROD_XPROD,ignore.case = T)

df_fd$QTE_TRIB_AJUSTADO[i_fd1] <- str_extract(tolower(df_fd$PROD_XPROD[i_fd1]),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- as.integer(str_extract(str_extract(tolower(df_fd$QTE_TRIB_AJUSTADO[i_fd1]),"\\d{1,2}\\s?x") ,"\\d{1,2}"))
p2 <- as.integer(str_extract(str_extract(tolower(df_fd$QTE_TRIB_AJUSTADO[i_fd1]),"x\\s?\\d{1,2}") ,"\\d{1,2}"))
df_fd$FATOR_MULTIPLICADOR[i_fd1] <- ifelse(p1 > p2,p1,p2)
df_fd$QTE_TRIB_AJUSTADO[i_fd1] <- as.integer(df_fd$FATOR_MULTIPLICADOR[i_fd1]) * df_fd$PROD_QCOM[i_fd1]

df_fd$QTE_TRIB_AJUSTADO[i_fd2] <- str_extract(tolower(df_fd$PROD_XPROD[i_fd2]),"fd\\s\\d{1,2}")
df_fd$FATOR_MULTIPLICADOR[i_fd2] <- str_extract(df_fd$QTE_TRIB_AJUSTADO[i_fd2],"\\d{1,2}")
df_fd$QTE_TRIB_AJUSTADO[i_fd2] <- as.integer(df_fd$FATOR_MULTIPLICADOR[i_fd2]) * df_fd$PROD_QCOM[i_fd2]

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_fd)
rm(df_fd,id_fd,i_fd1,i_fd2,p1,p2)
gc(reset = T)
###########################################

id_pac <- grep("^pac$",df_cerveja$PROD_UCOM,ignore.case = T)
df_pac <- df_cerveja[id_pac,]
df_cerveja <- setdiff(df_cerveja,df_pac)

i_pac1 <- grep("\\d{1,2}\\s?(p|un|pack)",df_pac$PROD_XPROD,ignore.case = T)
df_pac1 <- df_pac[i_pac1,]
df_pac1$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_pac1$PROD_XPROD),"\\d{1,2}\\s?(p|un|pack)")
df_pac1$FATOR_MULTIPLICADOR <- str_extract(df_pac1$QTE_TRIB_AJUSTADO,"\\d{1,2}")
df_pac1$QTE_TRIB_AJUSTADO <- as.integer(df_pac1$FATOR_MULTIPLICADOR) * df_pac1$PROD_QCOM

df_pac <- df_pac%>%
  filter(is.na(FATOR_MULTIPLICADOR))

i_pac2 <- grep("(pac|pct|com)([[:punct:]]|\\s)?\\d{1,2}",df_pac$PROD_XPROD,ignore.case = T)
df_pac$QTE_TRIB_AJUSTADO[i_pac2] <- str_extract(tolower(df_pac$PROD_XPROD[i_pac2]),"(pac|pct|com)([[:punct:]]|\\s)?\\d{1,2}")
df_pac$FATOR_MULTIPLICADOR[i_pac2] <- str_extract(df_pac$QTE_TRIB_AJUSTADO[i_pac2],"\\d{1,2}")
df_pac$QTE_TRIB_AJUSTADO[i_pac2] <- as.integer(df_pac$FATOR_MULTIPLICADOR[i_pac2]) * df_pac$PROD_QCOM[i_pac2]

df_pac <- rbind(df_pac,df_pac1)
df_cerveja_ajustada <-rbind(df_cerveja_ajustada,df_pac)
rm(df_pac,df_pac1,id_pac,i_pac1,i_pac2)
gc(reset = T)
###########################################

id_fdo <- grep("^fdo$",df_cerveja$PROD_UCOM,ignore.case = T)
df_fdo <- df_cerveja[id_fdo,]
df_cerveja <- setdiff(df_cerveja,df_fdo)

df_fdo$QTE_TRIB_AJUSTADO <- str_extract(tolower(df_fdo$PROD_XPROD),"\\d{1,2}\\s?x\\s?\\d{1,2}")
p1 <- as.integer(str_extract(str_extract(tolower(df_fdo$QTE_TRIB_AJUSTADO),"\\d{1,2}\\s?x") ,"\\d{1,2}"))
p2 <- as.integer(str_extract(str_extract(tolower(df_fdo$QTE_TRIB_AJUSTADO),"x\\s?\\d{1,2}") ,"\\d{1,2}"))
df_fdo$FATOR_MULTIPLICADOR <- ifelse(p1 > p2,p1,p2)
df_fdo$QTE_TRIB_AJUSTADO <- as.integer(df_fdo$FATOR_MULTIPLICADOR) * df_fdo$PROD_QCOM

df_cerveja_ajustada <- rbind(df_cerveja_ajustada,df_fdo)
rm(df_fdo,id_fdo,p1,p2)
gc(reset = T)
###########################################

############# AJUSTA COLUNAS DEFINITIVAS #############
df_cerveja_ajustada$QTE_SEFAZ <- as.double(df_cerveja_ajustada$QTE_TRIB_AJUSTADO)
df_cerveja_ajustada$FATOR_MULTIPLICADOR <- as.integer(df_cerveja_ajustada$FATOR_MULTIPLICADOR)
df_cerveja_ajustada$VLR_UNITARIO_SEFAZ <- as.double(df_cerveja_ajustada$PROD_VPROD / df_cerveja_ajustada$FATOR_MULTIPLICADOR)

## UNIFICA REGISTROS NAO AJUSTADOS EM UMA UNICA TABELA
df_cerveja <- rbind(df_cerveja,df_cerveja_un_outlier)

## ATRIBUI VALOR "-1" PARA AS COLUNAS DOS REGISTROS NAO AJUSTADOS
df_cerveja$QTE_SEFAZ <- -1
df_cerveja$FATOR_MULTIPLICADOR <- -1
df_cerveja$VLR_UNITARIO_SEFAZ <- -1


## UNIFICA TODOS OS REGISTROS (AJUSTADOS E NAO AJUSTADOS) EM UMA UNICA TABELA
df_cerveja <- rbind(df_cerveja,df_cerveja_ajustada)

## APAGA AS TABELAS USADAS COMO APOIO E LIBERA MEMORIA
rm(df_cerveja_ajustada,df_cerveja_un_outlier)
gc(reset = T)
###########################################################

source("./AJUSTES_VOLUME_UNID_MED_CERVEJA.R")

