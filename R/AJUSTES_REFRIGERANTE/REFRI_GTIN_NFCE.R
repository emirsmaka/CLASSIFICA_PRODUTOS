
NZ_TribRef <- fn_conect_NZ_2(NZ_TribRef)


df_refri_NFE_NFCE <- dbGetQuery(NZ_TribRef,"SELECT DISTINCT
 T1.DEST_CNPJ_CPF_IDESTRANGEIRO
,T0.PROD_XPROD
,T0.PROD_XPROD_SEFAZ
,T1.PROD_CEANTRIB AS CEANTRIB_NFE
,T2.PROD_CEANTRIB AS CEANTRIB_NFCE
,T2.PROD_XPROD  AS PROD_XPROD_NFCE
,T0.FATOR_MULTIPLICADOR
,T0.VOLUME_SEFAZ
,T0.UN_MEDIDA_SEFAZ
,T0.UNIDADE_SEFAZ
,T0.QTE_SEFAZ
,T0.VLR_UNITARIO_SEFAZ
,T0.PROD_VPROD
,T2.PROD_QCOM AS PROD_QCOM_NFCE
,T2.PROD_VUNCOM AS PROD_VUNCOM_NFCE
FROM MINING_SEFAZ.ADMIN.TB_REFRIGERANTE T0
INNER JOIN TRIBUTARIO_REFERENCIA.ADMIN.NFE T1
ON T0.IDNFE = T1.IDNFE AND T0.DET_NITEM = T1.DET_NITEM
INNER JOIN TRIBUTARIO_REFERENCIA.ADMIN.SPK_NFCE_ITEM T2
ON T1.PROD_CEANTRIB = T2.PROD_CEANTRIB AND T1.DEST_CNPJ_CPF_IDESTRANGEIRO = T2.EMIT_CNPJ_CPF AND T1.PROD_NCM_POSICAO = SUBSTRING(T2.PROD_NCM,1,4)
WHERE /*T1.DEST_CNPJ_CPF_IDESTRANGEIRO IN ('62545579002683',
                        '75315333000370',
                        '09477652001915',
                        '09477652001168',
                        '07170943008277') AND */
       T1.PROD_NCM_POSICAO = '2202'  AND
       T1.PROD_CEANTRIB <> 'SEM GTIN' AND T1.PROD_CEANTRIB <> 'NULO' AND
       T0.IDE_DHEMI_PERIODO BETWEEN 202001 AND 202012")

df_refri_NFE_NFCE$CEANTRIB_NFE <- as.factor(df_refri_NFE_NFCE$CEANTRIB_NFE)
df_refri_NFE_NFCE$CEANTRIB_NFCE <- as.factor(df_refri_NFE_NFCE$CEANTRIB_NFCE)

### AGRUPAR POR PROD_XPROD, XPROD_SEFAZ, CEANTRIB, VOLUME_SEFAZ
tb_refri_gtin <- df_refri_NFE_NFCE%>%
  filter(PROD_XPROD_SEFAZ != 'DEMAIS REFRIGERANTES' & PROD_XPROD_SEFAZ != 'SEM CLASSIFICACAO'
         & VOLUME_SEFAZ != -1)%>%
  group_by(PROD_XPROD, PROD_XPROD_SEFAZ,CEANTRIB_NFCE,VOLUME_SEFAZ)%>%
  summarise(TOTAL = n())%>%
  #distinct(CEANTRIB_NFCE,max(TOTAL),keep_all = TRUE)%>%
  arrange(desc(TOTAL))

#### FUNCAO ENCONTRAR MAIOR FREQUENCIA POR GTIN
max_total <- function(x){
  return(max(x))
}
lista_refri <- list()
lista_refri <- split(x=tb_refri_gtin,f=tb_refri_gtin$CEANTRIB_NFCE)
maior_freq <- lapply(lista_refri, function(x) max_total(x$TOTAL))
refri_maior_freq <- ldply(maior_freq,data.frame)
refri_maior_freq <-  rename(refri_maior_freq,CEANTRIB_NFCE = '.id',TOTAL = 'X..i..')

tb_refri_gtin <- inner_join(refri_maior_freq,tb_refri_gtin,by=c("CEANTRIB_NFCE","TOTAL"))


tb_pmpf <- read.xlsx("./DADOS/PMPF_BEBIDAS.xlsx")
tb_pmpf <- rename(tb_pmpf,CEANTRIB_NFCE = 'CODIGO.EAN.TRIB')
tb_pmpf <- rename(tb_pmpf,PROD_XPROD_PMPF = 'DESCRICAO.DO.PRODUTO')
tb_pmpf <- rename(tb_pmpf,VOLUME_PMPF = 'VOLUME')
tb_pmpf <- rename(tb_pmpf,EMBALAGEM_PMPF = 'EMBALAGEM')

df_pmpf <- tb_pmpf%>%
  select(CEANTRIB_NFCE,PROD_XPROD_PMPF,VOLUME_PMPF,EMBALAGEM_PMPF)
df_pmpf$CEANTRIB_PMPF <- df_pmpf$CEANTRIB_NFCE

tb_refri_nfce_pmpf <- inner_join(tb_refri_gtin,df_pmpf, by = "CEANTRIB_NFCE")
tb_refri_gtin <- anti_join(tb_refri_gtin,tb_refri_nfce_pmpf,by="CEANTRIB_NFCE")

rm(lista_refri,maior_freq,refri_maior_freq)



