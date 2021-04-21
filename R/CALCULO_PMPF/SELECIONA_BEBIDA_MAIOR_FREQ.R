fn_maior_freq <- function(x) {
  df_bebidas_classificadas <- left_join(df_pmpf_bebidas,df_bebidas_CLASSIF,by="PROD_CEANTRIB")

  df_bebidas_grp <- df_bebidas_classificadas%>%
    group_by(PROD_CEANTRIB,PROD_CPROD_SEFAZ_AJUSTADO)%>%
    tally()%>%
    filter(PROD_CPROD_SEFAZ_AJUSTADO != is.na(PROD_CPROD_SEFAZ_AJUSTADO))

  #### FUNCAO ENCONTRAR MAIOR FREQUENCIA POR GTIN
  max_total <- function(x){
    return(max(x))
  }

  df_bebidas_grp <- rename(df_bebidas_grp,PROD_FREQ = 'n')
  df_bebidas_grp <- as.data.table(df_bebidas_grp)
  lista <- list()
  lista <- split(df_bebidas_grp,by="PROD_CEANTRIB")
  maior_freq <- lapply(lista, function(x) max_total(x$PROD_FREQ))
  df_bebida_maior_freq <- ldply(maior_freq,data.frame)
  df_bebida_maior_freq <-  rename(df_bebida_maior_freq,PROD_CEANTRIB = '.id',PROD_FREQ = 'X..i..')
  df_bebida_class <- inner_join(df_bebida_maior_freq,df_bebidas_grp,by=c("PROD_CEANTRIB","PROD_FREQ"))
  return(x)
}
