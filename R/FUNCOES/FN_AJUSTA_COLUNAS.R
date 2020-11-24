fn_ajusta_colunas <- function(x,p1,p2){
  ###### AJUSTA COLUNAS ######
  x$QTE_TRIB_AJUSTADO <- str_extract(str_extract(tolower(x$PROD_XPROD),p1),p2)
  x$FATOR_MULTIPLICADOR <- as.numeric(x$QTE_TRIB_AJUSTADO)
  x$QTE_SEFAZ <- x$FATOR_MULTIPLICADOR * x$PROD_QCOM
  return(x)
}
