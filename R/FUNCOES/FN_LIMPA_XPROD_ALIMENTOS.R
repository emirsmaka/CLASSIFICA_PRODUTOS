fn_limpa_xprod_alim <- function(x){

  x$PROD_XPROD_LIMPO <- gsub("[[:digit:]]|[[:punct:]]"," ",x$PROD_XPROD)
  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "left")

  remove_word <- c("\\sx\\|\\<pct\\>|\\<kg\\>|\\srs\\s|\\s\\w\\s|\\<oc\\>|\\<cl\\>|\\<sh\\>")

  x$PROD_XPROD_LIMPO <- gsub(remove_word," ",x$PROD_XPROD_LIMPO,ignore.case = T)

  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "left")
  #x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "right")
  x$PROD_XPROD_LIMPO <- str_squish(x$PROD_XPROD_LIMPO)

  x$PROD_XPROD_LIMPO <- ifelse(x$PROD_XPROD_LIMPO == "","NULO",x$PROD_XPROD_LIMPO)
  x$PROD_XPROD_LIMPO <- tolower(x$PROD_XPROD_LIMPO)

  #x$PROD_XPROD_LIMPO <- as.factor(x$PROD_XPROD_LIMPO)
  #x$CPROD_CERVEJA_SEFAZ <- as.factor(x$CPROD_CERVEJA_SEFAZ)
  return(x)
}
