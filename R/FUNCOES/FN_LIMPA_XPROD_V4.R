fn_limpa_xprod_2 <- function(x){

  x$PROD_XPROD_LIMPO <- gsub("[[:digit:]]|[[:punct:]]"," ",x$PROD_XPROD)
  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "left")

  remove_word <- c("\\<cx\\>|\\<cartao\\>|\\<six pack\\>|\\<sixpack\\>|\\<cxa\\>|\\<ssh\\>|\\<gfa\\>|\\<l\\>|\\<lt\\>|\\<vd\\>|\\<sx\\>|\\<ln\\>|\\<npal\\>|\\<un\\>|\\<und\\>|\\<unid\\>|
  \\<com\\>|\\<ttc\\>|\\<pc\\>|\\<pct\\>|\\<lata\\>|\\<latas\\>|\\<latao\\>|\\<pack\\>|\\<alcool\\>|\\<sleek\\>|\\<com\\>|\\<p\\>|\\<lta\\>|\\<fi\\>|\\<f\\>|
  \\<pe sec rd\\>|\\<prec\\>|\\<ret\\>|\\<pbr\\>|\\<gfs\\>|\\<ow\\>|\\<d\\>|\\<fora de linha\\>|\\<garrfa\\>|\\<garrafa\\>|\\<long neck\\>|\\<caixa\\>|\\<cart\\>|
  \\<pap\\>|\\<fridge\\>|\\<a\\>|\\<ml\\>|\\<six\\>|\\<x\\>")

  x$PROD_XPROD_LIMPO <- gsub(remove_word," ",x$PROD_XPROD_LIMPO,ignore.case = T)

  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "left")
  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "right")
  x$PROD_XPROD_LIMPO <- str_squish(x$PROD_XPROD_LIMPO)

  x$PROD_XPROD_LIMPO <- ifelse(x$PROD_XPROD_LIMPO == "","NULO",x$PROD_XPROD_LIMPO)
  x$PROD_XPROD_LIMPO <- tolower(x$PROD_XPROD_LIMPO)

  #x$PROD_XPROD_LIMPO <- as.factor(x$PROD_XPROD_LIMPO)
  #x$CPROD_CERVEJA_SEFAZ <- as.factor(x$CPROD_CERVEJA_SEFAZ)
  return(x)
}
