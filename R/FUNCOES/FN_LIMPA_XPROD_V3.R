fn_limpa_xprod <- function(x){

  x$PROD_XPROD_LIMPO <- gsub("[[:digit:]]|[[:punct:]]"," ",x$PROD_XPROD)
  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "left")

  remove_words <- c("cx|ml|cartao|six\\s?pack|cxa|\\ssh\\s|l?gfa|\\sl\\s|\\slt(\\s)?|vd|\\sx\\s|ln|\\s(c)\\s|npal|un(id)?|\\scom\\s|ttc|pct?|\\sc\\s")
  remove_words2 <- c("lata(s|o)?|pack|alcool|\\sc\\s|sleek|\\scom\\s|\\sp\\s")
  remove_words3 <- c("\\scom(\\s)?|\\sfi(\\s)?|\\sf\\s|pe\\sec\\srd|\\sprec\\s|\\sret\\s|\\spbr(\\s)?|\\su\\s|\\sgfs(\\s)?|^i\\s|\\sn$|\\sow|\\sd(\\s|$)|fora de linha|
                   garrfa|garrafa|long neck|caixa|cart\\s|\\spap|fridge")
  x$PROD_XPROD_LIMPO2 <- gsub(remove_words," ",x$PROD_XPROD_LIMPO,ignore.case = T)
  x$PROD_XPROD_LIMPO3 <- gsub(remove_words2," ",x$PROD_XPROD_LIMPO2,ignore.case = T)
  x$PROD_XPROD_LIMPO <- NULL
  x$PROD_XPROD_LIMPO2 <- NULL

  x$PROD_XPROD_LIMPO4 <- gsub(remove_words3," ",x$PROD_XPROD_LIMPO3,ignore.case = T)
  x$PROD_XPROD_LIMPO3 <- NULL
  x <- rename(x,PROD_XPROD_LIMPO = PROD_XPROD_LIMPO4)

  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "left")
  x$PROD_XPROD_LIMPO <- str_trim(x$PROD_XPROD_LIMPO,side = "right")
  x$PROD_XPROD_LIMPO <- str_squish(x$PROD_XPROD_LIMPO)

  x$PROD_XPROD_LIMPO <- ifelse(x$PROD_XPROD_LIMPO == "","NULO",x$PROD_XPROD_LIMPO)
  x$PROD_XPROD_LIMPO <- tolower(x$PROD_XPROD_LIMPO)

  x$PROD_XPROD_LIMPO <- as.factor(x$PROD_XPROD_LIMPO)
  x$CPROD_CERVEJA_SEFAZ <- as.factor(x$CPROD_CERVEJA_SEFAZ)
  return(x)
}
