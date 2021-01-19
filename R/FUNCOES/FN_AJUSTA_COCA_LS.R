fn_cocacola <- function(x){
  y <- x%>%
    filter(CPROD_REFRIGERANTE_SEFAZ == 4000601 & VOLUME_SEFAZ == -1)

  i_ls <- grep("\\sls\\s",y$PROD_XPROD,ignore.case = T)
  x <- anti_join(x,y,by=c("IDNFE","DET_NITEM"))
  z <- y[i_ls,]
  y <- anti_join(y,z,by=c("IDNFE","DET_NITEM"))
  z$VOLUME_SEFAZ <- 1
  z$UN_MEDIDA_SEFAZ <- "L"
  x <- rbind(x,y,z)
  return(x)
}
