###############################################################################
##  SCRIPT.....: FUNCOES DE APOIO                                            ##
##  DATA.......: 09 julho 2020                                               ##
##  PROJETO....: CLASSIFICACAO DE PRODUTOS SEFAZ                             ##
##  ANALISTA...: EMIR MANSUR SMAKA                                           ##
##  SETOR......: COTIN/UGDT                                                  ##
##  GESTOR.....: GERSON LUIZ DOS SANTOS                                      ##
###############################################################################

## FUNCAO OUTLIERS
fn_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
### FIM FUNCAO OUTLIERS ###


############# FUNCAO MEDIAS  #############
fn_calc_media <- function(df_table){
  df_table%>%
    mutate(VLR_MEDIO_SEFAZ = mean(PROD_VUN_SEFAZ))
}
############# FIM MEDIAS #############



## FUNCAO VLRS EXTREMOS
fn_calc_min_max <- function(df_table,periodo){
  df_table %>%
  filter(IDE_DHEMI_PERIODO == periodo)%>%
  mutate(VLR_MIN_SEFAZ = min(PROD_VUN_SEFAZ))%>%
  mutate(VLR_MAX_SEFAZ = max(PROD_VUN_SEFAZ))
  #mutate(VLR_TRIB_MINIMO = ifelse(flag == "X" | flag == "T",min(PROD_VUNTRIB_AJUSTADO),VLR_TRIB_MINIMO))%>%
  #mutate(VLR_TRIB_MAXIMO =ifelse(flag == "X" | flag == "T",max(PROD_VUNTRIB_AJUSTADO),VLR_TRIB_MAXIMO))
}
### FIM FUNCAO VLRS EXTREMOS ###


### SALVA MEDIAS DATAFRAME ##
fn_sve_med <- function(df_table){
  tb_periodo <- df_table%>%
    distinct(IDE_DHEMI_PERIODO)
  
  x<-nrow(tb_periodo)
  y<-0
  
  for (i in 1:x) {
    y<-tb_periodo$IDE_DHEMI_PERIODO[i]
    
    vet <- df_table%>%
      filter(IDE_DHEMI_PERIODO == y)

    tb_tmp <- fn_calc_media(vet)
    ifelse(i == 1,tb_param <- tb_tmp,tb_param <- rbind(tb_param,tb_tmp))
  }
  return(tb_param)
}
### FIM SALVA MEDIAS ###

### SALVA EXTREMOS DATAFRAME ##
fn_sve_ext <- function(df_table){
  tb_periodo <- df_table%>%
    distinct(IDE_DHEMI_PERIODO)
  
  x<-nrow(tb_periodo)
  y<-0
  for (i in 1:x) {
    y<-tb_periodo$IDE_DHEMI_PERIODO[i]
    tb_tmp <- fn_calc_min_max(df_table,y)
    ifelse(i == 1,tb_param <- tb_tmp,tb_param <- rbind(tb_param,tb_tmp))
  }
  rm(tb_periodo)
  return(tb_param)
}
### FIM SALVA EXTREMOS

### CRIA COLUNAS ###
fn_cria_col <- function(df_table){
  df_table$PROD_VUNCOM_AJUSTADO <- as.double(0.0)
  df_table$PROD_QCOM_AJUSTADO <- as.double(0.0)
  df_table$PROD_UCOM_AJUSTADO <- as.character()
  df_table$VLR_UNITARIO_SEFAZ <- as.double(0.0)
  df_table$QTE_SEFAZ <- as.double(0.0)
  df_table$UNIDADE_SEFAZ <- as.character()
  df_table$VOLUME_SEFAZ <- as.double(0.0)
  df_table$VLR_MEDIO_SEFAZ <- as.double(0.0)
  df_table$VLR_MIN_SEFAZ <- as.double(0.0)
  df_table$VLR_MAX_SEFAZ <- as.double(0.0)
  df_table$FATOR_MULTIPLICADOR <- base::as.numeric(0)
  return(df_table)
}
### FIM CRIA COLUNAS ###

### FUNCAO AJUSTA REGISTROS COM OUTLIERS ###
fn_ajusta_outliers <- function(df_table,df_table2){
  tb_vlrs_calc<-df_table%>%
    distinct(IDE_DHEMI_PERIODO, VLR_MEDIO_SEFAZ,VLR_MIN_SEFAZ,VLR_MAX_SEFAZ)%>%
    arrange(IDE_DHEMI_PERIODO)
  
  vlrs_out<-anti_join(df_table2,df_table, by = c("IDNFE","DET_NITEM"))
  
  x<-left_join(vlrs_out,tb_vlrs_calc,by = "IDE_DHEMI_PERIODO")
  x<-rename(x,VLR_MEDIO_SEFAZ = 'VLR_MEDIO_SEFAZ.y',VLR_MIN_SEFAZ = 'VLR_MIN_SEFAZ.y',VLR_MAX_SEFAZ = 'VLR_MAX_SEFAZ.y')
  
  x$VLR_MEDIO_SEFAZ.x <- NULL
  x$VLR_MIN_SEFAZ.x <- NULL
  x$VLR_MAX_SEFAZ.x <- NULL
  
  x <- rbind(df_table,x)
  
  return(x)
}
### FIM AJUSTE OUTLIERS ###

