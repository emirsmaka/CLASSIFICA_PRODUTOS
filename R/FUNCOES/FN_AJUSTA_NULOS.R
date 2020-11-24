fn_ajusta_nulos <- function(x){
  df_cerveja$QTE_SEFAZ <- ifelse(is.na(df_cerveja$QTE_SEFAZ),0.0,df_cerveja$QTE_SEFAZ)
  df_cerveja$VLR_UNITARIO_SEFAZ <- ifelse(is.na(df_cerveja$VLR_UNITARIO_SEFAZ),0.0,df_cerveja$VLR_UNITARIO_SEFAZ)
  df_cerveja$VLR_UNITARIO_SEFAZ <- ifelse(df_cerveja$VLR_UNITARIO_SEFAZ == "Inf",0.0,df_cerveja$VLR_UNITARIO_SEFAZ)
  df_cerveja$VOLUME_SEFAZ <- ifelse(is.na(df_cerveja$VOLUME_SEFAZ),0.0,df_cerveja$VOLUME_SEFAZ)
  df_cerveja$UN_MEDIDA_SEFAZ <- ifelse(is.na(df_cerveja$UN_MEDIDA_SEFAZ),"NAO AJUSTADA",df_cerveja$UN_MEDIDA_SEFAZ)
}
