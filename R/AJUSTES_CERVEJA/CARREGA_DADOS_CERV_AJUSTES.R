df_cerveja <- fread("./DADOS/nfe_cerveja2020.csv",dec = ",",integer64 = "numeric")
df_cerveja$PROD_QCOM <- as.integer(df_cerveja$PROD_QCOM)
df_cerveja$PROD_VUNCOM <- as.double(df_cerveja$PROD_VUNCOM)
df_cerveja$PROD_QTRIB <- as.integer(df_cerveja$PROD_QTRIB)
df_cerveja$PROD_VPROD <- as.double(df_cerveja$PROD_VPROD)
df_cerveja$PROD_VUNTRIB <- as.double(df_cerveja$PROD_VUNTRIB)

source("./SCRIPTS/AJUSTA_FATOR_QTE_CERVEJA_V2.R")


fwrite(df_cerveja,"./DADOS/cerveja2020.csv",sep = ";")

df_cerveja$VLR_UNITARIO_SEFAZ <- ifelse(is.na(df_cerveja$VLR_UNITARIO_SEFAZ),-1,df_cerveja$VLR_UNITARIO_SEFAZ)
