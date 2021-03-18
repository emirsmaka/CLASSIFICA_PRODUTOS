

x <- df_refri_NFCE%>%
  group_by(IDE_DHEMI_PERIODO, PROD_CEANTRIB)%>%
  summarise(QTE_NFCE = n())%>%
  arrange(desc(QTE_NFCE))
y <- df_refri_NFCE_outlier%>%
  group_by(IDE_DHEMI_PERIODO, PROD_CEANTRIB)%>%
  summarise(QTE_OULIER = n())%>%
  arrange(desc(QTE_OULIER))
z <- left_join(x,y, by=c("IDE_DHEMI_PERIODO","PROD_CEANTRIB"))
z <- 
