library(stringi)
library(stringr)
retira_termos <- function(x){
  ######## RETIRA TERMOS INDESEJAVEIS PARA IDENTIFICAR PRODUTOS ########
  ## PASSO-1: REMOVE CARACTERES ESPECIAIS E NUMEROS DE XPROD & IDENTIFICA OS TERMOS INDESEJAVEIS
  df_cerveja$XPROD_LIMPO <- gsub("[[:digit:]]|[[:punct:]]"," ",df_cerveja$PROD_XPROD)
  df_cerveja$XPROD_LIMPO <- str_trim(df_cerveja$XPROD_LIMPO,side = "left")
  df_cerveja$PROD_XPROD_LIMPO <- stri_trans_general(df_cerveja$PROD_XPROD_LIMPO,"Latin-ASCII")

  # SEPARA REGISTROS COM CERVEJA NA DESCRICAO
  token_cerveja <- c("\\<cerveja\\>|\\<cerv\\>")
  id_cerveja <- grep(token_cerveja,df_cerveja$XPROD_LIMPO,ignore.case = T)
  df_cerveja0 <- setdiff(df_cerveja,df_cerveja[id_cerveja,])

  token_1 <- c("\\<ades\\>|\\<nect\\>|\\<copo\\>|\\<mesa\\>|\\<cadeira\\>|\\<quadro\\>|\\<balde\\>|\\<achoc\\>|\\<soja\\>|\\<cajuina\\>|\\<energy\\>|\\<energ\\>|\\<energetico\\>|\\<qualita\\>|")
  token_2 <- c("\\<adesivo\\>|\\<adesivao\\>|\\<porta\\>|\\<refri\\>|\\<nectar\\>|\\<dafruta\\>|\\<antes\\>|\\<acendedor\\>|\\<abridor\\>|\\<completa\\>|\\<-nd\\>|\\<cervegela\\>|\\<-nd\\>|")
  token_3 <- c("\\<gigantografia\\>|\\<up\\>|\\<taca\\>|\\<tacas\\>|\\<caneca\\>|\\<garrafeiro\\>|\\<garrafeira\\>|\\<banana\\>|\\<gatorade\\>|\\<guarana\\>|\\<guar\\>|\\<halissept\\>|\\<saboraki\\>|")
  token_4 <- c("\\<refrigerante\\>|\\<refrig\\>|\\<crush\\>|\\<fanta\\>|\\<sufresh\\>|\\<balcao\\>|\\<refr\\>|\\<refresco\\>|\\<refriger\\>|\\<refesc\\>|")
  token_5 <- c("\\<achocolatado\\>|\\<leite\\>|\\<alimento\\>|\\<mineral\\>|\\<advantedge\\>|\\<backfilm\\>|\\<bolachas\\>|\\<bolacha\\>|\\<cadeiras\\>|\\<mesas\\>|\\<papelao\\>|\\<plastica\\>|\\<calda\\>|")
  token_6 <- c("\\<camabox\\>|\\<carenagem\\>|\\<cartaz\\>|\\<cartazete\\>|\\<cartazes\\>|\\<cartola\\>|\\<castelinho\\>|\\<cestinho\\>|\\<cesta\\>|\\<cestas\\>|\\<cha\\>|\\<chocoleite\\>|\\<citrus\\>|\\<coca\\>|")
  token_7 <- c("\\<coleira\\>|\\<copos\\>|\\<cotubaina\\>|\\<auqarius\\>|\\<sukita\\>|\\<del\\>|\\<expositor\\>|\\<funada\\>|\\<azeite\\>|\\<vinagr\\>|\\<vinagre\\>|\\<gel\\>|\\<geladeira\\>|\\<inflavel\\>|\\<camisa\\>|")
  token_8 <- c("\\<vendedor\\>|\\<panela\\>|\\<kit\\>|\\<lantejoula\\>|\\<placa\\>|\\<sufr\\>|\\<sufre\\>|\\<lasur\\>|\\<pepsi\\>|\\<nickelodeon\\>|\\<piloto\\>|\\<poltr\\>|\\<eucaf\\>|\\<polido\\>|milk")

  token_ind <- paste(token_1,token_2,token_3,token_4,token_5,token_6,token_7,token_8, sep = "")

  ## PASSO-2: INDEXA OS REGISTROS Q POSSUEM OS TERMOS INDESEJAVEIS
  id_token <- grep(token_ind,df_cerveja0$XPROD_LIMPO,ignore.case = T)

  ## PASSO-4: RETIRA OS REGISTROS COM TERMOS INDESEJAVEIS
  df_nao_cerveja <- df_cerveja0[id_token,]
  df_cerveja_limpo <- setdiff(df_cerveja0,df_nao_cerveja)


  rm(token_ind,token_8,token_7,token_6,token_5,token_4,token_3,token_2,token_1,token_cerveja)
  ####################################################################################################
}
