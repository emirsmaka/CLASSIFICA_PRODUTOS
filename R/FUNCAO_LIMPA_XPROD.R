limpa_xprod=function(x){
  x%>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[/.|()#%.:-]", "") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace("_$", " ") %>%
    stringr::str_replace("^[' ']","") %>%
    stringr::str_replace_all("[0-9]","")
    
}