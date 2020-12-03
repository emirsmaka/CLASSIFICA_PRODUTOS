fn_token_indesejado <- function(x){
  x <- as.character()
  for (i in 1:length(word)) {
    x[i] <- paste("\\<",word[i],"\\>",sep = "")

  }
  return(x)
}
