### Inicio da Funcao para retirar os Acentos
# Rotinas e funcoes uteis V 1.0
# rm.accent - REMOVE ACENTOS DE PALAVRAS
# Funcao que tira todos os acentos e pontuacoes de um vetor de strings.
# Parametros:
# str - vetor de strings que terao seus acentos retirados.
# patterns - vetor de strings com um ou mais elementos indicando quais acentos deverao ser retirados.
#            Para indicar quais acentos deverao ser retirados, um vetor com os simbolos deverao ser passados.
#            Exemplo: pattern = c("??", "^") retirara os acentos agudos e circunflexos apenas.
#            Outras palavras aceitas: "all" (retira todos os acentos, que sao "??", "`", "^", "~", "¨", "c")
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="C"))
    pattern[pattern=="C"] <- "c"
  symbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("??","`","^","~","¨","c")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}
