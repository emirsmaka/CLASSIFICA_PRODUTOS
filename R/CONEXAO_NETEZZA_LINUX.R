conect_NZ <- function(x){library(DBI)
  library(RODBCDBI)

  con <- dbConnect(
    RODBCDBI::ODBC(),
    dsn = "ODBC Driver for NetezzaSQL",
    "DRIVER=ODBC Driver for NetezzaSQL;SERVER=s1436.ms;DATABASE=TRIBUTARIO_REFERENCIA;ReadOnly=true;UID=esmaka;PWD=@q158850i",
    case = 'nochange',
    believeNRows=FALSE
  )}
