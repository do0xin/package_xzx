#'Divide different tables in a sheet according to blank lines,
#'and turn each table into a list
#'
#'@param x A datatable

tag <- function(x){
  data <- x[-1,.SD
  ][, setnames(.SD, 1, "argus")
  ][,tag := fifelse(str_detect(argus,"Company/refinery"),1,0)
  ][!is.na(tag), .SD
  ][,tag := cumsum(tag)
  ][tag!=0,.SD
  ][,.(sheet = list(.SD)),by = .(tag)]
}
