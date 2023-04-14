#'for cleaning "Russian refinery output.xlsx" data

argus <- function(y){
  flat <- function(x) {
    x[, setnames(.SD, .SD[1] %>% unlist())
    ][-1, .SD
    ][,melt(.SD,id.vars = 1)
      # ][!str_detect(variable,"±%|YTD|\\.\\.\\.")&!str_detect(`Company/refinery`,"Russian refinery output"),.SD
    ][,":="(variable = as.character(variable),
            value = as.numeric(value))]
  }
  y[, .(sheet = lapply(sheet, flat)), by = tag
  ][, rbindlist(sheet), by = tag]
}
