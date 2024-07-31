import("dplyr")

consts <- use("constants.R")

createOptionsList <- function(choices, tid) {
  keys <- choices %>%
    lapply("[[", tid) %>%
    unname()
  
  values <- choices %>%
    lapply("[[", "id")
  
  names(values) <- keys
  return(values)
}


getMetricsChoices <- function(available_metrics, metrics_list, tid) {
  metrics_list[available_metrics] %>% createOptionsList(tid)
}
