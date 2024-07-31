
import("dplyr")

consts <- use("constants.R")


getTranslationArray <- function(a, i18n) {
  
  key <- names(a) %>%
    lapply(FUN=i18n$t)
  
  names(a) <- key
  
  return(a)
  
}

