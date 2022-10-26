#' Summarizes by 
#'
#' @return a plot
#' @export 

summarize_mussel <- function(measure, groupvars){
  out  <- ddply(mussels_wide, groupvars,  
                .fun = function(xx, col){ 
                  c(mean          = mean   (xx[ , col], na.rm = T),
                    min           = min    (xx[ , col],  na.rm = T),
                    median        = median (xx[ , col], na.rm = T),
                    max           = max    (xx[ , col], na.rm = T),
                    missing       = sum    (is.na(xx[ , col])),
                    deadN         = sum    (xx[ , 'dead'], na.rm = T),
                    prop_dead     = mean   (xx[ , 'dead'], na.rm = T)*100)
                }, measure)
  
  return(out)
}