#' formatSensitivityResults
#' 
#' This function formats the data frame of results from the simulations with changing input 
#' parameters for sensitivity analysis. Each simulation result is saved in a separate column, the 
#' data are converted to long format, values of 100 (meaning no detection) are convered to NA, the 
#' scenario name factor is ordered using natural order, and the data frame is split by the factor 
#' that was altered in each of the scenarios.
#'
#' @param x (class data frame) output fom runSurveillanceSensitivity.
#' 
#' @param config (class list) list of configuration parameters input containing number of 
#' simulations labelled `num_sim`.
#' 
#' @param factors (class character) vectors of parameters altered within the sensitivity analysis 
#' obtained from unput list of sensivitiy analysis configuration parameters.
#'
#' @return (class list) of length `factors` containing formatted data frames separated by factor to 
#' test sensivitiy of.
#'
formatSensitivityResults <- function(x, config, factors) {
  # create names for columns and add
  cols <- paste0("sim_", 1:config$num_sim)
  x[ , cols] <- NA
  
  # convert the list of results in col1 to own columns
  for(i in 1:nrow(x)) {
    x[i, cols] <- x[[i, 1]]
  }
  
  # make long format
  x_long <- reshape2::melt(x,
                           id.vars = colnames(x)[-pmatch(cols, colnames(x))])
  
  # make results of 1000 equal to NA for plotting
  x_long$value[x_long$value == 1000] <- NA
  
  # apply natural order to name factor for plotting
  x_long$name <- factor(x = x_long$name,
                        levels = gtools::mixedsort(unique(x_long$name)))
  
  # remove original data column
  x_long$sim_results <- NULL
  
  # split results by sensitivity factor to test
  x_long_factors <- lapply(setNames(factors, factors), function(x) {
    x_long[grep(x, x_long$name), ]
  })
  
  return(x_long_factors)
}
