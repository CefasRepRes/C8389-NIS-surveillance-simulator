#' summariseSensitivityResults
#' 
#' This function summarises the simulation results from multiple runs for sensitivity analysis 
#' outputting information such as number of times detected/not detected, mean, median, minimum and 
#' maximum time to detection. 
#'
#' @param results (class data frame) output fom runSurveillanceSensitivity.
#' 
#' @param config (class list) list of configuration parameters input containing number of 
#' simulations labelled `num_sim`.
#'
#' @return (class data frame) summary statistics from surveillance simulations.
#'
SummariseSensitivityResults <- function(results,
                                       config) {
  
  results$ct_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$pct_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) != 1000])/config$num_sim * 100)
  results$mean <- apply(results, 1, function(x) mean(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$median <- apply(results, 1, function(x) median(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$min <- apply(results, 1, function(x) min(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$max <- apply(results, 1, function(x) max(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$ct_no_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) == 1000]))
  results$pct_no_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) == 1000])/config$num_sim * 100)
  
  return(results)
}