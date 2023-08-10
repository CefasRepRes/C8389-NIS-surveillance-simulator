#' summariseElasticityResults
#' # checked and updated on 10/08/23 after noticing an error. 
#' 
#' This function summarises the simulation results from multiple runs for elasticity analysis 
#' outputting information such as number of times detected (ct_detect/pct_detect)/not detected (ct_no_detect/pct_no_detect), mean, median, minimum and 
#' maximum time to detection. 
#' Therefore a single value is produced, across simulations, for each parameter, for each summary statistic. 
#'
#' @param results  (class data frame)output from runSurveillanceSensitivity.
#' 
#' @param config (class list) list of configuration parameters input containing number of 
#' simulations labelled `num_sim`.
#'
#' @return (class data frame) summary statistics from surveillance simulations.
#' 
summariseElasticityResults <- function(results,
                                       config) {
  
  results$ct_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$pct_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) != 1000])/config$num_sim * 100)
  results$mean <- apply(results, 1, function(x) mean(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$median <- apply(results, 1, function(x) median(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$quantile_95_detect <- apply(results, 1, function(x) quantile(unlist(x[[1]])[unlist(x[[1]]) != 1000], 0.95))
  results$quantile_5_detect <- apply(results, 1, function(x) quantile(unlist(x[[1]])[unlist(x[[1]]) != 1000], 0.05))
  results$min <- apply(results, 1, function(x) min(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$max <- apply(results, 1, function(x) max(unlist(x[[1]])[unlist(x[[1]]) != 1000]))
  results$ct_no_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) == 1000]))
  results$pct_no_detect <- apply(results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) == 1000])/config$num_sim * 100)
  
  return(results)

  }