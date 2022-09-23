#' makeSensitivityParamsTable
#' 
#' Function takes ranges for parameters (`params`) and generates a data frame of surveillance 
#' simulator input parameters in each row, modifying only one parameter from the input default 
#' (`defaults`) values.
#'
#' @param defaults (class data.frame) default simulation parameters.
#' 
#' @param params (class list) ranges for each parameter for sensitivity testing (with minumum, 
#' maximum and interval values). These data are obtained from `config_sensitivity.yaml` file.
#'
#' @return (class data.frame) expanded data frame containing a row detailing the input parameters 
#' to use for each of the simulations run. One parameter is altered at a time.
#' 
#' @export
#'
makeSensitivityParamsTable <- function(defaults, params) {
  # create dataframe of scenarios with differing parameters
  # create the rows
  num_sites <- lapply(seq(from = params$num_sites$min,
                          to = params$num_sites$max,
                          by = params$num_sites$interval), function(x) {
                            defaults[ , "num_sites"] <- x # edit the value
                            defaults[ , "name"] <- paste0("num_sites_", x) # rename the column 
                            return(defaults)
                          })
  
  num_years <- lapply(seq(from = params$num_years$min,
                          to = params$num_years$max,
                          by = params$num_years$interval), function(x) {
                            defaults[ , "num_years"] <- x # edit the value
                            defaults[ , "name"] <- paste0("num_years_", x) # rename the column 
                            return(defaults)
                          })
  
  mean_visit_rate <- lapply(seq(from = params$mean_visit_rate$min,
                                to = params$mean_visit_rate$max,
                                by = params$mean_visit_rate$interval), function(x) {
                                  defaults[ , "mean_visit_rate"] <- x # edit the value
                                  defaults[ , "name"] <- paste0("mean_visit_rate_", x) # rename the column 
                                  return(defaults)
                                })
  
  p_detection <- lapply(seq(from = params$p_detection$min,
                            to = params$p_detection$max,
                            by = params$p_detection$interval), function(x) {
                              defaults[ , "p_detection"] <- x # edit the value
                              defaults[ , "name"] <- paste0("p_detection_", x) # rename the column 
                              return(defaults)
                            })
  
  establish_prob <- lapply(seq(from = params$establish_prob$min,
                               to = params$establish_prob$max,
                               by = params$establish_prob$interval), function(x) {
                                 defaults[ , "establish_prob"] <- x # edit the value
                                 defaults[ , "establish_risk"] <- "equal uniform"
                                 defaults[ , "name"] <- paste0("establish_prob_", x) # rename the column 
                                 return(defaults)
                               })
  
  # combine the list of rows containing parameters to test
  rows <- c(num_sites, num_years, mean_visit_rate, p_detection, establish_prob)
  scenarios <- do.call(rbind.data.frame, rows)
  
  # return data frame containing scenario parameters
  return(scenarios)
}
