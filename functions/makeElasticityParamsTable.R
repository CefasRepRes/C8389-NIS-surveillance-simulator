#' makeElasticityParamsTable
#' checked 05/07/23
#' 
#' Function takes ranges for parameters (`params`) and generates a data frame of surveillance 
#' simulator input parameters in each row, modifying only one parameter from the input default 
#' (`defaults`) values by the amount set out by `elasticity_proportion`.
#'
#' @param defaults (class data.frame) default simulation parameters.
#' 
#' @param elasticity_prop (class numeric) proportion to change each parameter above and below 
#' default for elasticity testing. These data are obtained from `config.yaml` file and must be 
#' between 0 and 1.
#'
#' @return (class data.frame) expanded data frame containing a row detailing the input parameters 
#' to use for each of the simulations run. One parameter is altered at a time.
#' 
#' @export
#'
makeElasticityParamsTable <- function(defaults, elasticity_prop) {
  # create dataframe of scenarios with differing parameters
  # create the rows
  num_sites <- lapply(seq(from = defaults$num_sites - (defaults$num_sites * elasticity_prop),
                          to = defaults$num_sites + (defaults$num_sites * elasticity_prop),
                          by = (defaults$num_sites * elasticity_prop)), function(x) {
                            defaults[ , "num_sites"] <- x # edit the value
                            defaults[ , "name"] <- paste0("num_sites_", x) # rename the column 
                            return(defaults)
                          })
  
  num_years <- lapply(seq(from = defaults$num_years - (defaults$num_years * elasticity_prop),
                          to = defaults$num_years + (defaults$num_years * elasticity_prop),
                          by = (defaults$num_years * elasticity_prop)), function(x) {
                            defaults[ , "num_years"] <- x # edit the value
                            defaults[ , "name"] <- paste0("num_years_", x) # rename the column 
                            return(defaults)
                          })
  
  mean_visit_rate <- lapply(seq(from = defaults$mean_visit_rate - (defaults$mean_visit_rate * elasticity_prop),
                                to = defaults$mean_visit_rate + (defaults$mean_visit_rate * elasticity_prop),
                                by = (defaults$mean_visit_rate * elasticity_prop)), function(x) {
                                  defaults[ , "mean_visit_rate"] <- x # edit the value
                                  defaults[ , "name"] <- paste0("mean_visit_rate_", x) # rename the column 
                                  return(defaults)
                                })
  
  p_detection <- lapply(seq(from = defaults$p_detection - (defaults$p_detection * elasticity_prop),
                            to = defaults$p_detection + (defaults$p_detection * elasticity_prop),
                            by = (defaults$p_detection * elasticity_prop)), function(x) {
                              defaults[ , "p_detection"] <- x # edit the value
                              defaults[ , "name"] <- paste0("p_detection_", x) # rename the column 
                              return(defaults)
                            })
  
  establish_prob <- lapply(seq(from = defaults$establish_prob - (defaults$establish_prob * elasticity_prop),
                               to = defaults$establish_prob + (defaults$establish_prob * elasticity_prop),
                               by = (defaults$establish_prob * elasticity_prop)), function(x) {
                                 defaults[ , "establish_prob"] <- x # edit the value
                                 defaults[ , "establish_risk"] <- "equal uniform"
                                 defaults[ , "name"] <- paste0("establish_prob_", x) # rename the column 
                                 return(defaults)
                               })
  
  min_p_detect <- lapply(seq(from = defaults$min_p_detect - (defaults$min_p_detect * elasticity_prop),
                               to = defaults$min_p_detect + (defaults$min_p_detect * elasticity_prop),
                               by = (defaults$min_p_detect * elasticity_prop)), function(x) {
                                 defaults[ , "min_p_detect"] <- x # edit the value
                                 defaults[ , "establish_risk"] <- "equal uniform"
                                 defaults[ , "name"] <- paste0("min_p_detect_", x) # rename the column 
                                 return(defaults)
                               })
  
  max_p_detect <- lapply(seq(from = defaults$max_p_detect - (defaults$max_p_detect * elasticity_prop),
                                to = defaults$max_p_detect + (defaults$max_p_detect * elasticity_prop),
                                by = (defaults$max_p_detect * elasticity_prop)), function(x) {
                                  defaults[ , "max_p_detect"] <- x # edit the value
                                  defaults[ , "establish_risk"] <- "equal uniform"
                                  defaults[ , "name"] <- paste0("max_p_detect_", x) # rename the column 
                                  return(defaults)
                                })
  
  seed_n <- lapply(seq(from = defaults$seed_n - (defaults$seed_n * elasticity_prop),
                          to = defaults$seed_n + (defaults$seed_n * elasticity_prop),
                          by = (defaults$seed_n * elasticity_prop)), function(x) {
                            defaults[ , "seed_n"] <- x # edit the value
                            defaults[ , "name"] <- paste0("seed_n_", x) # rename the column 
                            return(defaults)
                          })
  
  # combine the list of rows containing parameters to test
  rows <- c(num_sites, num_years, mean_visit_rate, p_detection, establish_prob, min_p_detect, max_p_detect, seed_n)
  scenarios <- do.call(rbind.data.frame, rows)
  scenarios <- rbind(defaults, scenarios)
  
  # return data frame containing scenario parameters
  return(scenarios)
}
