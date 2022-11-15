#' runSurveillanceSensitivity
#' 
#' Note: this function assumes that sites are visited at the same rate.
#'
#' @param X (class data.frame)  expanded data frame containing a row detailing the input parameters 
#' to use for each of the simulations run. One parameter is altered at a time.
#' 
#' @param surveillance_scenario (class string) stating the type of surveillance scenario to test. 
#' The input options are: "a_random", "b_risk_based", and "c_heavy_risk_based".
#'
#' @return (class data.frame) where each row depicts the results of one simulation run using the 
#' input `X` data.frame.
#' 
#' @export
#'
runSurveillanceSensitvity <- function(X, surveillance_scenario) {
  
  out <- as.data.frame(matrix(nrow = nrow(X), ncol = 1, NA))
  
  # FOR EACH OF THE ROWS
  for (i in 1:nrow(X)) {
    
    # create vector of sites
    site_vector <- 1:X$num_sites[i]
    
    # probability of establishment
    p_establish <- getEstablishProbability(method = X$establish_risk[i],
                                           n_sites = X$num_sites[i],
                                           x = X$establish_prob[i])
    
    # probability of introduction
    p_intro <- getIntroProbability(method = X$intro_risk[i],
                                   n_sites = X$num_sites[i])
    
    # combined introduction and establishment probs to give overall introduction rate
    p_intro_establish <- p_intro * p_establish
    
    
    # calculate the visit rate based on user input ----
    if (surveillance_scenario == "a_random") {

      visit_rate <- rep(x = X$mean_visit_rate[i], # if each site visited once (mean_visit_rate = 1)
                        times = X$num_sites[i])
      
    } else if (surveillance_scenario == "b_risk_based") {
      
      visit_rate <- rep(x = X$mean_visit_rate[i],
                        times = X$num_sites[i]) * p_intro_establish / mean(p_intro_establish)
      
    } else if (surveillance_scenario == "c_heavy_risk_based") {
      
      visit_rate <- (rep(x = X$mean_visit_rate[i],
                         times = X$num_sites[i]) * p_intro_establish ^ 3) / mean(p_intro_establish ^ 3) 
      
    } else {
      
      print("An invalid surveillance_scenario has been input.")
      
    }
    

    # run each of the input scenarios ----
    out[i, ][[1]] <- list(runSurveillanceSimulation(n_simulations = config$num_sim,
                                                    site_revisit = F,
                                                    surveillance_period = X$num_years[i],
                                                    site_visit_rate = visit_rate,
                                                    p_detection = X$p_detection[i],
                                                    max_p_detect = X$max_p_detect[i],
                                                    min_p_detect = X$min_p_detect[i],
                                                    detection_dynamic = X$detect_dynamic[i],
                                                    site_vector = site_vector,
                                                    p_intro_establish = p_intro_establish))
    #rownames(out[i, ]) <- X$name
  }
  
  out <- as.data.frame(out)
  names(out) <- "sim_results"
  out$name <- X$name
  
  return(out)
}
