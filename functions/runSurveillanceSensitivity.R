#' runSurveillanceSensitivity
#' Checked 29/06/23
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
runSurveillanceSensitvity <- function(X, surveillance_scenario, show.rows = T) {
  
  out <- as.data.frame(matrix(nrow = nrow(X), ncol = 1, NA))
  
  # FOR EACH OF THE ROWS
  for (i in 1:nrow(X)) {
    
    # if desired state the name of the sensitivity simulation
    if(show.rows == T){print(X$name[i])}
    
    # create vector of sites
    site_vector <- 1:X$num_sites[i]
    
    # probability of establishment
    p_establish <- getEstablishProbability(method = X$establish_risk[i],
                                           n_sites = X$num_sites[i],
                                           x = X$establish_prob[i])
    
    # probability of introduction
    p_intro <- getIntroProbability(method = X$intro_risk[i],
                                   n_sites = X$num_sites[i],
                                   x = x$intro_prob)
    
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
    resultsS <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                                    surveillance_period = X$num_years[i],
                                                    site_visit_rate = visit_rate,
                                                    p_detection = X$p_detection[i],
                                                    max_p_detect = X$max_p_detect[i],
                                                    min_p_detect = X$min_p_detect[i],
                                                    detection_dynamic = X$detect_dynamic[i],
                                                    site_vector = site_vector,
                                                    p_intro_establish = p_intro_establish,
                                                    seed_n = X$seed_n[i],
                                                    start_pop = X$start_pop[i],
                                                    start_possion = X$start_possion[i],
                                                    pop_R = X$pop_R[i],
                                                    growth_model = as.logical(X$start_possion)[i],
                                                    pop_cap = X$pop_cap[i],
                                                    APrb = X$APrb[i],
                                                    Abund_Threshold = X$Abund_Threshold[i],
                                                    Prob_Below = X$Prob_Below[i],
                                                    Prob_Above = X$Prob_Above[i]
                                          )
    
    # Select out the appropriate result 
    if(X$seed_n[i] == 1){resultsS_dt <- resultsS$dtime
    
    }else if(X$seed_n[i] > 1){resultsS_dt <- ProcessMultipleResults(result.df = resultsS, detection.summary = config$detect_summary,
                                                                    create.plot = F)}
    
    # Import into the data.frame. 
    out[i, ][[1]] <- list(resultsS_dt)
    
  }
  
  out <- as.data.frame(out)
  names(out) <- "sim_results"
  out$name <- X$name
  
  return(out)
}
