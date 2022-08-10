#' #' runSurveillanceSimulation
#'
#' @param n_simulations numeric number of simulations to run.
#' @param site_revisit logical TRUE/FALSE whether sites can be revisited or not 
#' in the simulation.
#' @param surveillance_period numeric period of time (years) for which 
#' surveillance and site visits are carried out.
#' @param site_visit_rate numeric vector containing the rate at which each site 
#' is visited per year. For example a rate of 1 indicates one visit per year, and 
#' a rate of 0.5 indicates one visit every two years.
#' @param p_detection numeric containing the probability that the event (e.g. 
#' infection or NIS) is detected.
#' @param detection_dynamic string stating whether detection of NIS remains constant
#' throughout the surveillance period or increases over time. Inputs are either: 
#' "constant" or "increasing". 
#' @param site_vector numeric vector of sites
#' @param p_intro_establish numeric vector stating probability of introduction and 
#' establishment of NIS at each site.
#'
#' @return numeric vector of length n_simulations containing the time taken for 
#' an event to be detected at a seed site given set parameters such as site 
#' revisiting, probability of detection, site visit rate and surveillance period.
#'
#' @examples sim_result <- runSurveillanceSimulation(n_simulations = 1000,
#'                                                   site_revisit = F,
#'                                                   surveillance_period = 10,
#'                                                   site_visit_rate = c(1, 1, 1, 1, ...),
#'                                                   p_detection = 0.9,
#'                                                   site_vector = c(1, 2, 3, 4, ..),
#'                                                   p_intro_establish = c(0.008, 0.21, 0.045, ...))
#'                                                   
runSurveillanceSimulation <- function(n_simulations, site_revisit, surveillance_period,
                                      site_visit_rate, p_detection, detection_dynamic,
                                      site_vector, p_intro_establish) {
  # define empty result vector of length n_simulations
  results <- numeric(n_simulations)
  
  for (i in 1:n_simulations) {
    # set the time to 0
    time <- 0
    
    # introduction seeded at site dependent on risk (if risk distribution is random normal most sites intermediate risk)
    seed_site <- sample(x = site_vector,
                        size = 1,
                        replace = site_revisit, # if set to F site is not revisited
                        prob = p_intro_establish / sum(p_intro_establish))
    
    # loop through site visits until the seed site is visited and detection = 1
    while(results[i] == 0 && time <= surveillance_period) {
      
      # work out average time to visit one site assuming sum of visit rate sites will be visited per year
      new_time <- rexp(n = 1, rate = sum(site_visit_rate))
      
      # add period of time to original (simulate time passing)
      time <- time + new_time
      
      # select site visited at this time
      visit <- sample(x = site_vector,
                      size = 1,
                      replace = site_revisit, # if set to F site is not revisited
                      prob = site_visit_rate)
      
      # if constant detection probability over time:
      if (grepl("constant", detection_dynamic, ignore.case = T)) {
        # return a 0/1 based on p of detection
        detect <- rbinom(n = 1, size = 1, prob = p_detection)
        
        # else if increasing detection probability over time:
      } else if (grepl("increasing", detection_dynamic, ignore.case = T)) {
        # scale the detection probability up by (1 - exponent of negative time)
        scaled_prob <- det_prob + (1 - (exp(-time)))
        
        # check scaled probability within min and max range
        scaling <- ifelse(scaled_prob > det_prob_max, det_prob_max, scaled_prob)
        scaling <- ifelse(scaling < det_prob_min, det_prob_min, scaling)
        
        # return a 0/1 based on p of detection which increases with time
        detect <- (rbinom(n = 1, size = 1, prob = scaling))
        
        # else if decreasing detection probability over time:
      } else if (grepl("decreasing", detection_dynamic, ignore.case = T)) {
        # scale the detection probability down by (1 - exponent of negative time)
        scaled_prob <- det_prob - (1 - (exp(-time)))
        
        # check scaled probability within min and max range
        scaling <- ifelse(scaled_prob > det_prob_max, det_prob_max, scaled_prob)
        scaling <- ifelse(scaling < det_prob_min, det_prob_min, scaling)
        
        # return a 0/1 based on p of detection which decreases with time
        detect <- (rbinom(n = 1, size = 1, prob = scaling))
        
      } else {
        print("WARNING: detection_dynamic contains an invalid entry, see function documentation.")
        
      }
      # report time if seed site visited and detected otherwise result remains at 0
      results[i] <- ifelse(visit == seed_site & detect == 1, time, 0)
      
    }
    # if surveillance_period passes with no detection report 100 otherwise report the time to detection (results[i])
    results[i] <- ifelse(results[i] == 0, 100, results[i])
    
  }
  return(results)
  
}