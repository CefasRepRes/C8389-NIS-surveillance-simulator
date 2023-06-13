#' #' runSurveillanceSimulation 
#' # still needed checking but initial work showed it runs. 
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
#' @param max_p_detection numeric containing the maximum probability that the event
#' (e.g. infection or NIS) is detected.
#' @param min_p_detection numeric containing the minimum probability that the event
#' (e.g. infection or NIS) is detected.
#' @param detection_dynamic string stating whether detection of NIS remains constant
#' throughout the surveillance period or increases over time. Inputs are either: 
#' "constant", "threshold", "linear"
#' @param site_vector numeric vector of sites. 
#' @param p_intro_establish numeric vector stating probability of introduction and 
#' establishment of NIS at each site.
#' @param multiple_seed logical indicating if invasive species can occur at multiple sites
#' @param seed_prop numeric giving the proportion of sites where NIS maybe found
#' 
#' Population Modelling Relevant Parameters
#' gen.time, start.pop, pop.R, growth.model, 
#' @APrb the abundance required to increase the probability of detection by 0.01
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
#'                                                   p_intro_establish = c(0.008, 0.21, 0.045, ...), 
#'                                                   multiple_seed = T, 
#'                                                   seed_prop = 0.01)
#'
# Code Checked 05/06/2023 TG # needs more work. 
#'
runSurveillanceSimulation <- function(n_simulations,
                                      site_revisit,
                                      surveillance_period,
                                      site_visit_rate,
                                      p_detection,
                                      max_p_detection,
                                      min_p_detection,
                                      detection_dynamic,
                                      site_vector,
                                      p_intro_establish,
                                      multiple_seed,
                                      seed_prop,
                                      start_pop, 
                                      pop_R,
                                      growth_model,
                                      pop_cap,
                                      APrb, 
                                      Abund_Threshold, 
                                      Prob_Below, 
                                      Prob_Above){
  
  ## define empty results list of length n_simulations
  results <- vector(length = n_simulations, mode = "list")
  
  ## Run simulations
  for (i in 1:n_simulations){ # for every simulation
  
    # set the time to 0
    time <- 0
    
    ## Bind site_vector and site_visit_rate into a data.frame to allow resampling, or no resampling. 
    site_df <- data.frame(site_vector, site_visit_rate)

    # introduction seeded at site dependent on risk (if risk distribution is random normal most sites intermediate risk)
    # single site or multiple sites selected based on the prob but proportional to the other intro_establish probabilities sum(p_intro_establish)
    
    # if multiple sites are needed
    if(multiple_seed == T){ 

      # create multiple seed_sites
      seed_site <- sample(x = site_vector,
                          size = length(site_vector)*seed_prop, 
                          prob = p_intro_establish / sum(p_intro_establish))
      
      results[[i]] <- data.frame(seed_site) # create data.frame for 
      results[[i]]$dtime <- 0 # give a basic value of 0
      results[[i]]$sim_n <- i # give simulation number
    
      # if abundances are required give the seed sites multiple starting values 
      if(detection_dynamic %in% c("threshold", "linear")){
        
        # Draw n random poisson distributed starting values, mean (lambda) = start_pop 
        start_ab <- rpois(n = length(seed_site), lambda = start_pop)
        seed_ab <- data.frame(seed_site, start_ab)} # add these to seed sites
        
     # Or for single sites
     }else{seed_site <- sample(x = site_vector,
                              size = 1, 
                              prob = p_intro_establish / sum(p_intro_establish))
          
          results[[i]] <- data.frame(dtime = 0)} # give the default time value of 0
    
    # loop through site visits until the seed site is visited and detection = 1
    # i.e. there are no results and the time i not over the surveillance period. 
    # while results are not equal to the length of seed site. 
    
    while(0 %in% results[[i]]$dtime && time <= surveillance_period){
      
      # work out average time to visit one site assuming sum of visit rate sites will be visited per year
      # random deviates of the exponential distribution with rate = total of site_visit_rates e.g. 100 
      # exp. dist. skewed distribution that applies when the variable is the time to the first 
      # occurrence of an event. 1/rate = mean time to first occurrence i.e. 1/100 = 0.01
      # This is adding variation to the time it takes to visit somewhere. 
      
      new_time <- rexp(n = 1, rate = sum(site_visit_rate)) 
      
      # add period of time to original (simulate time passing) 
      time <- time + new_time
      
      # select site visited at this time based on the site vector and the 
      # site_visit_rate i.e. more likely to visit sites under the higher scenario. 
      # due to the way this while loop works, replacement cannot be controlled by sample as it is 
      # being repeatedly re-run and does not 'remember the site' drawn in the previous run. 
      # this is controlled later to allow replacement = F 
      
      visit <- sample(x = site_df$site_vector,
                      size = 1,
                      replace = F, # kept as a default
                      prob = site_df$site_visit_rate)
      
      # if constant detection probability over time:
      if (grepl("constant", detection_dynamic, ignore.case = T)){ # i.e. is detection constant. 
        # return a 0/1 based on p of detection 
        detect <- rbinom(n = 1, size = 1, prob = p_detection)
        
        # rbinomial gives you a 1 or a 0, n = obs., size = n.trials
        # prob - probability of success i.e. detection on each trial. 
        # if this just tells you was it found or not... 
      
      # if abundance change has a linear relationship with detection probability
      
        } else if(grepl("linear", detection_dynamic, ignore.case = T)){ # stop here... 
        
        # if multiple sites are present and the visited site is a seed site, otherwise pop is starting pop for single sites
        # or if its a different site just get the mean value
        if(multiple_seed == T & visit %in% seed_site){pop <- seed_ab$start_ab[seed_ab$seed_site == visit]}else{pop <- start_pop}
          
        # Get abundance at a given time point depending on growth model
        Abund <- GetAbun.t(t = time, N0 = pop, r = pop_R, model = growth_model, K = pop_cap)
        
        # Use the abundance divided by the APrb (abundance required to raise detection probability by 0.01) to increase p_detection
        scaled_prob <- p_detection + (Abund/APrb)*0.01
        
        # check scaled probability within min and max range
        scaling <- ifelse(scaled_prob > max_p_detect, max_p_detect, scaled_prob)
        scaling <- ifelse(scaling < min_p_detect, min_p_detect, scaling)
        
        # return a 0/1 based on p of detection which increases with time
        detect <- rbinom(n = 1, size = 1, prob = scaling)
      
      # if abundance is exponentially increasing and has a threshold relationship with detection probability
      } else if(grepl("threshold", detection_dynamic, ignore.case = T)){
        
        # if multiple sites are present and the visited site is a seed site, otherwise pop is starting pop for single sites
        # or if its a different site just get the mean value
        if(multiple_seed == T & visit %in% seed_site){pop <- seed_ab$start_ab[seed_ab$seed_site == visit]}else{pop <- start_pop}
        
        # Get abundance at a given time point depending on growth model
        Abund <- GetAbun.t(t = time, N0 = pop, r = pop_R, model = growth_model, K = pop_cap)
        
        if(Abund < Abund_Threshold){ # if abundance is below the threshold
          
          scaled_prob <- Prob_Below # give the low abundance probability value
          
        }else if(Abund >= Abund_Threshold){ # if abundance is above the threshold
          
          scaled_prob <- Prob_Above # give the high abundance probability value

        }else{} # do nothing 
        
        # check scaled probability within min and max range
        scaling <- ifelse(scaled_prob > max_p_detect, max_p_detect, scaled_prob)
        scaling <- ifelse(scaling < min_p_detect, min_p_detect, scaling)
        
        # return a 0/1 based on p of detection which decreases with time
        detect <- (rbinom(n = 1, size = 1, prob = scaling))

      }else {
        print("WARNING: detection_dynamic contains an invalid entry, see function documentation.")}
        
      # To add detection time result for multiple seed sites
      if(multiple_seed == T){
        
        # report detection time if a seed site visited and detected otherwise result remains at 0
        if(visit %in% seed_site & detect == 1){
          results[[i]]$dtime[results[[i]]$seed_site == visit] <- time
          
           if(site_revisit == F){ # if a site should not be revisted after it has been sampled

             # Remove row which matches site_vector in site_df so that site cannot be resampled
             site_df <- site_df[!site_df$site_vector %in% visit,]
            
           }else{} # nothing the site_vector remains the same and all sites can be withdrawn
          
          }else{} # nothing continue with simulation...
      
      # Add detection time for single seed sites
      }else{
        
        # report detection time if seed site visited and detected otherwise result remains at 0
        results[[i]]$dtime <- ifelse(visit == seed_site & detect == 1, time, 0)}
      
    } # end of while loop
    
    # if surveillance_period passes and nothing detected i.e. 0 change time to detection to 1000
    results[[i]]$dtime <- replace(results[[i]]$dtime, results[[i]]$dtime == 0, 1000)
    
    } # end of simulation
    
  ## Following simulation clean up results
  
    results <- as.data.frame(rbindlist(results)) # collapse list to data.frame
    
    # For single results just convert data.frame into a single vector
    if(multiple_seed == F){results <- as.vector(results$dtime)}else{}
    
  # return results
  return(results)
  
}

