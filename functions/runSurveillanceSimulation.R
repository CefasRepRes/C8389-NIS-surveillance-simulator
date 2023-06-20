#' #' runSurveillanceSimulation 
#' This function was checked line by line on 20/06/23 

#' @param n_simulations numeric number of simulations to run.
#' @param surveillance_period numeric period of time (years) for which 
#' surveillance and site visits are carried out.
#' @param site_visit_rate numeric vector containing the rate at which each site 
#' is visited per year. For example a rate of 1 indicates one visit per year, and 
#' a rate of 0.5 indicates one visit every two years.
#' @param p_detection numeric containing the probability that the event (e.g. 
#' infection or NIS) is detected.
#' @param max_p_detect numeric containing the maximum probability that the event
#' (e.g. infection or NIS) is detected.
#' @param min_p_detect numeric containing the minimum probability that the event
#' (e.g. infection or NIS) is detected.
#' @param detection_dynamic string stating whether detection of NIS remains constant
#' throughout the surveillance period or increases over time. Inputs are either: 
#' "constant", "threshold", "linear"
#' @param site_vector numeric vector of sites. 
#' @param p_intro_establish numeric vector stating probability of introduction and 
#' establishment of NIS at each site.
#' @param seed_n numeric giving the number of sites where NIS maybe found
#' 
#' Population Modelling Relevant Parameters
#' 
#' @param start_pop numeric, the starting initial population. For single sites a single value. For multiple sites 
#' a single value to define the possion distribution mean or a vector of values for each site. Passed to N0 in GetAbun.t
#' @param start_possion logical, indicating if a single start_pop value should be used as a mean value to draw from a possion distribution
#' @param pop_R numeric the finite rate of population increase. Passed to r in GetAbun.t
#' @param growth_model character defines the population growth model in "logistic" or "exponential". Passed to model in GetAbun.t
#' @param pop_cap numeric the carrying capacity at each site. Passed to K in GetAbun.t
#' @param APrb numeric the abundance required to increase the probability of detection by 0.01 if detection_dynmaic = linear
#' @param Abund_Threshold numeric the abundance which defines if a value of Prob_Below or Prob_Above is used for detection probability 
#' @param Prob_Below numeric the value given to the detection probability if abundance is below the threshold
#' @param Prob_Above numeric the value given to the detection probability if abundance is at or above the threshold
#' 
#' @return a data.frame containing the time taken for an event to be detected - dtime
#' for each seed site - seed_site, the simulation number - sim_n and if required the abundance at detection - abund
#' 
#' @examples sim_result <- runSurveillanceSimulation(n_simulations = 1000,
#'                                                   surveillance_period = 10,
#'                                                   site_visit_rate = c(1, 1, 1, 1, ...),
#'                                                   p_detection = 0.9,
#'                                                   site_vector = c(1, 2, 3, 4, ...),
#'                                                   p_intro_establish = c(0.008, 0.21, 0.045, ...), 
#'                                                   max_p_detect = 1.0,
#'                                                   min_p_detect = 0.0,
#'                                                   detection_dynamic = "linear",
#'                                                   seed_n = 1,
#'                                                   start_pop = 100, 
#'                                                   start_possion = T, 
#'                                                   pop_R = 2,
#'                                                   growth_model = "logistic",
#'                                                   pop_cap = 500,
#'                                                   APrb = 10, 
#'                                                   Abund_Threshold = 1000, 
#'                                                   Prob_Below = 0.1, 
#'                                                   Prob_Above = 0.8)
#'

runSurveillanceSimulation <- function(n_simulations,
                                      surveillance_period,
                                      site_visit_rate,
                                      p_detection,
                                      max_p_detect,
                                      min_p_detect,
                                      detection_dynamic,
                                      site_vector,
                                      p_intro_establish,
                                      seed_n,
                                      start_pop, 
                                      start_possion, 
                                      pop_R,
                                      growth_model,
                                      pop_cap,
                                      APrb, 
                                      Abund_Threshold, 
                                      Prob_Below, 
                                      Prob_Above){
  
  ## define empty results list of length n_simulations
  results <- vector(length = n_simulations, mode = "list")
  
  ## if abundance is used create abundance plot to show growth pattern
  if(detection_dynamic %in% c("threshold", "linear")){
  
  plt.abnds <- GetAbun.t(t = seq(0, surveillance_period, by = 1), N0 = start_pop, 
                         r = pop_R, model = growth_model, K = pop_cap)
  
  plot(x = seq(0, surveillance_period, by = 1), y = plt.abnds, xlab = "Years", ylab = "Abundance",
             main = paste0(growth_model, " Growth", ". Start_Pop:", start_pop, ". R:", pop_R))
  
  }
  
  ## Run simulations
  for (i in 1:n_simulations){ # for every simulation
  
    # set the time to 0
    time <- 0
    
    # Bind site_vector and site_visit_rate into a data.frame to prevent resampling
    site_df <- data.frame(site_vector, site_visit_rate)

    # introduction seeded at site dependent on risk (if risk distribution is random normal most sites intermediate risk)
    # This is at 1 to n sites

    seed_site <- sample(x = site_vector,
                          size = seed_n, 
                          prob = p_intro_establish / sum(p_intro_establish))
      
    results[[i]] <- data.frame(seed_site) # create data.frame
    results[[i]]$dtime <- 0 # give a default value of 0
    results[[i]]$sim_n <- i # give simulation number
    
    # if abundances are required... give the seed sites multiple starting values 
    if(detection_dynamic %in% c("threshold", "linear")){
      
      # Add in a result column for results
      results[[i]]$abund <- NA

      # Define the starting abundances for seed sites. 
      # If you want abundances to be a poisson distributed mean value
      if(start_possion == T){
        
       # Draw n random poisson distributed starting values, mean (lambda) = start_pop 
       start_ab <- rpois(n = length(seed_site), lambda = start_pop)
       seed_ab <- data.frame(seed_site, start_ab) # add these to seed sites
        
      # For user defined sites if the start population vector is same length as number of sites
      }else if(length(start_pop) == length(seed_site)){
        
       start_ab <- start_pop
       seed_ab <- data.frame(seed_site, start_ab)
        
      }else{return((print("Warning: Issue with starting population specificiation")))}
      
      } # end of abundance definition  

    # loop through site visits until all the seed sites are visited and detection = 1
    # i.e. there are no results and the time is not over the surveillance period. 

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
      # this is controlled later 
      
      visit <- sample(x = site_df$site_vector,
                      size = 1,
                      replace = F, # kept as a default
                      prob = site_df$site_visit_rate)
      
      # if constant detection probability over time:
      if (grepl("constant", detection_dynamic, ignore.case = T)){ # i.e. is detection constant. 
        
        # return a 0/1 based on p of detection 
        detect <- rbinom(n = 1, size = 1, prob = p_detection)
        
      # if abundance change has a linear relationship with detection probability
      
        }else if(grepl("linear", detection_dynamic, ignore.case = T)){
        
        # if the visited site is a seed site then pop is the pre-calculated abundance, otherwise it constant 
        if(visit %in% seed_site){pop <- seed_ab$start_ab[seed_ab$seed_site == visit]}else{pop <- 1}
          
        # Get abundance at a given time point depending on growth model
        Abund <- GetAbun.t(t = time, N0 = pop, r = pop_R, model = growth_model, K = pop_cap)
        
        # Give results the abundance - note this will only be given where a site is visited. 
        results[[i]]$abund[results[[i]]$seed_site == visit] <- Abund
        
        # Use the abundance divided by the APrb (abundance required to raise detection probability by 0.01) to change p_detection
        scaled_prob <- p_detection + (Abund/APrb)*0.01
        
        # check scaled probability within min and max range
        scaling <- ifelse(scaled_prob > max_p_detect, max_p_detect, scaled_prob)
        scaling <- ifelse(scaling < min_p_detect, min_p_detect, scaling)
        
        # return a 0/1 based on p of detection which changes with time
        detect <- rbinom(n = 1, size = 1, prob = scaling)
      
      # if abundance is exponentially increasing and has a threshold relationship with detection probability
      } else if(grepl("threshold", detection_dynamic, ignore.case = T)){
        
        # if the visited site is a seed site pop is a pre-calculated abundance, otherwise it constant 
        if(visit %in% seed_site){pop <- seed_ab$start_ab[seed_ab$seed_site == visit]}else{pop <- 1}
        
        # Get abundance at a given time point depending on growth model
        Abund <- GetAbun.t(t = time, N0 = pop, r = pop_R, model = growth_model, K = pop_cap)
        
        # Give results the abundance - note this will only be given where a site is visited. 
        results[[i]]$abund[results[[i]]$seed_site == visit] <- Abund
        
        if(Abund < Abund_Threshold){ # if abundance is below the threshold
          
          scaled_prob <- Prob_Below # give the low abundance probability value
          
        }else if(Abund >= Abund_Threshold){ # if abundance is at or above the threshold
          
          scaled_prob <- Prob_Above # give the high abundance probability value

        }else{return(print("WARNING: threshold incorrectly specified"))} # do nothing 
        
        # check scaled probability within min and max range
        scaling <- ifelse(scaled_prob > max_p_detect, max_p_detect, scaled_prob)
        scaling <- ifelse(scaling < min_p_detect, min_p_detect, scaling)
        
        # return a 0/1 based on p of detection which decreases with time
        detect <- rbinom(n = 1, size = 1, prob = scaling)
        
      }else {
        print("WARNING: detection_dynamic contains an invalid entry, see function documentation.")}
        
      # report detection time if a seed site visited and detected otherwise result remains at 0
       if(visit %in% seed_site & detect == 1){
          
         results[[i]]$dtime[results[[i]]$seed_site == visit] <- time
        
      # report abundance at the detection time if its a threshold or linear relationship
       if(detection_dynamic %in% c("threshold", "linear")){
          
         results[[i]]$abund[results[[i]]$seed_site == visit] <- Abund}
          
      # Remove row which matches site_vector in site_df so that site cannot be resampled
       site_df <- site_df[!site_df$site_vector %in% visit,]}
      
    } # end of while loop
    
    # if surveillance_period passes and nothing detected i.e. 0 change time to detection to 1000
    results[[i]]$dtime <- replace(results[[i]]$dtime, results[[i]]$dtime == 0, 1000)
    
    } # end of simulation
  
  ## Following simulation clean up results
  
    results <- as.data.frame(rbindlist(results)) # collapse list to data.frame
    
  # return results
  return(results)
  
}

