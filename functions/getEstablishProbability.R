#' getEstablishProbability
#'
#' @param method character string containing details of method or distribution 
#' to be used to set probability of establishment. Current accepted inputs are 
#' "random uniform" which uses runif to define random establishment 
#' probabilities that fit a uniform distribution
#' "equal uniform" which requires additional input of a numeric establishment probability. 
#' "lognormal" which uses rlnormTrunc to define a lognormal distribution 
#' with meanlog = 0, sdlog = 1.5
#' "exponential" which uses rtexp to define a exponential distribution with rate = 4 
#' "positive normal" which uses mean = 0.5, sd = 0.25
#' "lognormal", "exponential" and "positive normal" outputs are constrained between 0 and 1. 
#' Parameter is case insensitive.
#' @param n_sites numeric containing the number of sites within the simulation.
#' @param ... additional parameters of runif and rep functions.
#'
#' @return vector of length n_sites containing probability of establishment for 
#' each site defined by input method.
#'
#' @examples p_establish <- getEstablishProbability(method = "random uniform", n_sites = 100)
#'           p_establish <- getEstablishProbability(method = "equal uniform", n_sites = 100, x = 0.8)
#'
#' Checked by TG: 22/05/23  

getEstablishProbability <- function(method, n_sites, ...) {
  
  if (grepl("random uniform", method, ignore.case = T)) {
    # random uniform distribution
    p_establish <- runif(n_sites)
    
    # print histogram of p_establish
    par(mfrow = c(1, 1))
    plot(p_establish)
    
    # return p_establish as output
    return(p_establish)
    
  } else if (grepl("equal uniform", method, ignore.case = T)) {
    # equal uniform distribution
    p_establish <- rep(..., n_sites)
    
    # print histogram of p_establish
    par(mfrow = c(1, 1))
    plot(p_establish)
    
    # return p_establish as output
    return(p_establish)
   
  } else if (grepl("lognormal", method, ignore.case = T)) {
    # define lognormal distribution
    p_establish <- rlnormTrunc(n_sites, meanlog = 0, sdlog = 1.5, min = 0, max = 1)
    
    # print histogram of p_establish
    par(mfrow = c(1,1))
    hist(p_establish)
    
    # return p_establish as output
    return(p_establish)
  
  } else if (grepl("exponential", method, ignore.case = T)) {
    # define exponential distribution ()
    p_establish <- rtexp(n_sites, rate = 4, endpoint = 1)

    # print histogram of p_establish
    par(mfrow = c(1,1))
    hist(p_establish)
    
    # return p_establish as output
    return(p_establish)
  
  } else if (grepl("positive normal", method, ignore.case = T)) {
    # random truncated normal distribution
    p_establish <- truncnorm::rtruncnorm(n = n_sites,
                                     a = 0, # min
                                     b = 1, # max
                                     mean = 0.5,
                                     sd = 0.25)
    
    # produce plots to check outputs
    par(mfrow = c(1, 1))
    hist(p_establish)
    
    # return desired output
    return(p_establish)
    
  } else {
    print("WARNING: method contains an invalid entry and is not calculated.")
  }
}
