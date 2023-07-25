#' getIntroProbability 
#'
#' @param method character string containing details of method or distribution
#' to be used to set probability of introduction. Current accepted inputs are
#' "random uniform" which uses runif to define random introduction
#' probabilities that fit a uniform distribution and "positive normal" which uses mean = 0.5, sd = 0.25
#' "lognormal" which uses rlnormTrunc to define a lognormal distribution with meanlog = 0, sdlog = 1.5
#' "exponential" which uses rtexp to define a exponential distribution with rate = 4 
#' "lognormal", "exponential" and "positive normal" outputs are constrained between 0 and 1. 
#' Parameter is case insensitive.
#' @param n_sites numeric containing the number of sites within the simulation.
#' @param ... additional parameters of runif and rep functions.
#'
#' @return vector of length n_sites with results of probability of introduction
#' at each site.
#'
#' @examples p_intro <- getIntroProbability(method = "positive_normal", n_sites = 100)
#'           p_intro <- getIntroProbability(method = "equal uniform", n_sites = 100, x = 0.8)
#'
#' Checked by TG: 22/05/23
#' @importFrom truncnorm rtruncnorm
getIntroProbability <-
  function(method, n_sites, ...) {
    if (grepl("random uniform", method, ignore.case = T)) {
      # random uniform distribution
      p_intro <- runif(n_sites)
      
      # produce plots to check outputs
      par(mfrow = c(1, 1))
      hist(p_intro)
      
      # return desired output
      return(p_intro)
      
    } else if (grepl("positive normal", method, ignore.case = T)) {
      # random truncated normal distribution
      p_intro <- truncnorm::rtruncnorm(n = n_sites,
                                       a = 0, # min
                                       b = 1, # max
                                       mean = 0.5,
                                       sd = 0.25)
      
      # produce plots to check outputs
      par(mfrow = c(1, 1))
      hist(p_intro)
      
      # return desired output
      return(p_intro)
    
    } else if (grepl("equal uniform", method, ignore.case = T)) {
      # equal uniform distribution
      p_intro <- rep(..., n_sites)
      
      # print histogram of p_intro
      par(mfrow = c(1, 1))
      plot(p_intro)
      
      # return p_intro as output
      return(p_intro)
      
    } else if (grepl("lognormal", method, ignore.case = T)) {
      # define lognormal distribution
      p_intro <- rlnormTrunc(n_sites, meanlog = 0, sdlog = 1.5, min = 0, max = 1)
      
      # print histogram of p_intro
      par(mfrow = c(1,1))
      hist(p_intro)
      
      # return p_intro as output
      return(p_intro)
      
    } else if (grepl("exponential", method, ignore.case = T)) {
      # define exponential distribution ()
      p_intro <- rtexp(n_sites, rate = 4, endpoint = 1)
      
      # print histogram of p_intro
      par(mfrow = c(1,1))
      hist(p_intro)
      
      # return p_intro as output
      return(p_intro)
      
    } else {
      print("WARNING: method contains an invalid entry and is not calculated.")
      
    }
  }
