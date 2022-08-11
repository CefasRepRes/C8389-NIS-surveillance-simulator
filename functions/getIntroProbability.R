#' getIntroProbability
#'
#' @param method character string containing details of method or distribution
#' to be used to set probability of introduction. Current accepted inputs are
#' "random uniform" which uses runif to define random establishment
#' probabilities that fit a uniform distribution and "positive normal" which
#' uses `rnorm()` to generate a random normal distribution. This is then
#' converted to a positive normal distribution by adding the minimum value in
#' the distribution multiplied by -1. Parameter is case insensitive.
#' @param n_sites numeric containing the number of sites within the simulation.
#'
#' @return vector of length n_sites with results of probability of introduction
#' at each site.
#'
#' @examples p_intro <- getIntroProbability(method = "positive_normal",
#'                                          n_sites = 100)
#'
getIntroProbability <-
  function(method, n_sites) {
    if (grepl("random uniform", method, ignore.case = T)) {
      # random uniform distribution
      p_intro <- runif(n_sites)
      
      # produce plots to check outputs
      par(mfrow = c(1, 1))
      hist(p_intro)
      par(mfrow = c(1, 1))
      
      # return desired output
      return(p_intro)
      
    } else if (grepl("positive normal", method, ignore.case = T)) {
      # random normal distribution
      p_intro_rnd <- rnorm(n_sites)
      
      # convert to positive normal distribution (remove negatives) and rescale
      positiveRescale <- function(x){(x-min(x))/(max(x)-min(x))}
      p_intro <- positiveRescale(p_intro_rnd)
      
      # produce plots to check outputs
      par(mfrow = c(1, 2))
      hist(p_intro_rnd)
      hist(p_intro)
      par(mfrow = c(1, 1))
      
      # return desired output
      return(p_intro)
      
    } else {
      print("WARNING: method contains an invalid entry and is not calculated.")
      
    }
  }
