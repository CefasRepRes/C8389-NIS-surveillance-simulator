#' getIntroAndEstablishProbability
#'
#' @param method character string containing details of method or distribution
#' to be used to set probability of introduction. Current accepted inputs are
#' "random uniform" which uses runif to define random establishment
#' probabilities that fit a uniform distribution and "positive normal" which
#' uses `rnorm()` to generate a random normal distribution. This is then
#' converted to a positive normal distribution by adding the minimum value in
#' the distribution multiplied by -1. Each of the probabilities of introduction
#' is multiplies by the p_establish vector. Parameter is case insensitive.
#' @param n_sites numeric containing the number of sites within the simulation.
#' @param p_establish vector of length n_sites contining probability of
#' establishment at each site.
#'
#' @return vector of length n_sites with results of probability of introduction
#' multiplied by the probability of establishment at each site.
#'
#' @examples p_intro_establish <- getEstablishProbability(method = "positive_normal",
#'                                                        n_sites = 100,
#'                                                        p_establish = c(0.008, 0.014, 0.15, ...))
#'
getIntroAndEstablishProbability <-
  function(method, n_sites, p_establish) {
    if (grepl("random uniform", method, ignore.case = T)) {
      # random uniform distributon (p_intro) * p_establish
      p_intro_establish <- runif(n_sites) * p_establish
      
      # produce plots to check outputs
      par(mfrow = c(1, 1))
      hist(p_intro_establish)
      par(mfrow = c(1, 1))
      
      # return desired output
      return(p_intro_establish)
      
    } else if (grepl("positive normal", method, ignore.case = T)) {
      # random normal distribution
      p_intro_rnd <- rnorm(n_sites)
      
      # convert to positive normal distribution (remove negatives)
      p_intro_pnd <- (p_intro_rnd + (min(p_intro_rnd) * -1))
      
      # combine positive normal (p_intro) * (p_establish)
      p_intro_establish <-
        (p_intro_pnd / mean(p_intro_pnd)) * p_establish
      
      # produce plots to check outputs
      par(mfrow = c(1, 3))
      hist(p_intro_rnd)
      hist(p_intro_pnd)
      hist(p_intro_establish)
      par(mfrow = c(1, 1))
      
      # return desired output
      return(p_intro_establish)
      
    } else {
      print("WARNING: method contains an invalid entry and is not calculated.")
      
    }
  }