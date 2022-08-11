#' getEstablishProbability
#'
#' @param method character string containing details of method or distribution 
#' to be used to set probability of establishment. Current accepted inputs are 
#' "random uniform" which uses runif to define random establishment 
#' probabilities that fit a uniform distribution and "equal uniform" which requires 
#' additional input of a numeric establishment probability. Parameter is case 
#' insensitive.
#' @param n_sites numeric containing the number of sites within the simulation.
#' @param ... additional parameters of runif and rep functions.
#'
#' @return vector of length n_sites containing probability of establishment for 
#' each site defined by input method.
#'
#' @examples p_establish <- getEstablishProbability(method = "random uniform", n_sites = 100)
#'           p_establish <- getEstablishProbability(method = "equal uniform", n_sites = 100, x = 0.8)
#'           
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
    
  } else {
    print("WARNING: method contains an invalid entry and is not calculated.")
  }
}