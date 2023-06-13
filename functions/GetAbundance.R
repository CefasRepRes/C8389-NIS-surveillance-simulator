#' 08/06/2023
#' Tom Gibson
#' GetAbundance with GetAbun.t
#' Checked line by line and with plots on 13/06/23

#' A helper function for runSurveillanceSimulation.R
#' gives abundance for a given time step (in years) based on user defined parameters

#' Exponential model
# N = N0 * e^(r*t)

#' Logistic Model
# N = K / (1 + ((K - N0) / N0) * e^(-r*t))

#' Reference
#' Rockwood, Larry L.. Introduction to Population Ecology, John Wiley & Sons, Incorporated, 2015. 
#' ProQuest Ebook Central, https://ebookcentral.proquest.com/lib/uea/detail.action?docID=1895786.

#' @param model choose exponential or logistic growth curve.  
#' @param N0 the starting individuals (at the beginning of the simulation). 
#' @param r the finite rate of population increase. 
#  e is Euler's constant (exp1 in R). 
#' @param K the carrying capacity in number of individuals. 

#' @return N the population size at a given time. 

GetAbun.t <- function(t = time, N0 = pop, r = pop_R, model = pop_model, K = pop_cap){
  
  e <- exp(1) # Create e
  
  if(model == "exponential"){ # Run exponential growth model 

    N <- N0 * e^(r*t)
  
  }else if(model == "logistic"){ # Run logistic growth model
  
    N <- K / (1 + ((K - N0) / N0) * e^(-r*t))

  }else{return("error: population model mis-spelt, choose exponential or logistic")} # return error
  
  return(N)
  
} # end function
