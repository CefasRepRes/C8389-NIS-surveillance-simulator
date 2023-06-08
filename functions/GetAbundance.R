#' 08/06/2023
#' Tom Gibson
#' GetAbundance with GetAbun.t
#' Checked 08/06/23 using graphs needs a logistic curve added. 

#' A helper function for runSurveillanceSimulation.R
#' it abundance for a given time step based on user defined parameters

#' Logistic model
#' N = N0*R^(t/g)

#' Madigan, M., et al. Brock Biology of Microorganisms, Global Edition, Pearson Education, Limited, 2018. 
#' and Vandermeer, J. (2010) How Populations Grow: The Exponential and Logistic Equations. 
#' Nature Education Knowledge 3(10):15

#' @param exp draw from an exponential growth curve. logical, if F draw logistic curve. 
#' @param t.yr the time from the simulation time step, in years generates t by t.yr*365
#' @param g the generation time (in days). 
#' @param N0 the starting individuals (at the beginning of the simulation). 
#' @param R the finite rate of population increase. 

#' @return N the population size at a given time. 

GetAbun.t <- function(t.yr = time, g = gen.time, N0 = 10, R = pop.R, exp = pop.expon){
  
  # Convert time (in years) into days 
  t <- t.yr*365
  
  if(exp == T){ # Run exponential growth model 

  N <- N0*R^(t/g)

  }else{} # Run logistic growth model - needs implementing not done yet

  return(N)
  
} # end function

