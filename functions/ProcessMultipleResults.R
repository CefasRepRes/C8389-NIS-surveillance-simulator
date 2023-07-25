#' 09/06/2023
#' Tom Gibson
#' ProcessMultipleResults
#' checked 09/06/2023

#' A helper function to process the results of runSurveillanceSimulation where multiple sites are sampled. 
#' It groups over each simulation and gets the required summary statistic across sites. 

#' @param result.df a results data.frame from runSurveillanceSimulation
#' @param detection.summary method for detection summary statistic - mean, median, first and last

#' @return final a processed vector of results, one element per simulation 

ProcessMultipleResults <- function(result.df = resultsA, detection.summary = "last", create.plot = F){
  
  # Generate Summary Information
  if(detection.summary == "mean"){ # get the mean time to detection per simulation
    
    result.df %>% group_by(sim_n) %>% summarise(dtime = mean(dtime)) -> final.dat
    final.dat$dtime -> final
  
  }else if(detection.summary == "median"){ # get the median time to detection per simulation
  
    result.df %>% group_by(sim_n) %>% summarise(dtime = median(dtime)) -> final.dat
    final.dat$dtime -> final

  }else if(detection.summary == "first"){ # get the time to first detection i.e. minimum detection time
    
    result.df %>% group_by(sim_n) %>% summarise(dtime = min(dtime)) -> final.dat
    final.dat$dtime -> final

  }else if(detection.summary == "last"){ # get the time to last i.e. maximum detection time
    
    result.df %>% group_by(sim_n) %>% summarise(dtime = max(dtime)) -> final.dat
    final.dat$dtime -> final

  }else{return(print("Error: Wrongly specified process option"))}
  
  # Create Plot 
  if(create.plot == T){
  
  final.plot <- final # copy to prevent data loss. 
  final.plot[final.plot == 1000] <- NA # remove any values of 1000 as they screw the graph up
  
  # Plot the histogram 
  print(hist(x = final.plot, breaks = 100, main = paste("Summary Method:", detection.summary, "detection time"),
             xlab = "Summarised Detection Time (yrs)", ylab = "Frequency"))}else{}
    
  # Return final result
  return(final)
  
}
