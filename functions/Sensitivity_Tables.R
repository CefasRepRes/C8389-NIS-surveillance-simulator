# 14/09/23
# Tom Gibson
# Sensitivity_Tables
# Checked 15/09/2023

# This script exports summaries of the data produced by SummariseSensitivityResults 

Sensitivity_Tables <- function(){
  
  # Create List
  exp.ls <- vector(mode = "list", length = length(s_results_all))
  names(exp.ls) <- names(s_results_all)
  
  # Loop over results
  for(i in 1:length(s_results_all)){
  
   exp.ls[[names(s_results_all[i])]] <- s_results_all[[i]][,2:10] # extract summary results
  
   exp.ls[[names(s_results_all[i])]]$scenario <- names(s_results_all[i])} # get scenario
  
  # Join the Results together
  sense_summary <- rbindlist(exp.ls)
  
  # Export Data
  write.csv(x = sense_summary, file = file.path("outputs", config$run_name, "Tables", paste0(config$run_name, "_Summary_Sensitivity.csv")))
  
}

