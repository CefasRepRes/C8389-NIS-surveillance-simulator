# NIS-intro-detect-sim
A  surveillance simulator to determine detection time, detection failure and overall survey probability of detection for NIS introduced at seed site(s) with different surveillance efforts (assumes no spread). 
It may include variation in method detection probability relative to abundance at each site as defined by a detection dynamic. 

## Open Government Licence Statement
THIS INFORMATION IS LICENSED UNDER THE CONDITIONS OF THE OPEN GOVERNMENT LICENCE found at: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3
The following attribution statement MUST then be cited in your products and applications when using this information. 
Contains public sector information licensed under the Open Government license v3. 

## Folder Structure

#### functions - folder for functions used by NIS-intro-detect-sim.R
  * formatSensitivityResults.R - formats the data frame from sensitivity analysis
  * getEstablishProbability.R - gets the probability of establishment based on the chosen distribution
  * getIntroProbability.R - gets introduction probability of establishment based on the chosen distribution
  * makeElasticityParamsTable.R - generates parameter dataframe for elasticity analysis by adjusting each parameter
  * makeSensitivityParamsTable.R - generates parameters dataframe for sensitivity analysis by adjusting each parameter
  * runSurveillanceSensitivity.R - runs the runSurveillanceSimulation repetively 
  * runSurveillanceSimulation.R -  runs the standard surveillance simulation
  * summariseElasticityResults.R - summarises the simulation results from multiple runs for elasticity analysis 
  * summariseSensitivityResults.R - summarises the simulation results from multiple runs for sensitivity analysis 
  * GetAbundance.R - helper function for runSurveillanceSimulation.R which gives abundance for a given time step (in years) based on user defined parameters
  * Elasticity_Graphs_All.R - helper function to export plots from the elasticity analysis
  * Elasticity_Graphs_Main.R - helper function to export the key elasticity plot used in the publication
  * Elasticity_Tables.R - helper function to export the elasticity data table
  * Sensitivity_Graphs.R - helper function to export sensitivity plots
  * Sensitivity_Tables.R - helper function to export data tables from the sensitivity analysis
  * Sim_Graphs_Tables.R - helper function to export various simulator tables and graphs for publication
  * ProcessMultipleResults.R - helper function to summarise results from multiple sites
    
#### outputs - folder for output htmls
  * Sim_x - folder(s) containing outputs for simulations numbered x
  * Manuscript - folder containing manuscript graphs
    
#### parameters - folder for .yaml files containing parameters
  * config_sim_x.yaml - parameters for standard model run
  * config_sensitivity.yaml - parameters for sensitivity analysis

#### R - main folders for scripts controlling the simulator
  * Manuscript_Plots.R - script for creating plots for manuscript
  * NIS-intro-detect-sim_x.R - script for controlling the simulator
  * report-NIS-intro-detect-elasticity.Rmd - markdown script for elasticity analysis
  * report-NIS-intro-detect-sensitivity.Rmd - markdown script for sensitivity analysis
  * report-NIS-intro-detect-sim.Rmd - markdown script for simulator

