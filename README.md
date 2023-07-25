# NIS-intro-detect-sim

Simulator to determine detection time of NIS introduced at seed site(s) with different surveillance efforts (assumes no spread). 
It may include variation in detection probability relative to abundance at each site. 

## Folder Structure

#### functions - folder for functions used by NIS-intro-detect-sim.R
  * formatSensitivityResults.R - formats the data frame from sensitivity analysis
  * getEstablishProbability.R - gets the probability of establishment based on the chosen distribution
  * getIntroProbability.R - gets introduction probability of establishment based on the chosen distribution
  * makeElasticityParamsTable.R - generates parameter dataframe for elasticity analysis by adjusting each parameter
  * makeSensitivityParamsTable.R - generates paramters dataframe for sensitvity analysis by adjusting each parameter
  * runSurveillanceSensitivity.R - runs the runSurveillanceSimulation repetively 
  * runSurveillanceSimulation.R -  runs the standard surveillance simulation
  * summariseElasticityResults.R - summarises the simulation results from multiple runs for elasticity analysis 
  * summariseSensitivityResults.R - summarises the simulation results from multiple runs for sensitivity analysis 

#### outputs - folder for output htmls
  * test_run_1 - folder for example test run
  
### parameters - folder for .yaml files containing parameters
  * config.yaml - parameters for standard model run
  * config_sensitivity.yaml - parameters for sensitivity analysis

#### R - main folders for scripts controlling the simulator
  * NIS-intro-detect-sim.R - script for controlling the simulator
  * report-NIS-intro-detect-elasticity.Rmd - script for generating elasticity mark down report
  * report-NIS-intro-detect-sensitivity.Rmd - script for generating sensitivity mark down report
  * report-NIS-intro-detect-sim.Rmd - script for generating standard mark down report
  
#### NIS-intro-detect-sim.Rproj - load before opening NIS-intro-detect-sim.R 