# NIS-intro-detect-sim
Simulator to determine detection time of NIS introduced at seed site with different surveillance efforts (assumed no spread).

## Folder Structure

#### functions - folder for functions used by NIS-intro-detect-sim.R
  formatSensitivityResults.R - 
  getEstablishProbability.R - script to get the probability of establishment either equal uniform or random uniform
  getIntroProbability.R - script to get introduction probability - accepted inputs are random uniform or positive normal
  makeElasticityParamsTable.R - 
  makeSensitivityParamsTable.R - 
  runSurveillanceSensitivity.R - 
  runSurveillanceSimulation.R - script which runs the standard surveillance simulation
  summariseElasticityResults.R - 
  summariseSensitivityResults.R - 

#### outputs - folder for output htmls
  test_run_1 - folder for run 1. 
  
### parameters - folder for .yaml files containing parameters
  config.yaml - parameters for standard model run
  config_sensitivity.yaml - parameters for sensitivity analysis

#### R - main folders for scripts controlling the simulator
  NIS-intro-detect-sim.R - script for controlling the simulator
  report-NIS-intro-detect-elasticity.Rmd - script for generating elasticity mark down report
  report-NIS-intro-detect-sensitivity.Rmd - script for generating sensitivity mark down report
  report-NIS-intro-detect-sim.Rmd - script for generating standard mark down report
  
#### NIS-intro-detect-sim.Rproj - The NIS project file. 