# Simulator to evaluate time to detection based on a variety of surveillance strategies
# (assuming no spread from initial intro site)

## LOAD FUNCTIONS/PACKAGES -----------------------------------------------------------
source("functions/getEstablishProbability.R")
source("functions/getIntroProbability.R")
source("functions/runSurveillanceSimulation.R")
source("functions/makeSensitivityParamsTable.R")
source("functions/makeElasticityParamsTable.R")
source("functions/runSurveillanceSensitivity.R")
source("functions/formatSensitivityResults.R")
source("functions/summariseSensitivityResults.R")
source("functions/summariseElasticityResults.R")
source("functions/GetAbundance.R")
source("functions/ProcessMultipleResults.R")

pkgs <- c("yaml", "here", "truncnorm", "reshape2", "gtools",
          "ggplot2", "patchwork", "EnvStats", "ReIns", "data.table")
lapply(pkgs, library, character.only = T)

## INPUTS ----------------------------------------------------------------------------
# load input parameters from config file
config <- yaml.load_file("parameters/config.yaml")

# set seed
set.seed(config$seed)

# create output directory
dirs <- c("outputs" = here::here("outputs"),
          "results" = here::here("outputs", config$run_name))

lapply(dirs, dir.create, showWarnings = FALSE)

# define the surveillance scenarios to run
surveillance <- c("a_random", "b_risk_based", "c_heavy_risk_based")


## CREATE SIMULATION INPUTS -----------------------------------------------------------
# create vector of sites
site_vector <- 1:config$num_sites

# probability of establishment
p_establish <- getEstablishProbability(method = config$establish_risk,
                                       n_sites = config$num_sites,
                                       x = config$establish_prob)

p_intro <- getIntroProbability(method = "equal uniform", n_sites = config$num_sites, x = 0.8)

# combined introduction and establishment probs to give overall introduction rate
p_intro_establish <- p_intro * p_establish

## SCENARIO A: RANDOM SURVEILLANCE STRATEGY (independent of risk) -----------------------
# A: rate at which random sites are visited (vector)
site_visit_rate_A <- rep(x = config$mean_visit_rate, # if each site visited once (mean_visit_rate = 1)
                         times = config$num_sites)

# Stopped here still working on this. 
# run simulation A to determine the number of years which is takes to detect an introduction (1000 simulations in total)
resultsA <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                      site_revisit = F,
                                      surveillance_period = config$num_years,
                                      site_visit_rate = site_visit_rate_A,
                                      p_detection = config$det_prob,
                                      max_p_detect = config$det_prob_max,
                                      min_p_detect = config$det_prob_min,
                                      detection_dynamic = "threshold",
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish, 
                                      multiple_seed = T, 
                                      seed_prop = 0.10,
                                      start_pop = 1000,
                                      pop_R = 2,
                                      growth_model = "exponential",
                                      pop_cap = 500,
                                      APrb = 10,
                                      Abund_Threshold = 1000,
                                      Prob_Below = 0.1,
                                      Prob_Above = 0.8)



exmp <- resultsA

exmp <- ProcessMultipleResults(result.df = resultsA, detection.summary = "fefea")
summary(exmp)

resultsB <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                      site_revisit = F,
                                      surveillance_period = config$num_years,
                                      site_visit_rate = site_visit_rate_A,
                                      p_detection = 0.5,
                                      max_p_detect = config$det_prob_max,
                                      min_p_detect = config$det_prob_min,
                                      detection_dynamic = "constant",
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish, 
                                      multiple_seed = F, 
                                      seed_prop = 0.10,
                                      gen_time = 50,
                                      start_pop = 100,
                                      pop_R = 2,
                                      growth_model = "exponential",
                                      APrb = 100,
                                      Abund_Threshold = 1000,
                                      Prob_Below = 0.1,
                                      Prob_Above = 0.8)

summary(resultsB)




## SCENARIO B: RISK BASED SURVEILLANCE FOCUSSED ON HIGH RISK SITES ----------------------
# B: rate at which risk-based sites are visited (vector)
site_visit_rate_B <- rep(x = config$mean_visit_rate,
                         times = config$num_sites) * p_intro_establish / mean(p_intro_establish)

resultsB <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                      site_revisit = F,
                                      surveillance_period = config$num_years,
                                      site_visit_rate = site_visit_rate_B,
                                      p_detection = config$det_prob,
                                      max_p_detection = config$det_prob_max,
                                      min_p_detection = config$det_prob_min,
                                      detection_dynamic = config$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish, 
                                      multiple_seed = T, seed_prop = 0.10)

## SCENARIO C: RISK BASED SURVEILLANCE VERY FOCUSSED ON HIGH RISK SITES -----------------
# site visit rate with heavy focus on high risk sites
# note overall number of sites visits are the same as for the random surveillance (A)
# C: rate at which high risk-based sites are visited (vector)
site_visit_rate_C <- (rep(x = config$mean_visit_rate,
                          times = config$num_sites) * p_intro_establish ^ 3) / mean(p_intro_establish ^ 3)

## TODO: WHY IS THE SITE_REVISIT PARAMETER NOW TRUE?
resultsC <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                      site_revisit = F,
                                      surveillance_period = config$num_years,
                                      site_visit_rate = site_visit_rate_C,
                                      p_detection = config$det_prob,
                                      max_p_detection = config$det_prob_max,
                                      min_p_detection = config$det_prob_min,
                                      detection_dynamic = config$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish, 
                                      multiple_seed = T, seed_prop = 0.10)

## GENERATE RESULTS REPORT -----------------------------------------------------------------------
rmarkdown::render(input = "R/report-NIS-intro-detect-sim.Rmd", # Rmd to run
                  output_format ="html_document",
                  output_file = paste0("report-", config$run_name, ".html"),
                  output_dir = dirs[["results"]],
                  params = list(user_inputs = config,
                                p_establish = p_establish,
                                p_intro = p_intro,
                                p_intro_establish = p_intro_establish,
                                resultsA = resultsA,
                                resultsB = resultsB,
                                resultsC = resultsC,
                                site_visit_rate_A = site_visit_rate_A,
                                site_visit_rate_B = site_visit_rate_B,
                                site_visit_rate_C = site_visit_rate_C))


## RUN SENSITIVITY ANALYSIS ----------------------------------------------------------------------

# Sensitivity analysis determines the impact that each parameter has on the outputs 
# The simulation is run iteratively incrementally altering input parameters and plotting results

if (config$sensitivity_analysis == TRUE) {
  
  # read in sensitivity analysis configuration settings
  sens <- yaml.load_file("parameters/config_sensitivity.yaml")
  
  # generate a data frame of default parameters
  defaults <- data.frame(
    name = config$defaults$name,
    num_sites = config$defaults$num_sites,
    num_years = config$defaults$num_years,
    establish_risk = config$defaults$establish_risk,
    establish_prob = config$defaults$establish_prob,
    intro_risk = config$defaults$intro_risk,
    mean_visit_rate = config$defaults$mean_visit_rate,
    p_detection = config$defaults$p_detection,
    max_p_detect = config$defaults$max_p_detect,
    min_p_detect = config$defaults$min_p_detect,
    detect_dynamic = config$defaults$detect_dynamic
  )
  
  # adjust each parameter according to input sensitivity config
  scenarios <- makeSensitivityParamsTable(defaults = defaults,
                                          params = sens)
  
  # max_p_detect # min_p_detect
  
  # calculate sensitivity results for each scenario under each surveillance strategy
  # note this step can take some time to run
  s_results_all <- lapply(setNames(surveillance, surveillance), function(y) {
    
    # run the surveillance sensitivity 
    # NOTE this includes generation of p_intro and p_establish as well as runSurveillanceSimulation()
    s_results <- runSurveillanceSensitvity(X = scenarios,
                                           surveillance_scenario = y)
    
    # summarise sensitivity results: total non detected/percent detected etc
    s_results <- summariseSensitvityResults(results = s_results,
                                            config = config)
    
  })
  
  # get the names of the sensitivity factors from config file
  factors <- names(sens)
  names(factors) <- factors
  
  df_factors_all <- lapply(setNames(surveillance, surveillance), function(z) {
    # format sensitivity results and split by factor to plot
    df_factors <- formatSensitivityResults(x = s_results_all[[z]],
                                           config = config,
                                           factors = factors)
    
  })
  
  ## PRODUCE SENSITIVITY ANALYSIS REPORT
  rmarkdown::render(input = "R/report-NIS-intro-detect-sensitivity.Rmd", # Rmd to run
                    output_format ="html_document",
                    output_file = paste0("report-", config$run_name, "-sensitivity.html"),
                    output_dir = dirs[["results"]],
                    params = list(user_inputs = config,
                                  sensitivity_inputs = sens,
                                  factors = factors,
                                  df_factors_all = df_factors_all,
                                  defaults = defaults)
  )

}


## RUN ELASTICITY ANALYSIS ----------------------------------------------------------------------

# Elasticity analysis determines the impact that each parameter has on the outputs 
# To calculate elasticity value use equation:
  # param/result * diff in result/diff in param

if (config$elasticity_analysis == TRUE) {
  
  # generate a data frame of default parameters
  defaults <- data.frame(
    name = config$defaults$name,
    num_sites = config$defaults$num_sites,
    num_years = config$defaults$num_years,
    establish_risk = config$defaults$establish_risk,
    establish_prob = config$defaults$establish_prob,
    intro_risk = config$defaults$intro_risk,
    mean_visit_rate = config$defaults$mean_visit_rate,
    p_detection = config$defaults$p_detection,
    max_p_detect = config$defaults$max_p_detect,
    min_p_detect = config$defaults$min_p_detect,
    detect_dynamic = config$defaults$detect_dynamic
  )
  
  # adjust each parameter according to input sensitivity config
  scenarios <- makeElasticityParamsTable(defaults = defaults,
                                         elasticity_prop = config$elasticity_proportion)
  
  # run the surveillance elasticity 
  # NOTE this includes generation of p_intro and p_establish as well as runSurveillanceSimulation()
  # NOTE the runSurveillanceSensitivity function just runs the model multiple times so is ok for elasticity analysis
  e_results_all <- lapply(setNames(surveillance, surveillance), function(y) {
    
    e_results <- runSurveillanceSensitvity(X = scenarios,
                                           surveillance_scenario = y)
    
    # summarise elasticity results: total non detected/percent detected etc
    e_results <- summariseElasticityResults(results = e_results,
                                            config = config)
    
  })
  
  # define columns to remove
  cols_to_remove <- c("sim_results", "name")
  cols_elasticity <- colnames(e_results_all[[1]])[!colnames(e_results_all[[1]]) %in% cols_to_remove]
  
  # define names of factors to analyse elasticity for
  factors <- c("num_sites", "num_years", "mean_visit_rate", "p_detection", "establish_prob", "min_p_detect", "max_p_detect" )
  
  # set up data frames to record results
  elasticity_calcs_reduce <- data.frame(matrix(nrow = length(factors),
                                               ncol = length(cols_elasticity) + 1,
                                               dimnames = list(factors, c(cols_elasticity, "direction"))))
  elasticity_calcs_increase <- elasticity_calcs_reduce
  
  elasticity_calcs_increase$direction <- "Parameter Increased"
  elasticity_calcs_reduce$direction <- "Parameter Reduced"
  
  
  # loop over scenarios (random, risk based, heavy risk based)
  elasticity_dfs <- lapply(setNames(surveillance, surveillance), function(w) {
  
    e_results <- e_results_all[[w]]
    
    # loop over factors and cols_elasticity to calculate elasticity value
    lapply(setNames(factors, factors), function(x) {
      
      lapply(cols_elasticity, function(y) {
        
        # get default and new simulation outcomes
        out_default <- e_results[grep("default", e_results$name), y]
        out_reduce <- e_results[grep(x, e_results$name)[1], y]
        out_increase <- e_results[grep(x, e_results$name)[3], y]
        
        # get default and new parameter values
        param_default <- scenarios[grep("default", scenarios$name), x]
        param_reduce <- scenarios[grep(x, scenarios$name)[1], x]
        param_increase <- scenarios[grep(x, scenarios$name)[3], x]
        
        # calculate elasticity where parameter reduced and increased
        ## Note uses <<- operator to fill data.frame as value evaluated inside function
        elasticity_calcs_reduce[x, y] <<- (param_default / out_default) *
          (diff(c(out_default, out_reduce)) / diff(c(param_default, param_reduce)))
        elasticity_calcs_increase[x, y] <<- (param_default / out_default) *
          (diff(c(out_default, out_increase)) / diff(c(param_default, param_increase)))
        
      })
      
    })
    # create list of filled dataframes
    outputs <- list(Reduced = elasticity_calcs_reduce,
                    Increased = elasticity_calcs_increase)
    return(outputs)
    
  })
  
  ## PRODUCE ELASTICITY ANALYSIS REPORT
  rmarkdown::render(input = "R/report-NIS-intro-detect-elasticity.Rmd", # Rmd to run
                    output_format ="html_document",
                    output_file = paste0("report-", config$run_name, "-elasticity.html"),
                    output_dir = dirs[["results"]],
                    params = list(user_inputs = config,
                                  factors = factors,
                                  elasticity_dfs = elasticity_dfs))
  
}
