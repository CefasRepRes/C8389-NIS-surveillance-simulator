# Simulator to evaluate time to detection based on a variety of surveillance strategies
# (assuming no spread from initial intro site)

## LOAD FUNCTIONS/PACKAGES -----------------------------------------------------------
source("functions/getEstablishProbability.R")
source("functions/getIntroProbability.R")
source("functions/runSurveillanceSimulation.R")
source("functions/makeSensitivityParamsTable.R")
source("functions/runSurveillanceSensitivity.R")
source("functions/formatSensitivityResults.R")

pkgs <- c("yaml", "here", "truncnorm", "reshape2", "gtools",
          "ggplot2", "patchwork")
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


## CREATE SIMULATION INPUTS -----------------------------------------------------------
# create vector of sites
site_vector <- 1:config$num_sites

# probability of establishment
p_establish <- getEstablishProbability(method = config$establish_risk,
                                       n_sites = config$num_sites,
                                       x = config$establish_prob)

# probability of introduction
p_intro <- getIntroProbability(method = config$intro_risk,
                               n_sites = config$num_sites)

# combined introduction and establishment probs to give overall introduction rate
p_intro_establish <- p_intro * p_establish


## SCENARIO A: RANDOM SURVEILLANCE STRATEGY (independent of risk) -----------------------
# A: rate at which random sites are visited (vector)
site_visit_rate_A <- rep(x = config$mean_visit_rate, # if each site visited once (mean_visit_rate = 1)
                         times = config$num_sites)

# run simulation A to determine the number of years which is takes to detect an introduction (1000 simulations in total)
resultsA <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                      site_revisit = F,
                                      surveillance_period = config$num_years,
                                      site_visit_rate = site_visit_rate_A,
                                      p_detection = config$det_prob,
                                      max_p_detect = config$det_prob_max,
                                      min_p_detect = config$det_prob_min,
                                      detection_dynamic = config$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish)


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
                                      p_intro_establish = p_intro_establish)


## SCENARIO C: RISK BASED SURVEILLANCE VERY FOCUSSED ON HIGH RISK SITES -----------------
# site visit rate with heavy focus on high risk sites
# note overall number of sites visits are the same as for the random surveillance (A)
# C: rate at which high risk-based sites are visited (vector)
site_visit_rate_C <- (rep(x = config$mean_visit_rate,
                          times = config$num_sites) * p_intro_establish ^ 3) / mean(p_intro_establish ^ 3)

## TODO: WHY IS THE SITE_REVISIT PARAMETER NOW TRUE?
resultsC <- runSurveillanceSimulation(n_simulations = config$num_sim,
                                      site_revisit = T,
                                      surveillance_period = config$num_years,
                                      site_visit_rate = site_visit_rate_C,
                                      p_detection = config$det_prob,
                                      max_p_detection = config$det_prob_max,
                                      min_p_detection = config$det_prob_min,
                                      detection_dynamic = config$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish)


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
                                site_visit_rate_C = site_visit_rate_C)
                  )


## RUN SENSITIVITY ANALYSIS ----------------------------------------------------------------------
if (config$sensitivity_analysis == TRUE) {
  
  # read in sensitivity analysis configuration settings
  sens <- yaml.load_file("parameters/config_sensitivity.yaml")
  
  # generate a data frame of default parameters
  defaults <- data.frame(
    name = "default",
    num_sites = 100,
    num_years = 11,
    establish_risk = "equal uniform",
    establish_prob = 0.5,
    intro_risk = "random uniform",
    mean_visit_rate = 1,
    p_detection = 0.5,
    max_p_detect = 1,
    min_p_detect = 0,
    detect_dynamic = "constant"
  )
  
  # adjust each parameter according to input sensitivity config
  scenarios <- makeSensitivityParamsTable(defaults = defaults,
                                          params = sens)
  
  # scenarios[58, "establish_risk"] <- "random uniform"
  # scenarios[58, "name"] <- paste0("establish_risk", scenarios[58, "establish_risk"])
  # scenarios[59, "intro_risk"] <- "positive normal"
  # scenarios[59, "name"] <- paste0("intro_risk", scenarios[59, "intro_risk"])
  # detection_dynamic # max_p_detect # min_p_detect
  
  # run the surveillance sensitivity 
  # NOTE this includes generation of p_intro and p_establish as well as runSurveillanceSimulation()
  sens_results <- runSurveillanceSensitvity(X = scenarios)
  
  # summarise the sensitivity results
  sens_results$ct_detect <- apply(sens_results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) != 100]))
  sens_results$pct_detect <- apply(sens_results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) != 100])/config$num_sim * 100)
  sens_results$mean <- apply(sens_results, 1, function(x) mean(unlist(x[[1]])[unlist(x[[1]]) != 100]))
  sens_results$median <- apply(sens_results, 1, function(x) median(unlist(x[[1]])[unlist(x[[1]]) != 100]))
  sens_results$min <- apply(sens_results, 1, function(x) min(unlist(x[[1]])[unlist(x[[1]]) != 100]))
  sens_results$max <- apply(sens_results, 1, function(x) max(unlist(x[[1]])[unlist(x[[1]]) != 100]))
  sens_results$ct_no_detect <- apply(sens_results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) == 100]))
  sens_results$pct_no_detect <- apply(sens_results, 1, function(x) length(unlist(x[[1]])[unlist(x[[1]]) == 100])/config$num_sim * 100)
  
  # get the names of the sensitivity factors from config file
  factors <- names(sens)
  names(factors) <- factors
  
  # format sensitivity results and split by factor to plot
  df_factors <- formatSensitivityResults(x = sens_results,
                                         config = config,
                                         factors = factors)
  
  ## PRODUCE SENSITIVITY ANALYSIS REPORT
  rmarkdown::render(input = "R/report-NIS-intro-detect-sensitivity.Rmd", # Rmd to run
                    output_format ="html_document",
                    output_file = paste0("report-", config$run_name, "-sensitivity.html"),
                    output_dir = dirs[["results"]],
                    params = list(user_inputs = config,
                                  sensitivity_inputs = sens,
                                  factors = factors,
                                  df_factors = df_factors,
                                  defaults = defaults)
  )

}
