# Simulator to evaluate time to detection based on a variety of surveillance strategies
# (assuming no spread from initial intro site)

## LOAD FUNCTIONS/PACKAGES -----------------------------------------------------------
source("functions/getEstablishProbability.R")
source("functions/getIntroProbability.R")
source("functions/runSurveillanceSimulation.R")

pkgs <- c("yaml", "here", "truncnorm")
lapply(pkgs, library, character.only = T)

## INPUTS ----------------------------------------------------------------------------
# load input parameters from config file
config <- yaml.load_file("parameters/config.yaml")

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
