# Simulator to evaluate time to detection based on a variety of surveillance strategies
# (assuming no spread from initial intro site)

## LOAD FUNCTIONS/PACKAGES -----------------------------------------------------------
source("functions/getEstablishProbability.R")
source("functions/getIntroAndEstablishProbability.R")
source("functions/runSurveillanceSimulation.R")

library("yaml")

## INPUTS ----------------------------------------------------------------------------
# load input parameters from config file
params <- yaml.load_file("parameters/config.yaml")


## CREATE SIMULATION INPUTS -----------------------------------------------------------
# create vector of sites
site_vector <- 1:params$num_sites

# probability of establishment
p_establish <- getEstablishProbability(method = params$establish_risk,
                                       n_sites = params$num_sites,
                                       x = params$establish_prob)

# probability of introduction AND establishment
# this function defines the introduction rates using intro_risk input parameter
# introduction rate is then combined with rate of establishment to give overall introduction rate
p_intro_establish <- getIntroAndEstablishProbability(method = params$intro_risk,
                                                     n_sites = params$num_sites,
                                                     p_establish = p_establish)


## SCENARIO A: RANDOM SURVEILLANCE STRATEGY (independent of risk) -----------------------
# A: rate at which random sites are visited (vector)
site_visit_rate_A <- rep(x = params$mean_visit_rate,
                          times = params$num_sites) # each site visited once (mean_visit_rate = 1)

# run simulation A to determine the number of years which is takes to detect an introduction (1000 simulations in total)
resultsA <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                      site_revisit = F,
                                      surveillance_period = params$num_years,
                                      site_visit_rate = site_visit_rate_A,
                                      p_detection = params$det_prob,
                                      max_p_detect = params$det_prob_max,
                                      min_p_detect = params$det_prob_min,
                                      detection_dynamic = params$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish)


## SCENARIO B: RISK BASED SURVEILLANCE FOCUSSED ON HIGH RISK SITES ----------------------
# B: rate at which risk-based sites are visited (vector)
site_visit_rate_B <- rep(x = params$mean_visit_rate,
                          times = params$num_sites) * p_intro_establish / mean(p_intro_establish)

resultsB <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                      site_revisit = F,
                                      surveillance_period = params$num_years,
                                      site_visit_rate = site_visit_rate_B,
                                      p_detection = params$det_prob,
                                      max_p_detection = params$det_prob_max,
                                      min_p_detection = params$det_prob_min,
                                      detection_dynamic = params$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish)


## SCENARIO C: RISK BASED SURVEILLANCE VERY FOCUSSED ON HIGH RISK SITES -----------------
# site visit rate with heavy focus on high risk sites
# note overall number of sites visits are the same as for the random surveillance (A)
# C: rate at which high risk-based sites are visited (vector)
site_visit_rate_C <- (rep(x = params$mean_visit_rate,
                           times = params$num_sites) * p_intro_establish ^ 3) / mean(p_intro_establish ^ 3)

## TODO: WHY IS THE SITE_REVISIT PARAMETER NOW TRUE?
resultsC <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                      site_revisit = T,
                                      surveillance_period = params$num_years,
                                      site_visit_rate = site_visit_rate_C,
                                      p_detection = params$det_prob,
                                      max_p_detection = params$det_prob_max,
                                      min_p_detection = params$det_prob_min,
                                      detection_dynamic = params$detect_dynamic,
                                      site_vector = site_vector,
                                      p_intro_establish = p_intro_establish)


## SCENARIO 1: (A, B, C) RESULTS -----------------------------------------------------------------
## TODO: sort this code out - output report?
par(mfrow=c(3,1))
hist(site_visit_rate_A); hist(site_visit_rate_B); hist(site_visit_rate_C)
hist(resultsA, breaks=10, freq = T); hist(resultsB, breaks = 10, freq = T); hist(resultsC, breaks = 10, freq = T)
plot(resultsA); plot(resultsB); plot(resultsC)
summary(resultsA); summary(resultsB); summary(resultsC)

plotProbability <- (0:(params$num_sim - 1) / params$num_sim - 1)

# line plot for results A, B and C
par(mfrow=c(1,1))
plot(jitter(sort(resultsA)),
     plotProbability + 1, 
     type = 'l',
     xlim = c(0, params$num_years), # TODO: THIS CAN BE THE SURVEIILLANCE PERIOD?
     xlab = 'Time (years)',
     ylab = 'Probability of Detection',
     main = "Scenario 1: Constant rate of detection")
lines(sort(resultsB), plotProbability + 1, col = 'red')
lines(sort(resultsC), plotProbability + 1, col= 'blue')
legend("bottomright",
       c("random", "risk-based", "heavy risk-based"),
       col = c("black", "red", "blue"),
       cex = 0.6,
       pch = 19)