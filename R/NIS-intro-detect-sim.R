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
p_intro_establish <- getIntroAndEstablishProbability(method = params$intro_risk, n_sites = params$num_sites, p_establish = p_establish)


## SCENARIO 1: ASSUME CONSTANT PROBABILITY OF DETECTING NIS INTRODUCTION THROUGH TIME ----
## SCENARIO 1A: RANDOM SURVEILLANCE STRATEGY (independent of risk) -----------------------
# 1A: rate at which random sites are visited (vector)
site_visit_rate_1a <- rep(params$mean_visit_rate, length(site_vector)) # each site visited once (mean_visit_rate = 1)

# run simulation 1A to determine the number of years which is takes to detect an introduction (1000 simulations in total)
results1a <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                       site_revisit = F,
                                       surveillance_period = params$num_years,
                                       site_visit_rate = site_visit_rate_1a,
                                       p_detection = params$det_prob,
                                       detection_dynamic = "constant",
                                       site_vector = site_vector,
                                       p_intro_establish = p_intro_establish)


## SCENARIO 1B: RISK BASED SURVEILLANCE FOCUSSED ON HIGH RISK SITES ----------------------
# CONSTANT DETECTION WITH SAME OVERALL SITE VISITS
# ESTABLISH RISK: equal uniform distribution with establish_prob
## TODO: SITE AND ESTABLISH RISK THE SAME AS PREVIOUS - DO THESE NEED TO BE EDITABLE?
site_visit_rate_1b <- rep(params$mean_visit_rate, params$num_sites) * p_intro_establish / mean(p_intro_establish)

results1b <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                       site_revisit = F,
                                       surveillance_period = params$num_years,
                                       site_visit_rate = site_visit_rate_1b,
                                       p_detection = params$det_prob,
                                       detection_dynamic = "constant",
                                       site_vector = site_vector,
                                       p_intro_establish = p_intro_establish)


## SCENARIO 1C: RISK BASED SURVEILLANCE VERY FOCUSSED ON HIGH RISK SITES -----------------
# site visit rate with heavy focus on high risk sites
# note overall number of sites visits are the same as for the random surveillance (1A)
site_visit_rate_1c <- (rep(params$mean_visit_rate, params$num_sites) * p_intro_establish ^ 3) / mean(p_intro_establish ^ 3)

## TODO: WHY IS THE SITE_REVISIT PARAMETER NOW TRUE?
results1c <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                       site_revisit = T,
                                       surveillance_period = params$num_years,
                                       site_visit_rate = site_visit_rate_1c,
                                       p_detection = params$det_prob,
                                       detection_dynamic = "constant",
                                       site_vector = site_vector,
                                       p_intro_establish = p_intro_establish)



## SCENARIO 1: (A, B, C) RESULTS -----------------------------------------------------------------
## TODO: sort this code out - output report?
par(mfrow=c(3,1))
hist(site_visit_rate_1a); hist(site_visit_rate_1b); hist(site_visit_rate_1c)
hist(results1a, breaks=10, freq = T); hist(results1b, breaks = 10, freq = T); hist(results1c, breaks = 10, freq = T)
plot(results1a); plot(results1b); plot(results1c)
summary(results1a); summary(results1b); summary(results1c)

plotProbability <- (0:(params$num_sim - 1) / params$num_sim - 1)

# line plot for results 1a, 1b and 1c
par(mfrow=c(1,1))
plot(jitter(sort(results1a)),
     plotProbability + 1, 
     type = 'l',
     xlim = c(0, params$num_years), # TODO: THIS CAN BE THE SURVEIILLANCE PERIOD?
     xlab = 'Time (years)',
     ylab = 'Probability of Detection',
     main = "Scenario 1: Constant rate of detection")
lines(sort(results1b), plotProbability + 1, col = 'red')
lines(sort(results1c), plotProbability + 1, col= 'blue')
legend("bottomright",
       c("random", "risk-based", "heavy risk-based"),
       col = c("black", "red", "blue"),
       cex = 0.6,
       pch = 19)


## SCENARIO 2: DETECTION INCREASES WITH TIME
## (TO MIMIC POPULATION EXPANSION FOLLOWING INTRODUCTION LEADING TO INCREASED DETECTION)
## Requires scaling of the detection probability so that it increases with time
## SCENARIO 2A: RANDOM SURVEILLANCE STRATEGY (independent of risk) -----------------------
# uses previously defined uniform site visit rate with uniform surveillance effort (visits - one per year)

# run simulation 2A to determine the number of years it takes to detect an introduction (num_sim simulations)
results2a <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                       site_revisit = F,
                                       surveillance_period = params$num_years,
                                       site_visit_rate = site_visit_rate_1a,
                                       p_detection = params$det_prob,
                                       detection_dynamic = "increasing",
                                       site_vector = site_vector,
                                       p_intro_establish = p_intro_establish)

# run simulation 2B to determine the number of years it takes to detect an introduction (num_sim simulations)
results2b <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                       site_revisit = F,
                                       surveillance_period = params$num_years,
                                       site_visit_rate = site_visit_rate_1b,
                                       p_detection = params$det_prob,
                                       detection_dynamic = "increasing",
                                       site_vector = site_vector,
                                       p_intro_establish = p_intro_establish)

# run simulation 2C to determine the number of years it takes to detect an introduction (num_sim simulations)
results2c <- runSurveillanceSimulation(n_simulations = params$num_sim,
                                       site_revisit = F,
                                       surveillance_period = params$num_years,
                                       site_visit_rate = site_visit_rate_1c,
                                       p_detection = params$det_prob,
                                       detection_dynamic = "increasing",
                                       site_vector = site_vector,
                                       p_intro_establish = p_intro_establish)


## SCENARIO 2: (A, B, C) RESULTS -----------------------------------------------------------------
## TODO: sort this code out - output report?
par(mfrow=c(3,1))
hist(site_visit_rate_2a); hist(site_visit_rate_2b); hist(site_visit_rate_2c)
hist(results2a, breaks=10, freq = T); hist(results2b, breaks = 10, freq = T); hist(results2c, breaks = 10, freq = T)
plot(results2a); plot(results2b); plot(results2c)
summary(results2a); summary(results2b); summary(results2c)

probability <- (0:(length(results2a) - 1) / length(results2a) - 1)
par(mfrow = c(1, 1))
plot(sort(results2a),
     probability + 1,
     type='l',
     xlim = c(0,10),
     xlab = 'Time (years)',
     ylab = 'Probability of Detection',
     main = "Scenario 2: Decreasing rate of detection")
lines(sort(results2b), probability + 1, col = 'red')
lines(sort(results2c), probability + 1, col = 'blue')
legend("bottomright",
       c("random", "risk-based", "heavy risk-based"),
       col = c("black", "red", "blue"),
       cex = 0.6,
       pch = 19)