# Simulator to evaluate time to detection based on a variety of surveillance strategies
# (assuming no spread from initial intro site)

## LOAD FUNCTIONS --------------------------------------------------------------------
source("functions/getEstablishProbability.R")
source("functions/getIntroAndEstablishProbability.R")
source("functions/runSurveillanceSimulation.R")


## INPUTS ----------------------------------------------------------------------------
# number of sites to survey
num_sites <- 100

# define distributions to use for introduction and establishment probabilities
# establishment is either: (1) 'random uniform' OR (2) 'equal uniform' (case insensitive)
# introduction is either: (1) 'random uniform' OR (2) 'positive normal' (case insensitive)
establish_risk <- "equal uniform"
intro_risk <- "positive normal" # for normal majority of sites are intermediate risk

# equal uniform probability of establishment
establish_prob <- 0.8

# frequency of site visits (e.g. 0.5 = once every 2 years)
mean_visit_rate <- 1 # sites visited once per year

# number of simulations to run
num_sim <- 1000

# number of years that sites are visited
num_years <- 11

# mean, minimum and maximum detection probability
det_prob <- 0.9 # assume that introduction will be detected


## CREATE SIMULATION INPUTS -----------------------------------------------------------
# create vector of sites
site_vector <- 1:num_sites

# probability of establishment
p_establish <- getEstablishProbability(method = establish_risk, n_sites = num_sites, x = establish_prob)

# probability of introduction AND establishment
# this function defines the introduction rates using intro_risk defined above
# introduction rate is then combined with rate of establishment to give overall introduction rate
p_intro_establish <- getIntroAndEstablishProbability(method = intro_risk, n_sites = num_sites, p_establish = p_establish)


## SCENARIO 1: ASSUME CONSTANT PROBABILITY OF DETECTING NIS INTRODUCTION THROUGH TIME ----
## SCENARIO 1A: RANDOM SURVEILLANCE STRATEGY (independent of risk) -----------------------
# 1A: rate at which random sites are visited (vector)
site_visit_rate_1a <- rep(mean_visit_rate, length(site_vector)) # each site visited once (mean_visit_rate = 1)

# run simulation 1A to determine the number of years which is takes to detect an introduction (1000 simulations in total)
results1a <- runSurveillanceSimulation(n_simulations = num_sim,
                                       site_revisit = F,
                                       surveillance_period = num_years,
                                       site_visit_rate = site_visit_rate_1a,
                                       p_detection = det_prob)


## SCENARIO 1B: RISK BASED SURVEILLANCE FOCUSSED ON HIGH RISK SITES ----------------------
# CONSTANT DETECTION WITH SAME OVERALL SITE VISITS
# ESTABLISH RISK: equal uniform distribution with establish_prob
## TODO: SITE AND ESTABLISH RISK THE SAME AS PREVIOUS - DO THESE NEED TO BE EDITABLE?
site_visit_rate_1b <- rep(mean_visit_rate, num_sites) * p_intro_establish / mean(p_intro_establish)

results1b <- runSurveillanceSimulation(n_simulations = num_sim,
                                       site_revisit = F,
                                       surveillance_period = num_years,
                                       site_visit_rate = site_visit_rate_1b,
                                       p_detection = det_prob)


## SCENARIO 1C: RISK BASED SURVEILLANCE VERY FOCUSSED ON HIGH RISK SITES -----------------
# site visit rate with heavy focus on high risk sites
# note overall number of sites visits are the same as for the random surveillance (1A)
site_visit_rate_1c <- (rep(mean_visit_rate, num_sites) * p_intro_establish ^ 3) / mean(p_intro_establish ^ 3)

## TODO: WHY IS THE SITE_REVISIT PARAMETER NOW TRUE?
results1c <- runSurveillanceSimulation(n_simulations = num_sim,
                                       site_revisit = T,
                                       surveillance_period = num_years,
                                       site_visit_rate = site_visit_rate_1c,
                                       p_detection = det_prob)



results1a# this is the last result of the simulation, so 1000 results in all. 
#plot(results1a)
summary(results1a)
hist(results1a, breaks=10)
Probability1random <- (0:(length(results1a)-1)/length(results1a)-1)
plot(jitter(sort(results1a)),Probability1random+1,type='l', xlim=c(0,10), xlab='Time (years)', ylab='Probability of Detection', main="constant rate of detection")


summary(results1b)
#hist(results, freq=T)
Probabilityrisk1 <- (0:(length(results1b)-1)/length(results1b)-1)
lines(sort(results1b),Probabilityrisk1+1,col='red')


summary(results1ba)
#hist(results, freq=T)
Probabilityrisk2 <- (0:(length(results1ba)-1)/length(results1ba)-1)
lines(sort(results1ba),Probabilityrisk2+1,col='green')

legend("bottomright", c("random", "risk-based", "heavy risk-based"), col=c("black", "red", "green"), cex=0.6,pch=19)




####################################################################################################
#2. dectection increases with time - this could mimic a situation where the population expands over time following introduction and therefore becomes easier to detect as time incerase.
####################################################################################################
#(this requires scaling of detectio probability so that it increases with time)
#a) non risk based
#b)risk based
#c)heavy risk based
#######################################################################################################

det_prob <- 1 #mean detection probability
# a) non risk based - radom surveillance approach

random_site_visit_rate_vector 		#Uniform surv effort (rate of visits), presently assumes 1 visit per year

resultsrandom2 <- numeric(1000)
for(i in 1:length(resultsrandom2))
{
  time <- 0 #set start time to 0
  inf_site <- sample(site_vector, 1, replace=T,prob=(p_intro_establish)/sum(p_intro_establish)) #select site to be infected
  
  #loop through site visit is until the infected site is detected
  
  while(resultsrandom2[i]==0 && time<11)
  {
    new_time <- rexp(1,sum(site_visit_rate_vector))
    time <- time+new_time
    visit <- sample(site_vector,1, replace=T,prob=(site_visit_rate_vector)/sum(site_visit_rate_vector)) #to determine site visited NOTE REPLACE SET to F NOT T as it is in next sim
    
    #detect <- (rbinom(1,1,det_prob)) #return a 0/1 based on p of detection
    
    scaling <- ifelse(exp(-time)>det_prob_max,det_prob_max,exp(-time)) #ensure p of detection does not exceed max limit
    scaling <- ifelse(scaling<det_prob_min,det_prob_min,scaling) #ensure p of detection does not go below min limit
    
    detect <- (rbinom(1,1,det_prob-scaling)) #return a 0/1 based on p of detection which increases with time
    #detect <- (rbinom(1,1,det_prob-(1-scaling))) #return a 0/1 based on p of detection which decreases with time
    
    resultsrandom2[i] <- ifelse(visit==inf_site & detect==1,time,0)
  }	
  resultsrandom2[i] <- ifelse(resultsrandom2[i]==0,100,resultsrandom2[i])
}

summary(resultsrandom2)
#hist(resultsrandom2, freq=T)
Probability <- (0:(length(resultsrandom2)-1)/length(resultsrandom2)-1)
plot(sort(resultsrandom2),Probability+1,type='l', xlim=c(0,10), xlab='Time (years)', ylab='Probability of Detection')