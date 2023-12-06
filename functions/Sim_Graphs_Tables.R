# 26/07/2023
# Tom Gibson
# Sim_Graph_Tables
# Checked 06/12/23

# This plot takes the code from report-NIS-intro-detect-sim.Rmd 
# And makes it easier to export plots in .png and saved in a list to /Graphs and 
# 1 table of summary results to /Table. 

Sim_Graphs_Tables <- function(){

### 1. Create Graphs  

# Create a list to save the graphs into (in case we need to build more extensive_panel plots for a paper)
plot_ls <- vector(mode = "list", length = 13)
  
## 1.1 Introduction and establishment probabilities across sites

# generate data frame of probabilities
df_probs <- data.frame("p_establish" = p_establish,
                       "p_intro" = p_intro,
                       "p_intro_establish" = p_intro_establish)

# generate histograms of probabilities

# Probability of Establishment
p1 <- ggplot2::ggplot(df_probs, aes(x = p_establish)) +
  geom_histogram(binwidth = 0.05, colour = "black", fill = "gray") +
  theme_bw() +
  scale_x_continuous(name = "Probability of Establishment",
                     limits = c(0, 1)) +
  ylab("Frequency") +
  ggtitle("Probability of Establishment")

plot_ls[[1]] <- p1; names(plot_ls)[1] <- paste0(config$run_name, "_Prob_Estab") # save into list

# Probability of Introduction
p2 <- ggplot2::ggplot(df_probs, aes(x = p_intro)) +
  geom_histogram(binwidth = 0.05, colour = "black", fill = "gray") +
  theme_bw() +
  scale_x_continuous(name = "Probability of Introduction",
                     limits = c(0, 1)) +
  ylab("Frequency") +
  ggtitle("Probability of Introduction")

plot_ls[[2]] <- p2; names(plot_ls)[2] <- paste0(config$run_name, "_Prob_Intro") # save into list

# Probability of Establishment and Introduction
p3 <- ggplot2::ggplot(df_probs, aes(x = p_intro_establish)) +
  geom_histogram(binwidth = 0.05, colour = "black", fill = "gray") +
  theme_bw() +
  scale_x_continuous(name = "Probability",
                     limits = c(0, 1)) +
  ylab("Frequency") +
  ggtitle("Probability of Introduction and Establishment")

plot_ls[[3]] <- p3; names(plot_ls)[3] <- paste0(config$run_name, "_Prob_Intro_Estab") # save into list

# display plots side by side width = height * 3
png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Probabilities.png")),
    width = 35, height = (35/3), units = "cm", res = 300)

plot(p1 + p2 + p3)

dev.off()

## 1.2 Site-specific visit rate for each surveillance scenario

# generate data frame of site visit rates
# added plus 0.05 to xlim as it was cutting off site visit rates all at 1.0
df_visits <- data.frame("site_visit_rate_A" = site_visit_rate_A,
                        "site_visit_rate_B" = site_visit_rate_B,
                        "site_visit_rate_C" = site_visit_rate_C)

# Site Visit Rate Scenario Random
p4 <- ggplot2::ggplot(df_visits, aes(x = site_visit_rate_A)) +
  geom_histogram(binwidth = 0.05, colour = "gray", fill = "gray") +
  theme_bw() +
  scale_x_continuous(name = "Site Visit Rate (per year)",
                     limits = c(0, max(df_visits$site_visit_rate_C)+0.05)) +
  ylab("Frequency") +
  ggtitle("Random Surveillance")

plot_ls[[4]] <- p4; names(plot_ls)[4] <- paste0(config$run_name, "_Visit_Rate_Rdm_Surveil") # save into list

# Site Visit Rate Scenario Risk Based
p5 <- ggplot2::ggplot(df_visits, aes(x = site_visit_rate_B)) +
  geom_histogram(binwidth = 0.05, colour = "lightblue2", fill = "lightblue2") +
  theme_bw() +
  scale_x_continuous(name = "Site Visit Rate (per year)",
                     limits = c(0, max(df_visits$site_visit_rate_C)+0.05)) +
  ylab("Frequency") +
  ggtitle("Risk-based surveillance")

plot_ls[[5]] <- p5; names(plot_ls)[5] <- paste0(config$run_name, "_Visit_Rate_Rsk_Surveil") # save into list

# Site Visit Rate Scenario Heavy Risk Based
p6 <- ggplot2::ggplot(df_visits, aes(x = site_visit_rate_C)) +
  geom_histogram(binwidth = 0.05, colour = "royalblue3", fill = "royalblue3") +
  theme_bw() +
  scale_x_continuous(name = "Site Visit Rate (per year)",
                     limits = c(0, max(df_visits$site_visit_rate_C)+0.05)) +
  ylab("Frequency") +
  ggtitle("Heavy risk-based surveillance")

plot_ls[[6]] <- p6; names(plot_ls)[6] <- paste0(config$run_name, "_Visit_Rate_Hvy_Rsk_Surveil") # save into list

png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Visit_Rate.png")),
    width = 35, height = (35/3), units = "cm", res = 300)

plot(p4 + p5 + p6)

dev.off()

### 1.3 Time to detection histograms for each simulation

# generate data frame of site visit rates
df_results <- data.frame("resultsA" = resultsA_dt,
                         "resultsB" = resultsB_dt,
                         "resultsC" = resultsC_dt)

# Random Surveillance
p7 <- ggplot2::ggplot(df_results, aes(x = resultsA)) +
  geom_histogram(binwidth = 0.05, colour = "gray", fill = "gray") +
  theme_bw() +
  ylab("Frequency") +
  ggtitle("Random surveillance") +
  scale_x_continuous(name = "Time to Detection (Years)",
                     limits = c(0, 101))

plot_ls[[7]] <- p7; names(plot_ls)[7] <- paste0(config$run_name, "_T_Detect_Hst_Rdm_Surveil") # save into list

# Risk Based Surveillance
p8 <- ggplot2::ggplot(df_results, aes(x = resultsB)) +
  geom_histogram(binwidth = 0.05, colour = "lightblue2", fill = "lightblue2") +
  theme_bw() +
  ylab("Frequency") +
  ggtitle("Risk-based surveillance") +
  scale_x_continuous(name = "Time to Detection (Years)",
                     limits = c(0, 101))

plot_ls[[8]] <- p8; names(plot_ls)[8] <- paste0(config$run_name, "_T_Detect_Hst_Rsk_Surveil") # save into list

# Heavy Risk Based Surveillance
p9 <- ggplot2::ggplot(df_results, aes(x = resultsC)) +
  geom_histogram(binwidth = 0.05, colour = "royalblue3", fill = "royalblue3") +
  theme_bw() +
  ylab("Frequency") +
  ggtitle("Heavy risk-based surveillance") +
  scale_x_continuous(name = "Time to Detection (Years)",
                     limits = c(0, 101))

plot_ls[[9]] <- p9; names(plot_ls)[9] <- paste0(config$run_name, "_T_Detect_Hst_Hvy_Rsk_Surveil") # save into list

png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Time_Detection_hist.png")),
    width = 35, height = (35/3), units = "cm", res = 300)

plot(p7 + p8 + p9)

dev.off()

### 1.4 Time to detection scatter plots for each simulation

# Random Surveillance
p10 <- ggplot2::ggplot(df_results, aes(x = 1:config$num_sim, y = resultsA)) +
  geom_point(color = "gray", shape = 1) +
  theme_bw() +
  xlab("Simulation") +
  scale_y_continuous(name = "Time to detection (Years)",
                     limits = c(0, 101)) +
  ggtitle("Random surveillance")

plot_ls[[10]] <- p10; names(plot_ls)[10] <- paste0(config$run_name, "_T_Detect_Pnt_Rdm_Surveil") # save into list

# Risk based surveillance
p11 <- ggplot2::ggplot(df_results, aes(x = 1:config$num_sim, y = resultsB)) +
  geom_point(color = "lightblue2", shape = 1) +
  theme_bw() +
  xlab("Simulation") +
  scale_y_continuous(name = "Time to detection (Years)",
                     limits = c(0, 101)) +
  ggtitle("Risk-based surveillance")

plot_ls[[11]] <- p11; names(plot_ls)[11] <- paste0(config$run_name, "_T_Detect_Pnt_Rsk_Surveil") # save into list

# Heavy Risk based surveillance
p12 <- ggplot2::ggplot(df_results, aes(x = 1:config$num_sim, y = resultsC)) +
  geom_point(color = "royalblue3", shape = 1) +
  theme_bw() +
  xlab("Simulation") +
  scale_y_continuous(name = "Time to detection (Years)",
                     limits = c(0, 101)) +
  ggtitle("Heavy risk-based surveillance")

plot_ls[[12]] <- p12; names(plot_ls)[12] <- paste0(config$run_name, "_T_Detect_Pnt_Hvy_Rsk_Surveil") # save into list

png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Time_Detection_plot.png")),
    width = 35, height = (35/3), units = "cm", res = 300)

plot(p10 + p11 + p12)

dev.off()

### 1.5 Probability of NIS Detection Over Time Depending on Surveillance Method

df_scenarios <- data.frame("probability" = rep((0:(config$num_sim - 1)
                                                / config$num_sim - 1) + 1,
                                               times = 3),
                           "results" = c(sort(resultsA_dt),
                                         sort(resultsB_dt),
                                         sort(resultsC_dt)),
                           "scenario" = rep(c("Random", "Risk-based", "Heavy risk-based"),
                                            each = config$num_sim))

df_scenarios$scenario <- factor(df_scenarios$scenario,
                                levels = c("Random",
                                           "Risk-based",
                                           "Heavy risk-based"))

# save this
write.csv(x = df_scenarios, file = file.path("outputs", config$run_name, "Tables", paste0(config$run_name, "_Survey_Detection_Probability.csv")))

p13 <- ggplot2::ggplot(df_scenarios, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), size = 1.25) +
  theme_bw() +
  ggtitle("Probability of NIS Detection Through Time") +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, config$num_years),
                     breaks = c(0:config$num_years)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("gray", "lightblue2", "royalblue3"))

plot_ls[[13]] <- p13; names(plot_ls)[13] <- paste0(config$run_name, "_Detec_Prob") # save into list

png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Detection_Probability.png")),
    width = 25, height = 20, units = "cm", res = 300)

plot(p13)

dev.off()

# Save the plot list
save(x = plot_ls, file = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Graphs_ls.RData")))

### Create Tables

resultsA_df <- data.frame("results" = resultsA_dt, "scenario" = rep("resultsA_dt", length(resultsA_dt)))
resultsB_df <- data.frame("results" = resultsB_dt, "scenario" = rep("resultsB_dt", length(resultsB_dt)))
resultsC_df <- data.frame("results" = resultsC_dt, "scenario" = rep("resultsC_dt", length(resultsC_dt)))

results_df <- rbind(resultsA_df, resultsB_df, resultsC_df)

# Generate Summary Statistics
results_df[!results_df$results == 1000,] %>% group_by(scenario) %>% summarise(Mean.Result = mean(results),
                                                 Median.Result = median(results),
                                                 Min.Result = min(results),
                                                 Max.Result = max(results),
                                                 IQR.Result = IQR(results)) -> results_df_sum 

# Create the Percentage no detection plots 

PND_resultsA <- (length(resultsA_df$results[resultsA_df$results == 1000])/length(resultsA_df$results))*100
PND_resultsB <- (length(resultsB_df$results[resultsB_df$results == 1000])/length(resultsB_df$results))*100
PND_resultsC <- (length(resultsC_df$results[resultsC_df$results == 1000])/length(resultsC_df$results))*100

results_df_sum$Pnct.No.Detect <- c(PND_resultsA, PND_resultsB, PND_resultsC)

write.csv(x = results_df_sum, file = file.path("outputs", config$run_name, "Tables", paste0(config$run_name, "_Summary_Results.csv")))

}

