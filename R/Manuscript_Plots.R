# 22/09/2023
# Manuscript Plots
# Tom Gibson
# Checked 05/01/24

## LOAD FUNCTIONS/PACKAGES -----------------------------------------------------------

pkgs <- c("yaml", "here", "truncnorm", "reshape2", "gtools", "cowplot",
          "ggplot2", "patchwork", "EnvStats", "ReIns", "data.table", "dplyr")
lapply(pkgs, library, character.only = T)

## INPUTS ----------------------------------------------------------------------------
# load input parameters from config file
dir.create(file.path("outputs/Manuscript/Graphs"), recursive = T)

### Load the Graphs from Simulation 1 to 3. 

load(file.path("outputs/Sim_1/Graphs/Sim_1_Graphs_ls.RData"))
Sim_1 <- plot_ls; rm(plot_ls)

load(file.path("outputs/Sim_2/Graphs/Sim_2_Graphs_ls.RData"))
Sim_2 <- plot_ls; rm(plot_ls)

load(file.path("outputs/Sim_3/Graphs/Sim_3_Graphs_ls.RData"))
Sim_3 <- plot_ls; rm(plot_ls)

### Create the Distribution Graphs from Simulation 1 to 3. 
S1_IE <- Sim_1$Sim_1_Prob_Intro_Estab
S2_IE <- Sim_2$Sim_2_Prob_Intro_Estab
S3_IE <- Sim_3$Sim_3_Prob_Intro_Estab

S1_IE <- S1_IE + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
S1_IE <- S1_IE + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))
S1_IE <- S1_IE + ggtitle("")

S2_IE <- S2_IE + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
S2_IE <- S2_IE + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))
S2_IE <- S2_IE + ggtitle("")

S3_IE <- S3_IE + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
S3_IE <- S3_IE + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))
S3_IE <- S3_IE + ggtitle("")

Panel_1 <- plot_grid(S1_IE, S2_IE, S3_IE, ncol = 2, 
                     labels = c("A", "B", "C"))

png(filename = "outputs/Manuscript/Graphs/Risk_Distribution_Sim_1_3.png", 
    width = 20, height = 20, units = "cm", res = 300) 

plot(Panel_1) 

dev.off() 

### Create the Detection Probability Graphs from Simulation 1 to 3. 

# Extract the Plots Elements of Interest
S1_DP_Dat <- read.csv(file = file.path("outputs", "Sim_1", "Tables", paste0("Sim_1", "_Survey_Detection_Probability.csv")))
S2_DP_Dat <- read.csv(file = file.path("outputs", "Sim_2", "Tables", paste0("Sim_2", "_Survey_Detection_Probability.csv")))
S3_DP_Dat <- read.csv(file = file.path("outputs", "Sim_3", "Tables", paste0("Sim_3", "_Survey_Detection_Probability.csv")))

# save this

# Sim 1 - Exponential Uniform
S1_DP <- ggplot2::ggplot(S1_DP_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))

# Sim 2 - Random Uniform
S2_DP <- ggplot2::ggplot(S2_DP_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))

# Sim 3 - Equal Uniform
S3_DP <- ggplot2::ggplot(S3_DP_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
DP_Leg <- get_legend(S3_DP + theme(legend.box.margin = margin(t = -5.5, r = 0, b = 0, l = -7.25, unit = "cm")))
S3_DP <- S3_DP + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Create Panel Plots
Panel_2 <- plot_grid(S1_DP, S2_DP, S3_DP, DP_Leg, label_x = 0.005,
                     ncol = 2, labels = c("A", "B", "C"))

# Export Panel Plot

png(filename = "outputs/Manuscript/Graphs/Detection_Probability_Sim_1_3.png", 
    width = 28, height = 20, units = "cm", res = 300) 

plot(Panel_2)

dev.off() 

### Create Sensitivity Maps

# Load Sensitivity Data
load(file.path("outputs", "Sim_1", "Tables", "Sim_1_Sensitivity_Data.RData"))

Sens_Dat <- df_factors_all; rm(df_factors_all)

# Reformatting the Data 
seed_n_df <- list(Sens_Dat$a_random$seed_n, Sens_Dat$b_risk_based$seed_n, Sens_Dat$c_heavy_risk_based$seed_n)
names(seed_n_df) <- c("a_random", "b_risk_based", "c_heavy_risk_based")
seed_n_df <- rbindlist(seed_n_df, idcol = T)

num_sites_df <- list(Sens_Dat$a_random$num_sites, Sens_Dat$b_risk_based$num_sites, Sens_Dat$c_heavy_risk_based$num_sites)
names(num_sites_df) <- c("a_random", "b_risk_based", "c_heavy_risk_based")
num_sites_df <- rbindlist(num_sites_df, idcol = T)

num_years_df <- list(Sens_Dat$a_random$num_years, Sens_Dat$b_risk_based$num_years, Sens_Dat$c_heavy_risk_based$num_years)
names(num_years_df) <- c("a_random", "b_risk_based", "c_heavy_risk_based")
num_years_df <- rbindlist(num_years_df, idcol = T)

mean_visit_rate_df <- list(Sens_Dat$a_random$mean_visit_rate, Sens_Dat$b_risk_based$mean_visit_rate, Sens_Dat$c_heavy_risk_based$mean_visit_rate)
names(mean_visit_rate_df) <- c("a_random", "b_risk_based", "c_heavy_risk_based")
mean_visit_rate_df <- rbindlist(mean_visit_rate_df, idcol = T)

p_detection_df <- list(Sens_Dat$a_random$p_detection, Sens_Dat$b_risk_based$p_detection, Sens_Dat$c_heavy_risk_based$p_detection)
names(p_detection_df) <- c("a_random", "b_risk_based", "c_heavy_risk_based")
p_detection_df <- rbindlist(p_detection_df, idcol = T)

## Creating the Sensitivity Plots for Median Time

# Seed_N Plots
seed_n_pl <- ggplot(seed_n_df, aes(x = name, y = value, fill = .id)) + geom_boxplot(outlier.shape = 19, outlier.size = 1, outlier.alpha = 0.5)
seed_n_pl <- seed_n_pl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                  axis.text = element_text(size = 10))
seed_n_pl <- seed_n_pl + scale_y_continuous(name = "Time to detection (years)", limits = c(0, 30), breaks = seq(0, 30, 5))
seed_n_pl <- seed_n_pl + scale_x_discrete(name = "Number of Seed Sites", labels = as.character(c(1, seq(10, 100, 10))))
seed_n_pl <- seed_n_pl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
seed_n_pl

# num_sites_df Plots
num_sites_pl <- ggplot(num_sites_df, aes(x = name, y = value, fill = .id)) + geom_boxplot(outlier.shape = 19, outlier.size = 1, outlier.alpha = 0.5)
num_sites_pl <- num_sites_pl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                            axis.text = element_text(size = 10))
num_sites_pl <- num_sites_pl + scale_y_continuous(name = "Time to detection (years)", limits = c(0, 30), breaks = seq(0, 30, 5))
num_sites_pl <- num_sites_pl + scale_x_discrete(name = "Number of Sites", labels = as.character(seq(50, 200, 25)))
num_sites_pl <- num_sites_pl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
num_sites_pl

# num_years_df Plots 
num_years_pl <- ggplot(num_years_df, aes(x = name, y = value, fill = .id)) + geom_boxplot(outlier.shape = 19, outlier.size = 1, outlier.alpha = 0.5)
num_years_pl <- num_years_pl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                            axis.text = element_text(size = 10))
num_years_pl <- num_years_pl + scale_y_continuous(name = "Time to detection (years)", limits = c(0, 30), breaks = seq(0, 30, 5))
num_years_pl <- num_years_pl + scale_x_discrete(name = "Number of Years", labels = as.character(seq(10, 50, 5)))
num_years_pl <- num_years_pl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
num_years_pl

# mean_visit_rate_df 
visit_rate_pl <- ggplot(mean_visit_rate_df, aes(x = name, y = value, fill = .id)) + geom_boxplot(outlier.shape = 19, outlier.size = 1, outlier.alpha = 0.5)
visit_rate_pl <- visit_rate_pl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                            axis.text = element_text(size = 10))
visit_rate_pl <- visit_rate_pl + scale_y_continuous(name = "Time to detection (years)", limits = c(0, 30), breaks = seq(0, 30, 5))
visit_rate_pl <- visit_rate_pl + scale_x_discrete(name = "Mean Visit Rate (per Year)", labels = as.character(seq(0.25, 4, 0.25)))
visit_rate_pl <- visit_rate_pl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
visit_rate_pl

# p_detection_df
p_detect_pl <- ggplot(p_detection_df, aes(x = name, y = value, fill = .id)) + geom_boxplot(outlier.shape = 19, outlier.size = 1, outlier.alpha = 0.5)
p_detect_pl <- p_detect_pl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                            axis.text = element_text(size = 10))
p_detect_pl <- p_detect_pl + scale_y_continuous(name = "Time to detection (years)", limits = c(0, 30), breaks = seq(0, 30, 5))
p_detect_pl <- p_detect_pl + scale_x_discrete(name = "Detection Probability", labels = as.character(seq(0.1, 1, 0.1)))
p_detect_pl <- p_detect_pl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
p_detect_pl

### Percentage Plots

# Seed_N

seed_n_df2 <- seed_n_df %>% group_by(.id, name) %>% summarise(pct_no_detect = unique(pct_no_detect))

seed_n_bl <- ggplot(seed_n_df2, aes(x = name, y = pct_no_detect, fill = .id))
seed_n_bl <- seed_n_bl + geom_col(stat = 'identity', position = position_dodge(), color = "black")
seed_n_bl <- seed_n_bl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                            axis.text = element_text(size = 10))
seed_n_bl <- seed_n_bl + scale_y_continuous(name = "Number of Detection Failures (%)", limits = c(0, 100), breaks = seq(0, 100, 20))
seed_n_bl <- seed_n_bl + scale_x_discrete(name = "Number of Seed Sites", labels = as.character(c(1, seq(10, 100, 10))))
seed_n_bl <- seed_n_bl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
seed_n_bl

# Number of Sites

num_sites_df2 <- num_sites_df %>% group_by(.id, name) %>% summarise(pct_no_detect = unique(pct_no_detect))

num_sites_bl <- ggplot(num_sites_df2, aes(x = name, y = pct_no_detect, fill = .id))
num_sites_bl <- num_sites_bl + geom_col(stat = 'identity', position = position_dodge(), color = "black")
num_sites_bl <- num_sites_bl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                            axis.text = element_text(size = 10))
num_sites_bl <- num_sites_bl + scale_y_continuous(name = "Number of Detection Failures (%)", limits = c(0, 100), breaks = seq(0, 100, 20))
num_sites_bl <- num_sites_bl + scale_x_discrete(name = "Number of Sites", labels = as.character(seq(50, 200, 25)))
num_sites_bl <- num_sites_bl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
num_sites_bl

# Number of Years

num_years_df2 <- num_years_df %>% group_by(.id, name) %>% summarise(pct_no_detect = unique(pct_no_detect))

num_years_bl <- ggplot(num_years_df2, aes(x = name, y = pct_no_detect, fill = .id))
num_years_bl <- num_years_bl + geom_col(stat = 'identity', position = position_dodge(), color = "black")
num_years_bl <- num_years_bl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                  axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                                  axis.text = element_text(size = 10))
num_years_bl <- num_years_bl + scale_y_continuous(name = "Number of Detection Failures (%)", limits = c(0, 100), breaks = seq(0, 100, 20))
num_years_bl <- num_years_bl + scale_x_discrete(name = "Number of Years", labels = as.character(seq(10, 50, 5)))
num_years_bl <- num_years_bl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
num_years_bl

# Visit Rate 

visit_rate_df2 <- mean_visit_rate_df %>% group_by(.id, name) %>% summarise(pct_no_detect = unique(pct_no_detect))

visit_rate_bl <- ggplot(visit_rate_df2, aes(x = name, y = pct_no_detect, fill = .id))
visit_rate_bl <- visit_rate_bl + geom_col(stat = 'identity', position = position_dodge(), color = "black")
visit_rate_bl <- visit_rate_bl + theme_bw() + theme(legend.position = "none", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                  axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                                  axis.text = element_text(size = 10))
visit_rate_bl <- visit_rate_bl + scale_y_continuous(name = "Number of Detection Failures (%)", limits = c(0, 100), breaks = seq(0, 100, 20))
visit_rate_bl <- visit_rate_bl + scale_x_discrete(name = "Mean Visit Rate (per Year)", labels = as.character(seq(0.25, 4, 0.25)))
visit_rate_bl <- visit_rate_bl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
visit_rate_bl

# Probability of Detection

p_detection_df2 <- p_detection_df %>% group_by(.id, name) %>% summarise(pct_no_detect = unique(pct_no_detect))

p_detect_bl <- ggplot(p_detection_df2, aes(x = name, y = pct_no_detect, fill = .id))
p_detect_bl <- p_detect_bl + geom_col(stat = 'identity', position = position_dodge(), color = "black")
p_detect_bl <- p_detect_bl + theme_bw() + theme(legend.position = "right", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                    axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.5),
                                                axis.text = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))
p_detect_bl <- p_detect_bl + scale_y_continuous(name = "Number of Detection Failures (%)", limits = c(0, 100), breaks = seq(0, 100, 20))
p_detect_bl <- p_detect_bl + scale_x_discrete(name = "Detection Probability", labels = as.character(seq(0.1, 1, 0.1)))
p_detect_bl <- p_detect_bl + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
Panel_3_Leg <- get_legend(p_detect_bl + theme(legend.box.margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "cm")))
p_detect_bl <- p_detect_bl + theme(legend.position = "none")
p_detect_bl

### Create the Sensitivity Panel Plots

Panel_3 <- plot_grid(seed_n_pl, seed_n_bl, Panel_3_Leg, num_sites_pl, num_sites_bl, NULL, num_years_pl, num_years_bl, NULL, visit_rate_pl,
                     visit_rate_bl, NULL, p_detect_pl, p_detect_bl, NULL, ncol = 3, byrow = T, 
                     labels = c("A ", "", "", "B ", "", "", "C ", "", "", "D ", "", "", "E "),
                     rel_widths = c(rep(c(1,1,0.25), 5)), label_x = -0.01)

png(filename = "outputs/Manuscript/Graphs/Sensitivity_Simulation_1.png", 
    width = 36, height = 50, units = "cm", res = 300) 

plot(Panel_3) 

dev.off() 

### Create Elasticity Analysis

# Load in the Elasticity Data

load(file.path("outputs", "Sim_1", "Tables", paste0("Sim_1", "_Elasticity_Data.RData")))

surveillance <- names(elasticity_dfs)

# for each of the surveillance strategy results
elasticity_dfs1 <- lapply(setNames(surveillance, surveillance), function(x) {
  
  lapply(names(elasticity_dfs[[x]]), function(y) {
    
    # place row names in column and reset row names
    new_df <- data.frame(names = row.names(elasticity_dfs[[x]][[y]]), elasticity_dfs[[x]][[y]], row.names = NULL)
    new_df <- new_df[,c("names", "median", "pct_no_detect", "direction")]
    
  }) })

elasticity_dfs2 <- lapply(setNames(surveillance, surveillance), function(x) {
  # combine these data frames
  elasticity_df <- rbind(elasticity_dfs1[[x]][[1]], elasticity_dfs1[[x]][[2]])
  df_long <- reshape2::melt(elasticity_df, id.vars = c("names", "direction"))
  
  # convert columns to factor
  cols <- c("variable", "direction", "names")
  df_long[cols] <- lapply(df_long[cols], factor)
  
  # make elasticity values positive
  df_long$value <- abs(df_long$value)
  
  return(df_long)})

# Sort dplyr
elas <- rbindlist(elasticity_dfs2, idcol = T)
elas_med <- elas[elas$variable == "median"]
elas_ct <- elas[elas$variable == "pct_no_detect"]

# Median Elasticity Values
elas_p1 <- ggplot(data = elas_med, aes(x = names, y = value, fill = .id, shape = direction))
elas_p1 <- elas_p1 + geom_point(size = 3) + theme_bw() + geom_hline(yintercept = 1, color = "dark gray") 
elas_p1 <- elas_p1 + theme(legend.position = "right", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.25))
elas_p1 <- elas_p1 + ylab("Elasticity") + xlab("Parameter")
elas_p1 <- elas_p1 + scale_shape_manual(name = "Elasticity Direction",values = c(24, 25))
elas_p1 <- elas_p1 + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
elas_p1 <- elas_p1 + guides(fill = guide_legend(override.aes = list(shape = 24)))
elas_p1 <- elas_p1 + scale_y_continuous(name = "Elasticity", limits = c(-0.25, 2.75), breaks = seq(-1, 3, 0.5))
elas_p1 <- elas_p1 + scale_x_discrete(name = "Parameter", labels = c("Mean Visit Rate", "Number of Sites", "Number of Years", "Detection Probability"))
Panel_4_Leg <- get_legend(elas_p1 + theme(legend.box.margin = margin(t = 0.6, r = 0, b = 0, l = 0, unit = "cm")))
elas_p1 <- elas_p1 + theme(legend.position = "none")
elas_p1

# Percentage Values
elas_p2 <- ggplot(data = elas_ct, aes(x = names, y = value, fill = .id, shape = direction))
elas_p2 <- elas_p2 + geom_point(size = 3) + theme_bw() + geom_hline(yintercept = 1, color = "dark gray") 
elas_p2 <- elas_p2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 1.25))
elas_p2 <- elas_p2 + ylab("Elasticity") + xlab("Parameter")
elas_p2 <- elas_p2 + scale_shape_manual(name = "Elasticity Direction",values = c(24, 25))
elas_p2 <- elas_p2 + scale_fill_manual(name = "Surveillance Scenario", values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))
elas_p2 <- elas_p2 + guides(fill = guide_legend(override.aes = list(shape = 24)))
elas_p2 <- elas_p2 + scale_y_continuous(name = "Elasticity", limits = c(-0.25, 2.75), breaks = seq(-1, 3, 0.5))
elas_p2 <- elas_p2 + scale_x_discrete(name = "Parameter", labels = c("Mean Visit Rate", "Number of Sites", "Number of Years", "Detection Probability"))
elas_p2 <- elas_p2 + theme(legend.position = "none")
elas_p2

# Create Panel Plot 4! 

Panel_4 <- plot_grid(elas_p1, elas_p2, Panel_4_Leg,
                     visit_rate_bl, NULL, p_detect_pl, p_detect_bl, NULL, ncol = 3, nrow = 1, 
                     labels = c("A", "B"), rel_widths = c(1,1,0.3), label_x = 0.005)

png(filename = "outputs/Manuscript/Graphs/Elasticity_Sim_1.png", 
    width = 32, height = 14, units = "cm", res = 300) 

plot(Panel_4) 

dev.off() 

### Load in the Abundance Comparison Plots... 

### Load the Graphs from Simulation 1 to 3. 

### Create the Distribution Graphs from Simulation 1 to 3. 
S4_IE_Dat <- read.csv(file = file.path("outputs", "Sim_4", "Tables", paste0("Sim_4", "_Survey_Detection_Probability.csv")))
S5_IE_Dat <- read.csv(file = file.path("outputs", "Sim_5", "Tables", paste0("Sim_5", "_Survey_Detection_Probability.csv")))
S6_IE_Dat <- read.csv(file = file.path("outputs", "Sim_6", "Tables", paste0("Sim_6", "_Survey_Detection_Probability.csv")))
S7_IE_Dat <- read.csv(file = file.path("outputs", "Sim_7", "Tables", paste0("Sim_7", "_Survey_Detection_Probability.csv")))

S4_IE <- ggplot2::ggplot(S4_IE_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))

S5_IE <- ggplot2::ggplot(S5_IE_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))

S6_IE <- ggplot2::ggplot(S6_IE_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))

S7_IE <- ggplot2::ggplot(S7_IE_Dat, aes(results, y = probability, group = scenario)) +
  geom_line(aes(color = scenario), linewidth = 1.25) +
  theme_bw() + theme(legend.position = "right", legend.justification = "top", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = -0.25),
                     axis.text = element_text(size = 10), legend.text = element_text(size = 10), legend.title = element_text(size = 10)) +
  scale_x_continuous(name = "Time (Years)",
                     limits = c(0, 30),
                     breaks = seq(0,30,1),
                     labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30)) +
  scale_y_continuous(name = "Probability of Detection\n",
                     limits = c(0, 1)) +
  guides(color = guide_legend("Surveillance Scenario")) +
  scale_color_manual(values = c("goldenrod1", "#7fcdbb", "#2c7fb8"), labels = c("Random", "Risk-Based", "Heavy Risk-Based"))

Panel_5_Leg <- get_legend(S7_IE + theme(legend.box.margin = margin(t = 0.6, r = 0, b = 0, l = 0, unit = "cm")))
S7_IE <- S7_IE + theme(legend.position = "none")

Panel_5 <- plot_grid(S4_IE, S5_IE, Panel_5_Leg, S6_IE, S7_IE, ncol = 3, 
                     labels = c("A", "B", "", "C", "D"), rel_widths = c(1,1,0.3), label_x = 0.005)

png(filename = "outputs/Manuscript/Graphs/Probability_Distribution_Sim_4_7.png", 
    width = 34, height = 20, units = "cm", res = 300) 

plot(Panel_5) 

dev.off() 

### End of additional plots
