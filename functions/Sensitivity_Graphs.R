# 27/07/23
# Tom Gibson
# Sensitivity_Graphs
# Checked 01/08/23

# This plot takes the code from report-NIS-intro-detect-sensitivity.Rmd 
# And makes it easier to export plots in .png and saved in a list to /Graphs and 
# 1 table of summary results to /Table. 
# This needs a function check. 

Sensitivity_Graphs <- function(){

# Create a plot list to save into
plot_ls <- vector(mode = "list", length = length(df_factors_all)) # create list to save into 
names(plot_ls) <- names(df_factors_all)
  
# produce plots for each sensitivity factor tested 
for(i in 1:length(df_factors_all)){
  
  w <- names(df_factors_all)[i]
  
  df_factors <- df_factors_all[[w]]
  
  plot_ls[[w]] <- vector(mode = "list", length = length(factors)*2) # create space
  names(plot_ls[[w]]) <- c(paste0(rep(factors), "_T_detect"), paste0(rep(factors), "_Pct_No_Detect"))

  # loop over each scenario
  for(i in 1:length(factors)){
    
    # Extract the bit you want
    x <- factors[i]
    
    # violin plot - time taken to detect NIS where detection != 1000
    p1 <- ggplot2::ggplot(df_factors[[x]],
                          aes(x = name, y = value, fill = name)) +
      geom_violin() +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_x_discrete(name = x,
                       labels = c(gsub(
                         pattern = paste0(factors[x], "_"),
                         replacement = "",
                         x = unique(df_factors[[x]][["name"]])
                       ))) +
      scale_y_continuous(name = "Time to detection (years)", limits = c(0, config$num_years)) +
      labs(title = paste(w),
           subtitle = paste("Sensivitiy analysis: ", x)) + 
      stat_summary(fun.y = median, geom = "point", size = 1.5, shape = 21, fill = "white")
    
    plot_ls[[w]][[paste0(x, "_T_detect")]] <- p1 # _T_detect
    
    # bar plot - percentage of simulations where NIS not detected
    p2 <- ggplot(df_factors[[x]],
                 aes(
                   x = name,
                   y = pct_no_detect / config$num_sim,
                   fill = name
                 )) + # fill=name allow to automatically dedicate a color for each group
      geom_bar(stat = 'identity') +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_x_discrete(name = x,
                       labels = c(gsub(
                         pattern = paste0(factors[x], "_"),
                         replacement = "",
                         x = unique(df_factors[[x]][["name"]])
                       ))) +
      scale_y_continuous(name = "Simulations NIS not detected (%)",
                         limits = c(0,100))
    
    # _Pct_No_Detect
    plot_ls[[w]][[paste0(x, "_Pct_No_Detect")]] <- p2 # _Pct_No_Detect
  
    #    
  }
  
  # for each extract out of list. 
  png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Sensitivity_", w, ".png")),
      width = 30, height = 60, units = "cm", res = 300)
  
  plot(plot_ls[[w]]$seed_n_T_detect + plot_ls[[w]]$seed_n_Pct_No_Detect +
       plot_ls[[w]]$num_sites_T_detect + plot_ls[[w]]$num_sites_Pct_No_Detect +
       plot_ls[[w]]$num_years_T_detect + plot_ls[[w]]$num_years_Pct_No_Detect +
       plot_ls[[w]]$mean_visit_rate_T_detect + plot_ls[[w]]$mean_visit_rate_Pct_No_Detect +
       plot_ls[[w]]$p_detection_T_detect + plot_ls[[w]]$p_detection_Pct_No_Detect +
       plot_ls[[w]]$establish_prob_T_detect + plot_ls[[w]]$establish_prob_Pct_No_Detect + plot_layout(nrow = 6, ncol = 2))
  
  dev.off()
  
  # Export this list
}

# Save the plot list
save(x = plot_ls, file = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Sensitivity_Graphs_ls.RData")))

}
