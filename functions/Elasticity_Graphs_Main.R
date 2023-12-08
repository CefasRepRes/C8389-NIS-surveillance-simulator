# 27/07/23
# Tom Gibson
# Elasticity_Graphs_Main
# Checked on 10/08/23 # updated to ensure its pct_no_detect being plotted. 

# This exports the key plots from the elasticity analysis in .png and saves in a list to /Graphs 

Elasticity_Graphs_Main <- function(){
  
  # DATA WRANGLING -----------------------------------------
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
  
  # Create plot list
  plot_ls <- vector(mode = "list", length = 1) # create list to save into 
  names(plot_ls) <- c("all")
  
  # produce plots
  
  elasticity_dfs3 <- data.table::rbindlist(elasticity_dfs2, idcol = "index")
  elasticity_dfs3$index <- factor(elasticity_dfs3$index)
  
  Var.lab <- c("Median Detection Time", "No Detection Simulations")
  names(Var.lab) <- c("median", "pct_no_detect")
  
  p2 <- ggplot(data = elasticity_dfs3,
               aes(x = names, y = value, color = index, shape = direction)) + 
    geom_point(size = 2.25) +
    facet_wrap(~ variable, ncol = 2, scales = "free_y", labeller = labeller(variable = Var.lab)) +
    theme_bw() +
    geom_hline(yintercept = 1, color = "dark gray") +
    ylab("Elasticity") +
    xlab("Parameter") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "bottom",
          legend.title = element_blank())
  
  plot_ls[["all"]] <- p2
  
  png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Elasticity_Main", ".png")),
      width = 20, height = 12, units = "cm", res = 300)
  
  plot(p2)
  
  dev.off()
  
  # Export this final list
  save(x = plot_ls, file = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Elasticity_Graphs_Main_ls.RData")))
  
}

