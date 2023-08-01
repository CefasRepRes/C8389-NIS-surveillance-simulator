# 27/07/23
# Tom Gibson
# Elasticity_Graphs
# Checked 01/08/23

# This plot takes the code from report-NIS-intro-detect-sensitivity.Rmd 
# And makes it easier to export plots in .png and saved in a list to /Graphs and 
# 1 table of summary results to /Table. 

Elasticity_Graphs <- function(){

# DATA WRANGLING -----------------------------------------
surveillance <- names(elasticity_dfs)

# for each of the surveillance strategy results
elasticity_dfs1 <- lapply(setNames(surveillance, surveillance), function(x) {
  
  lapply(names(elasticity_dfs[[x]]), function(y) {
    
    # place row names in column and reset row names
    new_df <- data.frame(names = row.names(elasticity_dfs[[x]][[y]]), elasticity_dfs[[x]][[y]], row.names = NULL)
    
  }) })

elasticity_dfs2 <- lapply(setNames(surveillance, surveillance), function(x) {
  # combine these data frames
  elasticity_df <- rbind(elasticity_dfs1[[x]][[1]], elasticity_dfs1[[x]][[2]])
  df_long <- reshape2::melt(elasticity_df, id.vars = c("names", "direction"))
  
  # convert columns to factor
  cols <- c("variable", "direction", "names")
  df_long[cols] <- lapply(df_long[cols], factor)
  
  # # make elastcity values positive
  df_long$value <- abs(df_long$value)
  
  return(df_long)})

# Create plot list
plot_ls <- vector(mode = "list", length = 4) # create list to save into 
names(plot_ls) <- c(surveillance, "all")

# produce plots

for(i in 1:length(surveillance)){
  
  x <- surveillance[i]
  
  p1 <- ggplot(data = elasticity_dfs2[[x]],
         aes(x = names, y = value, color = direction)) + 
    geom_point() +
    facet_wrap(~ variable, ncol = 5, scales = "free_y") +
    theme_bw() +
    geom_hline(yintercept = 1, color = "dark gray") +
    ylab("Elasticity") +
    xlab("Parameter") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "bottom",
          legend.title = element_blank()) +
    ggtitle(x)
  
  plot_ls[[surveillance[i]]] <- p1
  
  png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Elasticity_", surveillance[i], ".png")),
      width = 35, height = 17.5, units = "cm", res = 300)
  
  plot(p1)
  
  dev.off()
  
}

elasticity_dfs3 <- data.table::rbindlist(elasticity_dfs2, idcol = "index")
elasticity_dfs3$index <- factor(elasticity_dfs3$index)

p2 <- ggplot(data = elasticity_dfs3,
            aes(x = names, y = value, color = index, shape = direction)) + 
       geom_point() +
       facet_wrap(~ variable, ncol = 5, scales = "free_y") +
       theme_bw() +
       geom_hline(yintercept = 1, color = "dark gray") +
       ylab("Elasticity") +
       xlab("Parameter") +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
             legend.position = "bottom",
             legend.title = element_blank())

plot_ls[["all"]] <- p2

png(filename = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Elasticity_all", ".png")),
    width = 35, height = 17.5, units = "cm", res = 300)

plot(p2)

dev.off()

# Export this final list
save(x = plot_ls, file = file.path("outputs", config$run_name, "Graphs", paste0(config$run_name, "_Elasticity_Graphs_ls.RData")))

}

