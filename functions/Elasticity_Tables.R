# 15/09/23
# Tom Gibson
# Elasticity Tables
# Checked 15/09/23

Elasticity_Tables <- function(){

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

# Create the Final Table
elas_table <- rbindlist(elasticity_dfs2, idcol = "Scenario")

# Export the table
write.csv(x = elas_table, file = file.path("outputs", config$run_name, "Tables", paste0(config$run_name, "_Summary_Elasticity.csv")))

}

