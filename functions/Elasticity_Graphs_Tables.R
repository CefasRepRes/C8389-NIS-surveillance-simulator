Sensitivity_Graphs_Tables

# DATA WRANGLING -----------------------------------------
surveillance <- names(elasticity_dfs)

# for each of the surveillance strategy results
elasticity_dfs1 <- lapply(setNames(surveillance, surveillance), function(x) {
  
  lapply(names(elasticity_dfs[[x]]), function(y) {
    
    # place row names in column and reset row names
    new_df <- data.frame(names = row.names(elasticity_dfs[[x]][[y]]), elasticity_dfs[[x]][[y]], row.names = NULL)
    
  })
  
})

elasticity_dfs2 <- lapply(setNames(surveillance, surveillance), function(x) {
  # combine these data frames
  elasticity_df <- rbind(elasticity_dfs1[[x]][[1]], elasticity_dfs1[[x]][[2]])
  df_long <- reshape2::melt(elasticity_df, id.vars = c("names", "direction"))
  
  # convert columns to factor
  cols <- c("variable", "direction", "names")
  df_long[cols] <- lapply(df_long[cols], factor)
  
  # # make elastcity values positive
  df_long$value <- abs(df_long$value)
  
  return(df_long)
})

# produce plots
lapply(surveillance, function(x) {
  
  ggplot(data = elasticity_dfs2[[x]],
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
})

```

```{r, echo = FALSE, fig.width = 12, fig.height = 6, fig.align = 'center', warnings = FALSE}
elasticity_dfs3 <- data.table::rbindlist(elasticity_dfs2, idcol = "index")
elasticity_dfs3$index <- factor(elasticity_dfs3$index)

ggplot(data = elasticity_dfs3,
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




