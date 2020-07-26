
drill_down_model_plot <- function(df) {
  p <- ggplot()
  p <- p + geom_line(data=df,aes(x=x,y=predicted_y,color=experiment_id))
  p <- p + scale_x_log10()
  p <- p + theme_bw()
  p <- p + facet_wrap(~compound)
  p <- p + labs(color="Experiment ID")
  # Y label not showing up
  p <- p + xlab("Concentration") + ylab("Response")
  
  return(p)
}

drill_down_table <- function(df) {
  df <- df[!duplicated(df$experiment_id),]
  df <- dplyr::select(df,-x,-y)
  colnames(df) <- c("Compound","Batch","Experiment Date", "Experiment ID")
  df <- df[order(df$Compound),]
  df <- datatable(df)
  return(df)
}