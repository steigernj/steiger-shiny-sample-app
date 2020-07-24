library(plyr)

drill_down_model_plot <- function(df) {
  p <- ggplot()
  p <- p + geom_line(data=df,aes(x=x,y=predicted_y,color=experiment_id))
  p <- p + scale_x_log10()
  p <- p + theme_bw()
  p <- p + facet_wrap(~compound)
  
  return(p)
}

drill_down_table <- function(df) {
  print(head(df))
  df <- df[!duplicated(df$experiment_id),]
  df <- dplyr::select(df,-x,-y)
  print(head(df))
}