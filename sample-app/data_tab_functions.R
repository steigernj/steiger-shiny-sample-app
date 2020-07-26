library(DT)
source("plots.R")

build_summary_table <- function(df,control_sel) {
  avg_df <- bar_plots_calculations(df=df,control_sel=control_sel)
  avg_df <- dplyr::select(avg_df,-color_fcts)
  avg_df$control_bool <- FALSE
  avg_df$control_bool[avg_df$mean_per==100] <- TRUE
  col_order <- c("compound","control_bool","n","mean_EC50","sd_EC50","mean_per","sd_per")
  avg_df <- avg_df[, col_order]
  colnames(avg_df) <- c("Compound","Control Selected","N","Mean EC50","SD EC50","Mean % Change\nFrom Control","SD % Change\nFrom Control")
  return(avg_df)
}

build_raw_table <- function(df) {
  col_order <- c("compound","batch","date","experiment_id","x","y")
  df <- df[, col_order]
  colnames(df) <- c("Compound","Batch","Experiment Date","Experiment ID","Concentration","Response")
  return(df)
}

build_model_table <- function(df) {
  col_order <- c("compound","batch","date","experiment_id","x","predicted_y","interval_min","interval_max","predicted_y_norm","EC50","SE","")
  df <- df[, col_order]
  colnames(df) <- c("Compound","Batch","Experiment Date","Experiment ID","Concentration","Predicted Response","Confidence Interval Min","Confidence Interval Max","Min/Max Normalzed Predicted Response","Estimated EC50","EC50 SE")
  return(df)
}