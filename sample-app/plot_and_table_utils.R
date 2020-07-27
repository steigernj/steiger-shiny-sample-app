
bar_plots_calculations <- function(df,control_sel) {
  df <- df[!duplicated(df$experiment_id),]
  control_EC50 <- mean(df$EC50[df$compound==control_sel])
  df$perc_change <- ((df$EC50-control_EC50)/control_EC50)*100
  
  avg_df <- ddply(df,c("compound"),summarise,
                  mean_EC50=round(mean(EC50),1),
                  sd_EC50=round(sd(EC50),1),
                  mean_per=round(mean(perc_change),1),
                  sd_per=round(sd(perc_change),1),
                  n=length(EC50))
  
  avg_df$mean_per[avg_df$compound==control_sel] <- 0
  avg_df$sd_per[avg_df$compound==control_sel] <- 0
  
  df_col <- data.frame(compound=c("X-101","X-102","X-103","X-104","X-105","X-106","X-107","X-108","X-109","X-110"),color_fcts=c(2,3,4,5,6,7,8,9,10,11))
  avg_df <- merge(avg_df,df_col)
  avg_df$color_fcts[avg_df$compound==control_sel] <- 1
  avg_df$color_fcts <- as.numeric(avg_df$color_fcts)
  avg_df$compound <- factor(avg_df$compound, levels=unique(avg_df$compound[order(avg_df$color_fcts)]),ordered=TRUE)
  
  return(avg_df)
}

build_summary_table <- function(df,control_sel) {
  avg_df <- bar_plots_calculations(df=df,control_sel=control_sel)
  avg_df <- dplyr::select(avg_df,-color_fcts)
  avg_df$control_bool <- ""
  avg_df$control_bool[avg_df$mean_per==0] <- "Control"
  col_order <- c("compound","control_bool","n","mean_EC50","sd_EC50","mean_per","sd_per")
  avg_df <- avg_df[, col_order]
  colnames(avg_df) <- c("Compound","Control Selected","N","Mean EC50","SD EC50","Mean % Change\nFrom Control","SD % Change\nFrom Control")
  datatable(avg_df,
            extensions = 'Buttons', options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
  )
}

build_raw_table <- function(df) {
  col_order <- c("compound","batch","date","experiment_id","x","y")
  df <- df[, col_order]
  colnames(df) <- c("Compound","Batch","Experiment Date","Experiment ID","Concentration","Response")
  datatable(df,
            extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
  )
}

build_model_table <- function(df) {
  col_order <- c("compound","batch","date","experiment_id","x","predicted_y","interval_min","interval_max","predicted_y_norm","EC50","SE")
  df <- df[, col_order]
  colnames(df) <- c("Compound","Batch","Experiment Date","Experiment ID","Concentration","Predicted Response","Confidence Interval Min","Confidence Interval Max","Min/Max Normalzed Predicted Response","Estimated EC50","EC50 SE")
  datatable(df,
            extensions = list(
              'FixedColumns'=NULL,
              'Buttons'=NULL
            ),
              options = list(
                # dom = 't',
                scrollX = TRUE,
                fixedColumns = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )
              # )
              # 'Buttons', options = list(
              # dom = 'Bfrtip',
              # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
            # )
  ) 
}

# Dose-response based experiments are used throughout the drug discovery process in order to compare novel molecules' efficacy, potency, and mitogenicity performances during cell-based assays. Analysis of this data plays a vital role in research planning and progress. This tool is meant to be an example of dose-response data modeling and analysis using data similar to that resulting from cell-based assays.