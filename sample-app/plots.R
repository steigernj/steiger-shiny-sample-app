library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
source("plot_and_table_utils.R")

# df <- read.csv("results.csv")
raw_df_norm <- function(id,df) {
  df <- subset(df,experiment_id==id)
  min_y=df$y[df$x==min(df$x)]
  max_y=df$y[df$x==max(df$x)]
  df$norm_y <- (df$y-min_y)/(max_y-min_y)
  return(df)
}

model_plot <- function(df,model_df,showdata,control_sel,normalize_input,model_version) {
  # if(is.null(model_version)) {
  #   model_version<-"4-phase Log-Log"
  # }
  
  ## CLEAN THIS MESS ##
  df$experiment_id <- paste(df$compound,df$batch,df$date,sep="_")
  df_indiv <- lapply(unique(df$experiment_id),raw_df_norm,df)
  df <- do.call(rbind,df_indiv)
  
  #Calculate mean curve data and raw points data and add factor ordering for graph colors(set control to red)
  avg_df <- ddply(df,c("compound","x"),summarise,
                  mean_y=mean(y),
                  sd_y=sd(y),
                  mean_norm=mean(norm_y),
                  sd_norm=sd(norm_y),
                  n=length(y))
  
  df_col <- data.frame(compound=c("X-101","X-102","X-103","X-104","X-105","X-106","X-107","X-108","X-109","X-110"),color_fcts=c(2,3,4,5,6,7,8,9,10,11))  
  
  avg_df <- merge(avg_df,df_col)
  avg_df$color_fcts[avg_df$compound==control_sel] <- 1
  avg_df$color_fcts <- as.numeric(avg_df$color_fcts)
  avg_df$compound <- factor(avg_df$compound, levels=unique(avg_df$compound[order(avg_df$color_fcts)]),ordered=TRUE)
  # colnames(avg_df)[names(avg_df)=="x"] <- "conc"
  
  avg_model_df <- ddply(model_df,c("compound","x"),summarise,
                        mean_predicted_y=mean(predicted_y),
                        sd_predicted_y=sd(predicted_y),
                        mean_norm=mean(predicted_y_norm),
                        sd_norm=sd(predicted_y_norm),
                        n=length(predicted_y))
  avg_model_df <- merge(avg_model_df,df_col)
  avg_model_df$color_fcts[avg_model_df$compound==control_sel] <- 1
  avg_model_df$color_fcts <- as.numeric(avg_model_df$color_fcts)
  avg_model_df$compound <- factor(avg_model_df$compound, levels=unique(avg_model_df$compound[order(avg_model_df$color_fcts)]),ordered=TRUE)
  
  palette <- c("#ff0000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  p <- ggplot()
  p <- p + geom_point()
  # p <- p + xlim(0,5000) + ylim(0,650)
  
  #Plot layers based on user inputs
    p_model <- geom_line(data=avg_model_df,aes(x=x,y=mean_predicted_y,color=compound)) 
    p_model_ribbon <- geom_ribbon(data=avg_model_df,aes(x=x,ymin=mean_predicted_y-sd_predicted_y,ymax=mean_predicted_y+sd_predicted_y,fill=compound),alpha=0.2,show.legend=F)
    p_norm_model <- geom_line(data=avg_model_df,aes(x=x,y=mean_norm,color=compound))
    p_norm_model_ribbon <- geom_ribbon(data=avg_model_df,aes(x=x,ymin=mean_norm-sd_norm,ymax=mean_norm+sd_norm,fill=compound),alpha=0.2,show.legend=F)
    p_raw <- geom_point(data=avg_df,aes(x=x,y=mean_y,color=compound)) 
    p_raw_error <- geom_errorbar(data=avg_df,aes(x=x,ymin=mean_y-sd_y,ymax=mean_y+sd_y,color=compound),width=0.03,size=0.01)
    p_raw_norm <- geom_point(data=avg_df,aes(x=x,y=mean_norm,color=compound)) 
    p_raw_norm_error <- geom_errorbar(data=avg_df,aes(x=x,ymin=mean_norm-sd_norm,ymax=mean_norm+sd_norm,color=compound),width=0.03,size=0.01)
    
    if(normalize_input==T){
      if(setequal(showdata,c("Raw Data", "Modeled Data"))==TRUE) {
        p <- p + p_norm_model + p_norm_model_ribbon + p_raw_norm + p_raw_norm_error
      } else if(showdata=="Raw Data") {
        p <- p + p_raw_norm + p_raw_norm_error
      } else if(showdata=="Modeled Data") {
        p <- p + p_norm_model + p_norm_model_ribbon
      }
    } else {
      if(setequal(showdata,c("Raw Data", "Modeled Data"))==TRUE) {
        p <- p + p_model + p_model_ribbon + p_raw + p_raw_error
      } else if(showdata=="Raw Data") {
        p <- p + p_raw + p_raw_error
      } else if(showdata=="Modeled Data") {
        p <- p + p_model + p_model_ribbon
      }
    }
    
  p <- p + theme_bw()
  p <- p + scale_x_log10()
  p <- p + scale_color_manual(values=palette)
  p <- p + scale_fill_manual(values=palette)
  p <- p + xlab("Concentration") + ylab("Response +/- SD")
  p <- p + labs(title=paste("Average Models: ",model_version,sep=""), subtitle=paste("Model Used: ",model_version), color="Compound", fill="Compound")
  return(p)
}


bar_plot <- function(df,control_sel) {
  ##Error bars are misaligned
  avg_df <- bar_plots_calculations(df=df,control_sel=control_sel)
  
  col_palette <- c("#ff0000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  col_palette <- col_palette[1:length(unique(avg_df$compound))]
  avg_df <- arrange(avg_df,desc(compound))
  p <- plot_ly(data=avg_df,x=~compound,y=~mean_EC50,color=~compound,colors=col_palette,type='bar') %>%
               # error_y=~list(array=sd_EC50,color="#000000")) %>%
    layout(showlegend=FALSE,
           title="Half Maximal Effective Concentration(EC50)",
           xaxis=list(title=""),
           yaxis=list(title="Mean EC50 +/- SD"))

  return(p)
}

perc_plot <- function(df,control_sel) {
  ##Error bars are misaligned
  avg_df <- bar_plots_calculations(df=df,control_sel=control_sel) 
  
  col_palette <- c("#ff0000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  col_palette <- col_palette[1:length(unique(avg_df$compound))]
  avg_df <- arrange(avg_df,desc(compound))
  p <- plot_ly(data=avg_df,x=~compound,y=~mean_per,color=~compound,colors=col_palette,type='bar') %>%
               # error_y=~list(type="data",array=sd_per)) %>%
    layout(showlegend=FALSE,
           title="% Change from Control",
           xaxis=list(title=""),
           yaxis=list(title="Mean Change(%) +/- SD"))
  
  return(p)
}