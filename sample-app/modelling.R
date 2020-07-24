library(drc)
df <- read.csv("results.csv")

model_individual <- function(id,df,model_version) {
  df_ <- subset(df,experiment_id==id)
  
  if(model_version=="4-phase Log-Log") {
    fun_type=LL.4()
  } else if(model_version=="3-phase Log-Log") {
    fun_type=LL.3()
  }
  
  #Four-Phase Log-Logistic Model from the drc package
  Model.LL.4 <- drm(y~x, data=df_, fct=fun_type) #,names=c("Slope","Lower Limit","Upper Limit","ED50"))
  
  # model_coef <- as.data.frame(coef(Model.LL.4))[,1]
  # print(model_coef)
  
  #Create interval over which to predict the modeled line
  newdata <- expand.grid(x=exp(seq(log(min(df_$x)),log(max(df_$x)),length=100)))
  predicted_model <- predict(Model.LL.4, newdata=newdata, interval="confidence")
  
  newdata$predicted_y <- predicted_model[,1]
  newdata$interval_min <- predicted_model[,2]
  newdata$interval_max <- predicted_model[,3]
  newdata$compound <- df_$compound[1]
  newdata$experiment_id <- df_$experiment_id[1]
  newdata$date <- df_$date[1]
  
  #normalize predicted curves
  min_p <- newdata$predicted_y[newdata$x==min(newdata$x)]
  max_p <- newdata$predicted_y[newdata$x==max(newdata$x)]
  newdata$predicted_y_norm <- (newdata$predicted_y-min_p)/(max_p-min_p)
  
  # newdata$slope <- model_coef[1]
  # newdata$min <- model_coef[2]
  # newdata$max <- model_coef[3]
  
  #Calculate the half maximal effective concentration(EC50)
  EC50 <- ED(Model.LL.4, 50, interval="delta")
  EC50_df <- as.data.frame(EC50)
  colnames(EC50_df)[1:2] <- c("EC50","SE")
  
  newdata$EC50 <- EC50_df$EC50[1]
  newdata$SE <- EC50_df$SE[1]
  return(newdata)
}

split_and_model <- function(df,model_version="4-phase Log-Log") {
  df$experiment_id <- paste(df$compound,df$batch,df$date,sep="_")
  indiv_model_df <- lapply(unique(df$experiment_id),model_individual,df=df,model_version=model_version)
  combined_model_df <- do.call(rbind,indiv_model_df)
  return(combined_model_df)
}
