library(ggplot2)
library(plotly)

buildata <- function() {
  compounds <- c("X-101","X-102","X-103","X-104","X-105","X-106","X-107","X-108","X-109","X-110")
  x = exp(seq(log(0.15),log(5000),length=10))
  df_indiv <- lapply(compounds,
                     function(compound,x) {
                        df_ <- data.frame(compound=compound,x=x)
                      },
              x=x)
  df_combined <- do.call(rbind,df_indiv)
  print(df_combined)
}

