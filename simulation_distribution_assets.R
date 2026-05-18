rm(list = ls())

#install.packages("arrow")
library("arrow")
library(lubridate)
library(dplyr)

library(plotly)
Sys.setenv(LANGUAGE = "en")

setwd("C:/Users/CONS_LGA/Dropbox/hedge_fund/polygon-downloader/polygon-api")

source("R_analysis/R_functions/transform.R")
source("R_analysis/R_functions/estimation.R")
source("R_analysis/R_functions/strategies.R")
source("R_analysis/R_functions/statistics.R")



function_C <- function(z){
  
  return((1-sqrt(1-4*z) ) / (2*z))
}

# point to the data location
setwd("C:/Users/CONS_LGA/Dropbox/hedge_fund/production/quiescence/data/POLYGON")

root_dir <- file.path(getwd())

symbols <- c("X_BTCUSD", "PLTR","GOOG","AAPL","PYPL","META","NFLX","MSFT")
symbols <- c("AMD", "AMZN", "JNJ", "NVDA", "ORCL", "TSLA", "UNH", "WMT")

for (sym in symbols){
  
# select the ticker and data frequency
# symbol <- "X_BTCUSD"
# symbol <- "PLTR"
# symbol <- "GOOG"
# symbol <- "AAPL"
# symbol <- "PYPL"
# symbol <- "META"
# symbol <- "NFLX"
# symbol <- "MSFT"
  
symbol <- sym

bar_interval <- "minute"

# create common data set
dir_path_sessions <- file.path(symbol, "bar", "1-MINUTE-LAST")
dates_sessions <- list.files(path = dir_path_sessions, full.names = TRUE)

parquet_files <- character(0)
for (i in 1: length(dates_sessions)){
  parquet_files <- c(parquet_files, list.files(path = dates_sessions[i], full.names = TRUE))
}
print(parquet_files)

df_list <- lapply(parquet_files, read_parquet)

# get all the data into the datframe
df_all <- do.call(rbind, df_list)

# convert the time stamp and convert into the New York exchange time 
df_all$timestamp <- as.numeric(as.character(df_all$timestamp))

df_all$time_date <- as.POSIXct(
  df_all$timestamp  / 1000,
  origin = "1970-01-01",
  tz = "UTC"
)

df_all$time_ny <- with_tz(df_all$time_date, "America/New_York")

# df_all <- df_all %>%
#   filter(
#     hour(time_ny) >= 12 & hour(time_ny) < 15
#   )

# extract the close value from the df to the random walk container
rw_cont <- df_all$close

# binomialize
rw_binom_asset <-binarize_rolling(rw_cont, window_length = 1000)

numSim <- 100000
T <- 500

J_w_record <- rep(NA, numSim)
J_w_2_record <- rep(NA, numSim)
m_n <- rep(NA, numSim)
w_record <- rep(NA, numSim)
w_2_record <- rep(NA, numSim)

T_w_record <- rep(NA, numSim)
T_w_2_record <- rep(NA, numSim)


for (i in 1: numSim) {
  
  
  start <- round(runif(1, min = 1, max = length(rw_binom_asset)-T), 0 )
  
  
  rw_binom <- rw_binom_asset[start:(start+T)]
  rw_binom <- rw_binom - rw_binom[1]
  
  
  w_record[i] <- max(rw_binom)
  
  T_w_record[i] <- which(rw_binom >=  w_record[i])[1]
   
  
  if (T_w_record[i] > ( 2) ) {
    
    
    T_w_2_record[i] <- which(rw_binom >=  w_record[i]/2)[1]
 
    
    estimation_results_w <- p_estimator(rw_binom[1:T_w_record[i]])
    estimation_results_w_2 <- p_estimator(rw_binom[1:T_w_2_record[i]])
    
    
    p_est_w <- estimation_results_w$p_analytical
    p_est_w_2 <- estimation_results_w_2$p_analytical
    
    
    J_w_record[i] <- estimation_results_w$sum_jumps
    J_w_2_record[i] <- estimation_results_w_2$sum_jumps
    
    m_n[i] <- J_w_2_record[i] / J_w_record[i]
    
    
  }

  
}



tol = 0.25
J_w_target <- quantile(J_w_record, 0.6, na.rm= TRUE)

ymax <- 0

# all figure max
filters_J_w <- (
  (J_w_record > ((1 - tol) * J_w_target)) &
    (J_w_record < ((1 + tol) * J_w_target) ))

w_start <- quantile(w_record[filters_J_w], 0.05, na.rm = TRUE)
w_end <- quantile(w_record[filters_J_w], 0.9, na.rm = TRUE)
# w_start <- 10

num_graphs <- 6

for (w_target in seq(w_start, w_end, length.out = num_graphs)   ){
  
#  w_target <- w_start + step
  
  filters <- (
    (J_w_record > ((1 - tol) * J_w_target)) &
      (J_w_record < ((1 + tol) * J_w_target)) &
      (w_record  > ((1 - tol) * w_target)) &
      (w_record  < ((1 + tol) * w_target))
  )
  
  x <- J_w_2_record[filters] / 2
  x <- x[!is.na(x)]
  
  if(length(x) > 1){
    d <- density(x)
    ymax <- max(ymax, d$y)
  }
}


pdf(paste0("C:/Users/CONS_LGA/Dropbox/habilitacja_gatarek_welfe/asset_distribution/graphs_distribution/", symbol, ".pdf"),
   width = 8,
    height = 12)

par(mfrow = c(num_graphs / 2, 2))

# draw
for (w_target in seq(w_start, w_end, length.out = num_graphs)    )    {
  
 # w_target <- w_start + step
  
  filters <- (
    (J_w_record > ((1 - tol) * J_w_target)) &
      (J_w_record < ((1 + tol) * J_w_target)) &
      (w_record  > ((1 - tol) * w_target)) &
      (w_record  < ((1 + tol) * w_target))
  )
  
  x <- J_w_2_record[filters] / 2
  x <- x[!is.na(x)]
  
  hist(x,
       breaks = 50,
       prob = TRUE,
       ylim = c(0, ymax),
       xlim = c(0, 1.1*round(J_w_target/2)),
       main = "",
       cex.main = 2.5,   
       cex.lab  = 1.9,  
       cex.axis = 1.9, 
       xlab = "m",
       ylab = ""
   
       
       )
  
  title(main = paste0("w = ", round(w_target)),
        cex.main = 1.5,
        line = -6,
        adj = 1)
  
  lines(density(x), lwd = 3)
}

dev.off()

}

  