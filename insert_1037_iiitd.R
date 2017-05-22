

hour <- 7
anomaly   <- rnorm(6*hour,1300,8) # 6 refers to the no. of observations per hour
start_time <- as.POSIXct("2014-07-10 18:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend1 <- xts(anomaly,time_sequence) 
#dframe_extend2
#3 AC works at unknown time
hour <- 6
anomaly <- rep(c(rnorm(5,1300,8),rnorm(1,2,1)),hour)
start_time <- as.POSIXct("2014-07-17 17:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend2 <- xts(anomaly,time_sequence) 
#dframe_extend3

hour <- 8
anomaly <- rep(c(rnorm(4,1300,8),rnorm(2,2,1)),hour)
start_time <- as.POSIXct("2014-07-24 16:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend3 <- xts(anomaly,time_sequence) 

hour <- 6
anomaly  <- rnorm(6*hour,1300,8)
start_time <- as.POSIXct("2014-08-04 08:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend4 <- xts(anomaly,time_sequence) 


hour <- 7
anomaly   <- rnorm(6*hour,160,8) # 6 refers to the no. of observations per hour
start_time <- as.POSIXct("2014-08-22 18:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend5 <- xts(anomaly,time_sequence) 


hour <- 6
anomaly   <- rnorm(6*hour,160,8) # 6 refers to the no. of observations per hour
start_time <- as.POSIXct("2014-08-28 15:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend6 <- xts(anomaly,time_sequence) 


insert_frame <- rbind(dframe_extend1,dframe_extend2,dframe_extend3,dframe_extend4,
                      dframe_extend5,dframe_extend6)
#colnames(insert_frame) <- c("air1","furnace1")


file1 <- "1037_iiit.csv"
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
df <- fread(paste0(path1,file1))
df_xts <- xts(df$use,fasttime::fastPOSIXct(df$localminute)-19800)
head(df,2)
head(df_xts,2)
df_orig <- df_xts["2014-06-1/2014-08-30"]

mutate_frame <- add_anomaly(df_orig,insert_frame)
colnames(mutate_frame) <- "use"
save_dir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/mutated/"
mutate_csv <- data.frame(localminute=index(mutate_frame),use =coredata(mutate_frame))
write.csv(mutate_csv,file=paste0(save_dir,file1),row.names = FALSE)

visualize_context_data_facet_form(mutate_frame["2014-07-01/2014-07-30"],"use")

plot(neural_result["2014-08-28"]$upr)
lines(mutate_frame["2014-08-28"],col="red")


dat = df_orig
dat <-dat["2014-08-2"]
dat2 <- fortify(dat)
colnames(dat2) <- c("Index","power")
g <- ggplot(dat2,aes(Index,power)) + geom_line()
ggplotly(g)

add_anomaly <- function(df_orig,insert_frame){
  # function used to mutate columns of df_orig with synthetic data
    t_stamp <- index(insert_frame)
    df_orig[t_stamp] <- coredata(insert_frame)
  return(df_orig)
}