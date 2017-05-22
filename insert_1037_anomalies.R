# read data file, identify spaces, insert signatures
# 
#AC work for extra hours cycle
hour <- 5
anomaly   <- rnorm(6*hour,1150,2) # 6 refers to the no. of observations per hour
anomaly_2 <- rnorm(6*hour,310,2) # for second appliance
start_time <- as.POSIXct("2014-07-02 07:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend1 <- xts(data.frame(anomaly,anomaly_2),time_sequence) 
 #2 AC took long cycles during day
hour <- 7
anomaly   <- rnorm(6*hour,1150,2) # 6 refers to the no. of observations per hour
anomaly_2 <- rnorm(6*hour,310,2) # for second appliance
start_time <- as.POSIXct("2014-07-20 05:30:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend2 <- xts(data.frame(anomaly,anomaly_2),time_sequence) 
#dframe_extend2
#3 AC works at unknown time
hour <- 8
anomaly <- rep(c(rnorm(5,1150,1),rnorm(1,2,1)),hour)
anomaly_2 <- rep(c(rnorm(5,310,1),rnorm(1,1,0.5)),hour)
start_time <- as.POSIXct("2014-07-28 13:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend3 <- xts(data.frame(anomaly,anomaly_2),time_sequence) 
#dframe_extend3

#4 AC work for extra hours cycle
hour <- 8
anomaly <- rep(c(rnorm(4,1150,1),rnorm(2,2,1)),hour)
anomaly_2 <- rep(c(rnorm(4,310,1),rnorm(2,1,0.5)),hour)
start_time <- as.POSIXct("2014-08-10 00:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend4 <- xts(data.frame(anomaly,anomaly_2),time_sequence) 
#5 AC took long cycles during day
hour <- 10
anomaly <- rep(c(rnorm(4,1150,1),rnorm(2,2,1)),hour)
anomaly_2 <- rep(c(rnorm(4,310,1),rnorm(2,1,0.5)),hour)
start_time <- as.POSIXct("2014-08-18 14:00:00")
end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
time_sequence <- seq(start_time, end_time, by = "10 min")
dframe_extend5 <- xts(data.frame(anomaly,anomaly_2),time_sequence) 
#dframe_extend5
#6 AC works at unknown time
# hour3 <- 6
# anomaly <- rep(c(rnorm(4,1150,1),rnorm(2,2,1)),hour3)
# anomaly_2 <- rep(c(rnorm(4,310,1),rnorm(2,1,0.5)),hour3)
# start_time <- as.POSIXct("2014-08-28 12:00:00")
# end_time <- start_time + (length(anomaly)-1)*10*60 # mins to seconds
# time_sequence <- seq(start_time, end_time, by = "10 min")
# dframe_extend6 <- xts(data.frame(anomaly,anomaly_2),time_sequence) 
# #dframe_extend
insert_frame <- rbind(dframe_extend1,dframe_extend2,dframe_extend3,
                      dframe_extend4,dframe_extend5)
colnames(insert_frame) <- c("air1","furnace1")

file1 <- "1037.csv"
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
df <- fread(paste0(path1,file1))
df_xts <- xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$localminute)-19800)
head(df,2)[,1]
head(df_xts,2)[,2]
df_orig <- df_xts["2014-06-1/2014-08-30"]

mutate_frame <- mutate_columns(df_orig,insert_frame)
save_dir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/mutated/"
mutate_csv <- data.frame(localminute=index(mutate_frame),coredata(mutate_frame))
write.csv(mutate_csv,file=paste0(save_dir,file1),row.names = FALSE)

mutate_columns <- function(df_orig,insert_frame){
  # function used to mutate columns of df_orig with synthetic data
  cols <- colnames(insert_frame)
  for(i in 1:length(cols)){
    t_stamp <- index(insert_frame)
    df_orig[t_stamp,'use'] <- df_orig[t_stamp,'use'] - df_orig[t_stamp,cols[i]]
    df_orig[t_stamp,cols[i]] <- insert_frame[t_stamp,cols[i]]
    df_orig[t_stamp,'use'] <- df_orig[t_stamp,'use'] + df_orig[t_stamp,cols[i]]
  }
return(df_orig)
}