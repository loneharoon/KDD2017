
# RANDOM CODE TO FORMAT IAWE DATASET

path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/1.csv"
df <- fread(path)
Sys.setenv(TZ='Asia/Kolata') # resets system to GMT
df$UNIX_TS <- as.POSIXct(df$UNIX_TS,origin="1970-01-01")# subtracts 7 hours so that it sets to vancouer timings
df_power <- xts(df[,2:dim(df)[2]],df[,1])


combine_aggregate_plus_appliance_data <- function() {
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity"
  #***CHANGE PATH FOR EACH HOUSE********************
  savepath <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/"
  sm_path <- paste0(path,"06_sm/")
  plugs_path <-  paste0(path,"06_plugs")
  #***********************************************
 df1 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/1.csv")
 df2 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/2.csv")
 
 df1_new <- xts(df1$W,as.POSIXct(df1$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
 df2_new <- xts(df2$W,as.POSIXct(df2$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
 
 temp_df <- cbind(df1_new,df2_new)
 
 
 df3 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/3.csv")
 df4 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/4.csv")
 df5 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/5.csv")
 df6 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/6.csv")
 df7 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/7.csv")
 df8 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/8.csv")
 df9 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/9.csv")
 df10 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/10.csv")
 df11 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/11.csv")
 df12 <-  read.csv(file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/electricity/12.csv")
 
 df3_new <- xts(df3$W,as.POSIXct(df3$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
 df4_new <- xts(df4$W,as.POSIXct(df4$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
 df5_new <- xts(df5$W,as.POSIXct(df5$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
 df6_new <- xts(df2$W,as.POSIXct(df6$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
 fls <- paste0("df",3:12)
 fls <- c(df3, df4, df5, df6,df7,df8,df9,df10,df11, df12)
 for(i in 1:10){
     tempx <- xts(df12$W,as.POSIXct(df12$timestamp,origin="1970-01-01",tz="Asia/Kolkata")) 
   temp_df <- cbind(temp_df,tempx)
 
   }
 
 colnames(temp_df) <- c("main1","main2","fridge","ac1","ac2","washing_mc","laptop","iron","kitchen","tv","water_filter","water_motor")
 dfs <- data.frame(timestamp=index(temp_df),coredata(temp_df))
# write.csv(dfs,file = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/aiwe_dataset.csv",row.names = FALSE) 
 
 resample_GMT_data<- function(xts_datap,xminutes) {
   # works when timezone is GMT or Sys.setenv('TZ'='') 
   ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap), on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
   # align data to nearest time boundary
   align_data <- align.time(ds_data,xminutes*60) # aligning to x minutes
   # return(ds_data)
   rm(ds_data)
   return(align_data)
 }
 
df <- fread("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/aiwe_dataset.csv")
df_xts <- xts(df[,2:dim(df)[2]],as.POSIXct(df$timestamp,origin="1970-01-01",tz="Asia/Kolkata") )
index(df_xts) <- index(df_xts) - 19800
dfs <- data.frame(timestamp=index(df_xts),coredata(df_xts))
write.csv(dfs,file = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/aiwe_dataset.csv",row.names = FALSE) 

mindat <- resample_GMT_data(df_xts,10)
mindat$main <- mindat$main1 + mindat$main2
dfs <- data.frame(timestamp=index(mindat),coredata(mindat))
write.csv(dfs,file = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/iawe/aiwe_dataset_10min.csv",row.names = FALSE) 

}