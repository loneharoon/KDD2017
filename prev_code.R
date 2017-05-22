dataport_prediction_part <- function() {
  # this function does two tasks [currently first one is in hidden mode]
  # 1: stores only prediction accuracy results of all homes using both methods in a csv
  # 2: stores prediction along with actual data in files corresponding to each home 
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  fls <- mixedsort(list.files(path1,pattern = ".csv"))
  file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  
  day_pred_result <- list()
  
  for (i in 4:10) {
    file1 <- fls[i]
    data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
    #appliance_features <- get_appliance_features(path1, file1)
    print("DATA RANGES ARE:")
    print(paste0("Power data,","start: ",index(first(data_ob$power_data))," end: ",index(last(data_ob$power_data))))
    print(paste0("Weather data,","start: ",index(first(data_ob$weather_data))," end: ",index(last(data_ob$weather_data))))
    merge_start_date <- as.POSIXct(strptime('2014-06-05',format = "%Y-%m-%d"))
    merge_end_date   <- as.POSIXct(strptime('2014-08-31',format = "%Y-%m-%d"))
    confirm_validity(data_ob, merge_start_date, merge_end_date)
    my_range <- paste0(merge_start_date,'/',merge_end_date)
    sampled_ob <- combine_energy_weather(data_ob,my_range)
    
    train_dates <- '2014-06-05/2014-06-24'
    test_dates <-  '2014-06-25/2014-08-30'
    
    
    sampled_ob2 <- subset(sampled_ob,select=power)
    train_data2 <- sampled_ob2[train_dates]
    test_data2 <- sampled_ob2[test_dates]
    # CASE 1: only energy data, no outliers removed
    neural_result2 <- neuralnetwork_procedure_without_weather_with_outliers (train_data2,test_data2,hourwindow = 6, daywindow = 15)
    # CASE 2: outliers removed, no weather variable
    neural_result3 <- neuralnetwork_procedure_without_weather_without_outliers (train_data2,test_data2,hourwindow = 6, daywindow = 15)
    
    ###### CASE 3: outliers removed weather included, i.e, full proposed model
    #train_data <- sampled_ob[train_dates]
    #test_data <- sampled_ob[test_dates]
    #  neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 15)
    comb_df2 <- cbind(test_data2$power,neural_result2)
    comb_temp2 <- data.frame(timestamp = index(comb_df2),round(coredata(comb_df2),2))
    write.csv(comb_temp2,paste0("dport_prediction_without_context/","with_outlier_",file1),row.names=FALSE)
    comb_df3 <- cbind(test_data2$power,neural_result3)
    comb_temp3 <- data.frame(timestamp = index(comb_df3),round(coredata(comb_df3),2))
    write.csv(comb_temp3,paste0("dport_prediction_without_context/","without_outlier_",file1),row.names=FALSE)
    
    #regression_days <- sampled_ob['2014-06-26/2014-07-30']
    #find_regrassivedays_with_BIC(regression_days) # USED TO FIND NO. OF BEST HISTORICAL DAYS FOR REGRESSION
    #reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
    #print("Regression done")
    
    #neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 15)
    #print("N-network done")
    #comb_df <- cbind(test_data$power,neural_result)
    #comb_temp <- data.frame(timestamp = index(comb_df),round(coredata(comb_df),2))
    #write.csv(comb_temp,paste0("dataport_prediction/",file1),row.names=FALSE)
  }
}
#agg_result <- do.call(rbind,day_pred_result)
#write.csv(agg_result,file="prediction_result.csv")
}

dataport_anomaly_detection_part <- function() {
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  pathxx <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_prediction_without_context/"
  pathy <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_AD/"
  fls <- mixedsort(list.files(pathxx,pattern = ".csv"))
  for (i in 2:length(fls)) {
    file1 <- fls[i]
    data_ob <- fread(paste0(pathxx,file1))
    data_xts <- xts(data_ob[,c("power","fit","lwr","upr")],as.POSIXct(strptime(data_ob$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01"))
    test_data_orig <- data_xts$power
    neu_result  <- subset(data_xts,select = -power)
    anomaly_status <- find_anomalous_status(test_data=test_data_orig,result=neu_result,anomaly_window = 1,anomalythreshold_len = 4)
    #anomaly_status <- find_anomalous_status_neg_anomalies(test_data=test_data_orig,result=neu_result,anomaly_window = 1,anomalythreshold_len = 4)
    anom_readings_online <- anomaly_status[anomaly_status == TRUE]
    write.csv(x = fortify(anom_readings_online),file = paste0(pathy,file1),row.names = FALSE)
  }
}

dataport_anomaly_localization <- function() {
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  # this will need three inputs
  # 1. ANomaly detection results file [folder dataport_AD]
  # 2. Prediction file [folder dataport dataport prediction]
  # 3 appliance specifiction/ratings file
  AD_detection_results <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_AD/"
  fls <- mixedsort(list.files(AD_detection_results,pattern="*.csv"))
  predict_data <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_prediction_without_context/"
  write_path <-  "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_AL/"
  appliance_feature_file <- fread("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_applianceRatings_corrected.csv")
  
  for (i in 1:length(fls)) {
    file1 <- fls[i]
    dfile <- strsplit(file1,'[_]')[[1]][3]
    df <- fread(paste0(predict_data,file1))
    df_xts <- xts(subset(df,select=-timestamp),as.POSIXct(strptime(df$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    df_AD <- read.csv(paste0(AD_detection_results,file1))
    df_AD_xts <- xts(df_AD[,2],as.POSIXct(strptime(df_AD[,1],format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    appliance_features <- appliance_feature_file[V1==dfile][,2:dim(appliance_feature_file)[2]]
    #test_data, result, anom_status, appliance_features, window_minutes
    anom_location <- find_anomaly_appliance_with_stored_Results(pred_results = df_xts, anom_status = df_AD_xts,appliance_features,window_minutes = 5)
    write.csv(x = fortify(anom_location),file = paste0(write_path,file1),row.names = FALSE)
  }
  
}

create_third_column <- function() {
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_AD/"
  fls <- list.files(path)
  for(i in 2:length(fls)){
    df <- fread(paste0(path,fls[i]))
    df$actual <- 0
    write.csv(x = data.frame(df$Index,df$actual),file = paste0(path,"gt_",fls[i]),row.names = FALSE)
  }
}
