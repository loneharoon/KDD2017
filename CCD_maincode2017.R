library(xts)
library(data.table)
library(ggplot2)
library(gtools)


file1 <- "6165.csv"
#house_no <- "house1_10min.csv"
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
print("reading mutated dataframe")
#path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/mutated/"
file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
#file2<- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/weatherdata_PROCESSED/year2014/delhi_weatherData_year2014_FULL_10mins.csv"
#eco_dataset_path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
#data_ob <- create_weather_power_object(path1,file1,file2)
data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
#data_ob_eco <- create_weather_power_object_ECOdataset(eco_dataset_path,house_no)
# home_details <- get_appliance_features(path1, file1) # returns two things: appliance rating and dissagrigated energy
# replace above function with create_appliance_rating_file() function
print("DATA RANGES ARE:")
print(paste0("Power data,","start: ",index(first(data_ob$power_data))," end: ",index(last(data_ob$power_data))))
print(paste0("Weather data,","start: ",index(first(data_ob$weather_data))," end: ",index(last(data_ob$weather_data))))
merge_start_date <- as.POSIXct(strptime('2014-06-06',format = "%Y-%m-%d"))
merge_end_date   <- as.POSIXct(strptime('2014-08-30',format = "%Y-%m-%d"))
confirm_validity(data_ob,merge_start_date,merge_end_date)
my_range<- paste0(merge_start_date,'/',merge_end_date)
sampled_ob <- combine_energy_weather(data_ob,my_range)
#sampled_ob <- combine_energy_weather(data_ob_eco,my_range) # FOR ECO DATASET
train_data <- sampled_ob['2014-06-01/2014-06-30 23:59:59']
test_data <- sampled_ob['2014-07-01/2014-08-30 23:59:59']
#regression_days <- sampled_ob['2014-06-26/2014-07-30']
#find_regrassivedays_with_BIC(regression_days) # USED TO FIND NO. OF BEST HISTORICAL DAYS FOR REGRESSION
# PREDICTION PART
# reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
 # print_single_prediction_method_table(test_data, method_result = reg_result$fit)
#  plot_singlepredictor_graph(train_data,test_data,reg_result$fit)
 

neural_result <- neuralnetwork_procedure(train_data,test_data,hourwindow = 6, daywindow = 30)
 # print_single_prediction_method_table(test_data, method_result = neural_result$fit)
#  plot_singlepredictor_graph(train_data,test_data,neural_result$fit)
savedir <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_neural_results/"
#write.csv(fortify(neural_result),file=paste0(savedir,file1),row.names = FALSE)

#ESTABLISH GROUND TRUTH
savedir_gt <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_gt_results/"
gt_data <- compute_groundtruth_main(data_ob,past_days = 6,weekday_context = TRUE)
#write.csv(fortify(gt_data),file=paste0(savedir_gt,file1),row.names = FALSE)

  
#recurrent_neural_procedure(train_data,test_data,hourwindow = 6, daywindow = 10)
#plot_graph(test_data,reg_result,neural_result) # PLOTS WITHOUT HISTORICAL DATA
plot_withhistoricaldata_graph(train_data, test_data,reg_result$fit,neural_result$fit)
plot_table(test_data,reg_result$fit,neural_result$fit,intent="print")

# VERIFY ANOMALIES WITH GT
res_reg <- find_anomalous_status(test_data,result=reg_result,anomaly_window = 1,anomalythreshold_len = 4)
res_reg[res_reg==TRUE]

res_neu <- find_anomalous_status(test_data,result=neural_result,anomaly_window = 3,anomalythreshold_len = 12)
res_neu[res_neu==TRUE]

res_neu <- find_anomalous_status_without_rle_logic(test_data,result=neural_result,anomaly_window = 3,anomalythreshold_len = 10,rle_logic=FALSE)
res_neu[res_neu==TRUE]



# LOCALIZE ANOMALY PART
appliance_feature_file <- fread("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_applianceRatings_corrected.csv")
appliance_features <- appliance_feature_file[V1==file1][,2:dim(appliance_feature_file)[2]]
anom_loc <- find_anomaly_appliance_from_livedata(test_data, result = neural_result, anom_status = res_neu, applince_features,window_minutes = 5)

daywise_anomaly_inspection(home_details,anom_loc)



#INCREMENTAL LOF APPROACH
lof_ans <- incremental_lof_procedure(train_data,test_data,hourwindow = 3, daywindow = 20)
pathy <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
anom_readings <- lof_ans[lof_ans > 0.90]
write.csv(x = fortify(anom_readings),file = paste0(pathy,"Lof_",file1),row.names = FALSE)


resp <- incremental_multiuser_procedure(train_data$power,test_data$power,hourwindow = 3, daywindow = 20)
anom_readings <- resp[resp > 0.90]
