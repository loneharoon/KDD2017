

compute_predictionband <- function(datas2_copy,predvalue,predictionband){
  # used to compute prediction band for neural networks setup
  df <- datas2_copy
  df <- df[!colnames(df)%in%c("power","temperature","humidity")]
  #mean_val <- rowMeans(df,na.rm = TRUE)
  sd_val <- apply(df,1,sd)
  dfs <- data.frame(lwr = predvalue - (predictionband * sd_val),upr = predvalue + (predictionband * sd_val))
  return(dfs)
}

compute_regression_model <- function(train_data,test_days,windowsize) {
  #browser()
  #library(MASS)
  #get train data acc to test day nature(train or test day)
  # train_data <- separate_weekend_weekday(train_data,test_days) 
  split_train <- split.xts(train_data,f="days",k=1)
  prac_day <- xts::last(split_train)
  temp <- tail(split_train,12)
  temp <- temp[1:length(temp)-1] # recent seven without last one, already used for testing
  xdat <- create_feature_matrix(temp)
  colnames(xdat) <- paste0('D',dim(xdat)[2]:1)
  #pdatsplit <- split(prac_day[[1]],lubridate::hour(index(prac_day[[1]])))
  pdatsplit <- split_hourwise(prac_day[[1]],windowsize)
  
  
  hhmodels <- list()
  for (i in 1:length(pdatsplit)) {
    #browser()
    testinterval <- pdatsplit[[i]]
    #next lines ensures that all values are not same otherwise scale function fails
    testinterval$temperature <- testinterval$temperature + rnorm(NROW(testinterval),0.01,0.01)
    testinterval$humidity <- testinterval$humidity + rnorm(NROW(testinterval),0.01,0.01)
    temp <- xdat[lubridate::hour(xdat) %in% unique(lubridate::hour(testinterval))]
    stat <- apply(temp,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp <- temp[,!stat]
    # browser()
    print(last(index(testinterval)))
    temp_N_anom <- get_selected_nonanomalousdays(temp)
    #temp_N_anom <- temp
    datas <- cbind(coredata(temp_N_anom),coredata(testinterval))
    datas <- as.data.frame(datas)
    datas_temp <- scale(datas[,!colnames(datas)%in% c("power")]) # scaling only regressors
    datas_temp <- cbind(datas_temp,power=datas$power)# combining regressor and predictor
    modelformula <- as.formula(log(power) ~  D1 + D2 + D3 + D4 + D5 + temperature + humidity)
    # print(paste0("Rl1:",i))
    # browser()
    hhmodels[[i]] <- lm(modelformula, 
                        data = as.data.frame(datas_temp), na.action = na.exclude)
  }
  print("regression trainin done")
  #browser()
  #  NOW PREDICT FOR ACTUAL TEST DAY
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  temp2 <- tail(split_train,9)
  xdat2 <- create_feature_matrix(temp2)
  colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  
  ydatsplit <- split_hourwise(ydat,windowsize)
  pred_value <- list()
  for (i in 1:length(ydatsplit)) {
    #browser()
    testinterval2 <- ydatsplit[[i]]
    testinterval2$temperature <- testinterval2$temperature + rnorm(NROW(testinterval2),0.01,0.01)
    testinterval2$humidity <- testinterval2$humidity + rnorm(NROW(testinterval2),0.01,0.01)
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    stat <- apply(temp3,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp3 <- temp3[,!stat]
    temp3_N_anom <- get_selected_nonanomalousdays(temp3) # removing anomalous days
    #temp3_N_anom <- temp3
    datas2 <- cbind(coredata(temp3_N_anom),coredata(testinterval2))
    datas2 <- as.data.frame(datas2)
    datas_temp2 <- scale(datas2[,!colnames(datas2)%in% c("power")]) # scaling only regressors
    datas_temp2 <- cbind(datas_temp2,power=datas2$power)# combining regressor and predictor
    # browser()
    #print(paste0("Rl2:",i))
    pred_value[[i]] <- predict(hhmodels[[i]], newdata = as.data.frame(datas_temp2),interval="prediction",level=0.95)
    print(paste0("prediction done",last(index(testinterval2))))
    pred_value[[i]] <- exp(pred_value[[i]])
    
  }
  pred_energy <- xts(do.call(rbind,pred_value),index(ydat))
  #pred_energy <- xts(unlist(pred_value),index(ydat))
  return(pred_energy)
}


compute_averaging_model <- function(train_data,test_days) {
  # this function predicts usage by averaging on past usage
  split_train <- split.xts(train_data,f="days",k=1)
  split_train <- tail(split_train,6)
  xdat <- create_feature_matrix(split_train)
  colnames(xdat) <- paste0('D',dim(xdat)[2]:1)
  
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  ydatsplit <- split(ydat,lubridate::hour(index(ydat)))
  pred_value <- list()
  for (i in 1:length(ydatsplit)) {
    # browser()
    testinterval2 <- ydatsplit[[i]]
    testinterval2$temperature <- testinterval2$temperature + rnorm(NROW(testinterval2),0.01,0.01)
    temp3 <- xdat[lubridate::hour(xdat) %in% unique(lubridate::hour(testinterval2))]
    pred_value[[i]] <- rowMeans(temp3,na.rm = TRUE)
  }
  pred_energy <- xts(unlist(pred_value),index(ydat))
  return(pred_energy)
}

compute_recurrent_Neural_model <- function(train_data,test_days,windowsize,trainingdays) {
  #browser()
  #get train data acc to test day nature(train or test day)
  # train_data <- separate_weekend_weekday(train_data,test_days) 
  library(rnn)
  split_train <- split.xts(train_data,f="days",k=1)
  prac_day <- xts::last(split_train)
  temp <- tail(split_train,trainingdays)
  temp <- temp[1:length(temp)-1] # recent seven without last one, already used for testing
  xdat <- create_feature_matrix(temp)
  colnames(xdat) <- paste0('D',dim(xdat)[2]:1)
  #pdatsplit <- split(prac_day[[1]],lubridate::hour(index(prac_day[[1]])))
  pdatsplit <- split_hourwise(prac_day[[1]],windowsize)
  
  hhmodels <- list()
  sel_columns <- c(paste0('D',1:4),'temperature','humidity')
  for (i in 1:length(pdatsplit)) {
    #browser()
    testinterval <- pdatsplit[[i]]
    #next lines ensures that all values are not same otherwise scale function fails
    testinterval$temperature <- testinterval$temperature + rnorm(NROW(testinterval),0.01,0.01)
    testinterval$humidity <- testinterval$humidity + rnorm(NROW(testinterval),0.01,0.01)
    temp <- xdat[lubridate::hour(xdat) %in% unique(lubridate::hour(testinterval))]
    # browser()
    temp_N_anom <- get_selected_nonanomalousdays(temp)
    datas <- cbind(coredata(temp_N_anom),coredata(testinterval))
    datas <- as.data.frame(datas)
    datas_temp <- datas[,!colnames(datas)%in% c("power")] # scaling only regressors
    #datas_temp <- scale(datas[,!colnames(datas)%in% c("power")]) # scaling only regressors
    
    #browser()
    datas_temp <- datas_temp[,sel_columns]
    #datas_temp <- t(datas_temp)
    #response <- t(datas$power)
    #response <- datas$power
    browser()
    tx <- array(datas_temp,dim=c(NROW(datas_temp),1,NCOL(datas_temp)))
    tx <- array(datas_temp,dim=c(dim(datas_temp),NCOL(datas_temp)))
    ty <- array(response,dim=c(NROW(response),1,NCOL(response)))
    
    hhmodels[[i]] <- trainr(X=tx,Y=ty,learningrate = 0.05, hidden_dim = 16, numepochs = 1000)
  }
  
  #  NOW PREDICT FOR ACTUAL TEST DAY
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  temp2 <- tail(split_train,trainingdays)
  xdat2 <- create_feature_matrix(temp2)
  colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  ydatsplit <- split_hourwise(ydat,windowsize)
  
  pred_value <- list()
  for (i in 1:length(ydatsplit)) {
    #browser()
    testinterval2 <- ydatsplit[[i]]
    testinterval2$temperature <- testinterval2$temperature + rnorm(NROW(testinterval2),0.01,0.01)
    testinterval2$humidity <- testinterval2$humidity + rnorm(NROW(testinterval2),0.01,0.01)
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    temp3_N_anom <- get_selected_nonanomalousdays(temp3) # removing anomalous days
    datas2 <- cbind(coredata(temp3_N_anom),coredata(testinterval2))
    datas2 <- as.data.frame(datas2)
    #datas_temp2 <- scale(datas2[,!colnames(datas2)%in% c("power")]) # scaling only regressors
    datas_temp2 <- datas2[,!colnames(datas2)%in% c("power")] # scaling only regressors
    temp <- datas_temp2[,sel_columns]
    temp <- t(temp)
    #response <- t(datas$power)
    tz <- array(temp,dim=c(NROW(temp),1,NCOL(temp)))
    pred_value[[i]] <- predictr(hhmodels[[i]],tz)
    
  }
  pred_energy <- xts(unlist(pred_value),index(ydat))
  return(pred_energy)
}

compute_LOF_scores <- function(train_data,test_days,windowsize,trainingdays) {
  # for each window (hour or complete day), this function calls LOF and computes anomaly score
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  split_train <- split.xts(train_data,f="days",k=1)
  temp2 <- tail(split_train,trainingdays)
  xdat2 <- create_feature_matrix(temp2)
  colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  ydatsplit <- split_hourwise(ydat,windowsize)
  
  lof_score_xts <- list()
  for (i in 1:length(ydatsplit)) {
    #browser()
    testinterval2 <- ydatsplit[[i]]
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    temp3_update <- cbind(temp3,coredata(testinterval2$power))
    lof_score <- return_LOF_anomaly_scores(temp3_update)
    lof_score_xts[[i]] <- xts(lof_score,last(index(testinterval2)))
  }
  combine_scores <- do.call(rbind,lof_score_xts)
  return(combine_scores)
}

compute_Neural_model <- function(train_data,test_days,windowsize,trainingdays) {
  #browser()
  #get train data acc to test day nature(train or test day)
  # train_data <- separate_weekend_weekday(train_data,test_days) 
  library(caret)
  split_train <- split.xts(train_data,f="days",k = 1)
  prac_day <- xts::last(split_train)
  temp <- tail(split_train,trainingdays)
  temp <- temp[1:length(temp)-1] # recent seven without last one, already used for testing
  xdat <- create_feature_matrix(temp)
  colnames(xdat) <- paste0('D',dim(xdat)[2]:1)
  #pdatsplit <- split(prac_day[[1]],lubridate::hour(index(prac_day[[1]])))
  pdatsplit <- split_hourwise(prac_day[[1]],windowsize)
  
  #browser()
  hhmodels <- list()
  for (i in 1:length(pdatsplit)) {
    #browser()
    testinterval <- pdatsplit[[i]]
    #next lines ensures that all values are not same otherwise scale function fails
    testinterval$temperature <- testinterval$temperature + rnorm(NROW(testinterval),0.01,0.01)
    testinterval$humidity <- testinterval$humidity + rnorm(NROW(testinterval),0.01,0.01)
    temp <- xdat[lubridate::hour(xdat) %in% unique(lubridate::hour(testinterval))]
    temp <- subset(temp,select = -D1) # temporary fix. otherwise code creates sometimes problems
    # browser()
    stat <- apply(temp,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp <- temp[,!stat]
    temp_N_anom <- get_selected_nonanomalousdays(temp)
    temp_N_anom <- temp_N_anom[,(dim(temp_N_anom)[2]):(dim(temp_N_anom)[2] - 5)] # USING ONLY 5 DAYS FOR TRAINING
    datas <- cbind(coredata(temp_N_anom),coredata(testinterval))
    datas <- as.data.frame(datas)
    datas_temp <- scale(datas[,!colnames(datas)%in% c("power")]) # scaling only regressors
    # datas_temp <- cbind(datas_temp,power=datas$power)# combining regressor and predictor
    #modelformula <- as.formula(log(power) ~  D1 + D2 + D3 +D4 + temperature + humidity)
    hhmodels[[i]] <- avNNet(x = datas_temp, y = datas$power, size = 10, decay = 0.05, 
                            linout = TRUE, maxit = 500)
  }
  print("NN training done")
  
  #  NOW PREDICT FOR ACTUAL TEST DAY
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  temp2 <- tail(split_train,trainingdays)
  xdat2 <- create_feature_matrix(temp2)
  colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  
  #ydatsplit <- split(ydat,lubridate::hour(index(ydat)))
  ydatsplit <- split_hourwise(ydat,windowsize)
  #browser()
  pred_value <- list()
  for (i in 1:length(ydatsplit)) {
    #browser()
    testinterval2 <- ydatsplit[[i]]
    testinterval2$temperature <- testinterval2$temperature + rnorm(NROW(testinterval2),0.01,0.01)
    testinterval2$humidity <- testinterval2$humidity + rnorm(NROW(testinterval2),0.01,0.01)
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    stat2 <- apply(temp3,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp3 <- temp3[,!stat2]
    temp3_N_anom <- get_selected_nonanomalousdays(temp3) # removing anomalous days
    temp3_N_anom <- temp3_N_anom[,(dim(temp3_N_anom)[2]):(dim(temp3_N_anom)[2] - 5)] #USING ONLY 5 DAYS FOR TRAINING
    if(NROW(temp3_N_anom)!=NROW(testinterval2)){
      print ("rows are not equal")
      browser()
    }
    datas2 <- cbind(coredata(temp3_N_anom),coredata(testinterval2))
    datas2 <- as.data.frame(datas2)
    datas2_copy <- datas2  # replica used afterwards
    datas_temp2 <- scale(datas2[,!colnames(datas2)%in% c("power")]) # scaling only regressors
    # datas_temp2 <- cbind(datas_temp2,power=datas2$power)# combining regressor and predictor
    # browser()
    print(last(index(testinterval2)))
    pred_value[[i]] <- predict(hhmodels[[i]], newdata = datas_temp2)
    pred_value[[i]] <- ifelse(pred_value[[i]]<0,100+rnorm(1,2,2),pred_value[[i]])
    bands <- compute_predictionband(datas2_copy,pred_value[[i]],2) #computing prediction bands
    pred_value[[i]] <- cbind(fit=pred_value[[i]],bands)
    
  }
  pred_energy <- xts(do.call(rbind,pred_value),index(ydat))
  #pred_energy <- xts(unlist(pred_value),index(ydat))
  return(pred_energy)
}

create_weather_power_object<- function(path1,file1,file2) {
  
  #path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/"
  #file1 = "ap601_twomonths.csv"
  #file2 = "delhi_weatherData.csv"
  powerdata<- fread(paste0(path1,file1),header=TRUE,sep=",")
  #xx<- read.csv(paste0(path,file1),header=TRUE,sep=",")
  weatherdata <- fread(file2,header=TRUE,sep=",")
  
  power_data <- xts(powerdata$power,as.POSIXct(strptime(powerdata$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  names(power_data)="power"
  weather_data<- xts(data.frame(temperature=weatherdata$TemperatureC,humidity=as.numeric(weatherdata$Humidity)),order.by = as.POSIXct(weatherdata$timestamp,origin="1970-01-01"))
  
  return(list(power_data=power_data,weather_data=weather_data)) 
} 

create_weather_power_object_fromAggDataport <- function(path1,file1,file2) {
  #browser()
  powerdata<- fread(paste0(path1,file1),header=TRUE,sep=",")
  #xx<- read.csv(paste0(path,file1),header=TRUE,sep=",")
  weatherdata <- fread(file2,header=TRUE,sep=",")
  
  power_data <- xts(powerdata$use,as.POSIXct(strptime(powerdata$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  names(power_data)="power"
  weather_data<- xts(data.frame(temperature=weatherdata$TemperatureC,humidity=as.numeric(weatherdata$Humidity)),order.by = as.POSIXct(weatherdata$timestamp,origin="1970-01-01"))
  
  return(list(power_data=power_data,weather_data=weather_data)) 
}

create_weather_power_object_from_ECO_dataset <- function(path1,file1,file2) {
  
  powerdata<- fread(paste0(path1,file1),header=TRUE,sep=",")
  #xx<- read.csv(paste0(path,file1),header=TRUE,sep=",")
  weatherdata <- fread(file2,header=TRUE,sep=",")
  
  power_data <- xts(powerdata$total,as.POSIXct(strptime(powerdata$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  names(power_data)="power"
  weather_data<- xts(data.frame(temperature=weatherdata$TemperatureC,humidity=as.numeric(weatherdata$Humidity)),order.by = as.POSIXct(weatherdata$timestamp,origin="1970-01-01"))
  
  return(list(power_data=power_data,weather_data=weather_data)) 
}

create_weather_power_object_from_REFIT_dataset <- function(path1,file1,file2) {
  
  powerdata<- fread(paste0(path1,file1),header=TRUE,sep=",")
  #xx<- read.csv(paste0(path,file1),header=TRUE,sep=",")
  weatherdata <- fread(file2,header=TRUE,sep=",")
  
  power_data <- xts(powerdata$Aggregate,as.POSIXct(strptime(powerdata$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  names(power_data)="power"
  
  timerange <- seq( first(index(power_data)), last(index(power_data)), by ="10 mins")
  temp = xts(rep(NA,length(timerange)),timerange)
  complete_xts = merge(power_data,temp)[,1]
  power_data_complete <- na.approx(complete_xts)
  #browser()
  weather_data<- xts(data.frame(temperature=weatherdata$TemperatureC,humidity=as.numeric(weatherdata$Humidity)),order.by = as.POSIXct(weatherdata$timestamp,origin="1970-01-01"))
  
  return(list(power_data = power_data_complete, weather_data = weather_data)) 
}


create_feature_matrix <- function(tdata) {
  #daydata <- split.xts(tdata,f="days",k=1)
  #sapply(daydata)
  matdata <- as.data.frame(sapply(tdata,function(x) coredata(x$power)))
  mat_xts <- xts(matdata,index(tdata[[1]]))
  return(mat_xts)
}


confirm_validity <- function(data_ob,merge_start_date,merge_end_date){
  if( (merge_start_date < start(data_ob$power_data)) || (merge_end_date > end(data_ob$power_data) ))
    stop("Power data within this range does't exists!!")
  if( (merge_start_date < start(data_ob$weather_data)) || (merge_end_date > end(data_ob$weather_data) ))
    stop("weather data within thisrange does't exists!!")
}

combine_energy_weather <- function(data_ob,my_range){
  # this functions simply cbinds energy and weather data
  power_data <- data_ob$power_data
  weather_data <- data_ob$weather_data
  subpower_data <- power_data[my_range]
  subweather_data <- weather_data[my_range]
  merge_data <- merge(subpower_data,subweather_data)
  no_na_data <- round(na.approx(merge_data),2) # remove NA
  return(no_na_data)
}

combine_energy_weather_ECOdata <- function(data_ob,my_range){
  # this functions simply cbinds energy and weather data
  power_data <- data_ob$power_data
  weather_data <- data_ob$weather_data
  subpower_data <- power_data[my_range]
  subweather_data <- weather_data[my_range]
  merge_data <- merge(subpower_data,subweather_data)
  merge_data <-  merge_data[!is.na(merge_data$power)]# EXPLICITLY DONE TO HANDLE MISSING POWER VALUES OF HOUSE # 5
  no_na_data <- round(na.approx(merge_data),2) # remove NA
  return(no_na_data)
}

compute_prediction_accuracy_onDatasets <- function() {
  # FOR DATAPORT
  paths <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/"
  file <- "prediction_result_dataport.csv"
  df <- fread(paste0(paths,file))
  nnet <- df[grep("NNet",df$V1),]
  regres <- df[grep("Regression",df$V1),]
  apply(regres[,2:4],2,mean)
  apply(nnet[,2:4],2,mean)
  
  # FOR AMPDS
  file <- "prediction_result_dataport.csv"
  df <- fread(paste0(paths,file))
  nnet <- df[grep("NNet",df$V1),]
  regres <- df[grep("Regression",df$V1),]
  apply(regres[,2:4],2,mean)
  apply(nnet[,2:4],2,mean)
  
  # FOR REFIT
  file <- "prediction_result_REFIT_dataset.csv"
  df <- fread(paste0(paths,file))
  nnet <- df[grep("NNet",df$V1),]
  regres <- df[grep("Regression",df$V1),]
  apply(regres[,2:4],2,mean)
  apply(nnet[,2:4],2,mean)
  
  # FOR ECO
  file <- "prediction_result_eco_dataset.csv"
  df <- fread(paste0(paths,file))
  nnet <- df[grep("NNet",df$V1),]
  regres <- df[grep("Regression",df$V1),]
  apply(regres[,2:4],2,mean,na.rm=TRUE)
  apply(nnet[,2:4],2,mean,na.rm=TRUE)
  
}

create_appliance_rating_file <- function(){
  # this stores appliance ratings of dataprot houses in a single file. Next step: Please plot each home separtely and edit this  file acc. to VI 
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  fls <- mixedsort(list.files(path1,pattern = ".csv"))
  #file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  
  home_appliances <- list()
  
  #for (i in 1:length(fls)) {
  for (i in 1:length(fls)) {
    file1 <- fls[i]
    #data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
    #appliance_features <- get_appliance_features(path1, file1)
    df <- fread(paste0(path1,file1))
    df_xts <- xts(df[,2:dim(df)[2]],as.POSIXct(strptime(df$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
    appliances <- subset(df_xts,select=-use)
    app_rating <- sort(apply(appliances,2,max),decreasing = TRUE)
    home_appliances[[file1]] <- t(as.data.frame(app_rating))
    
  }
  
  home_applinace_df <- do.call(gtools::smartbind,home_appliances)
  write.csv(home_app2,file="dport_applianceRatings.csv")
  
}

metric_SMAPE <- function(object){
  numer <- sum(abs(object$fit-object$actual))
  denom <- sum(abs(object$fit)+abs(object$actual))
  smape <- numer/denom
  return (smape)
}

metric_MASE <- function(forecast_ob){
  # https://en.wikipedia.org/wiki/Mean_absolute_scaled_error 
  numerator <- abs(forecast_ob$actual - forecast_ob$fit)
  naive_error <- 0
  multiplier <- NROW(forecast_ob)/NROW(forecast_ob-1)
  for(i in 2:NROW(forecast_ob)){
    naive_difference <- abs(coredata(forecast_ob$actual[i]) - coredata(forecast_ob$actual[i-1]))
    naive_error <- naive_error + naive_difference
  }
  metric_val <- as.numeric(sum(numerator)/(multiplier*naive_error))
  return(metric_val)
}

metric_RMSE <- function(object) {
  error <- object$fit - object$actual
  return(sqrt(mean(error^2)))
}



get_selected_nonanomalousdays <- function(df_matrix) {
  
  # this function takes input matrix of energy consumption of n days and outputs selected days non anomalous days
  # browser()
  library(Rlof) # FOR LOF
  library(HighDimOut)
  df_matrix <- t(df_matrix)# transpose as it works row wise
  dist_matrix <- dist(df_matrix, method = "euclidean", diag = TRUE, upper = TRUE)
  fit <- cmdscale(dist_matrix, eig = TRUE, k = 2)
  daymat <- fit$points
  #print(row.names(daymat)[1:2])
  k <- NROW(daymat)
  dfLof <- lof(daymat, c((k-2):(k-1)), cores = 2)
  #dfLof <- lof(daymat, c(4:5), cores = 2)
  dfLofNormalized <- apply(dfLof,2,function(x) Func.trans(x,method = "FBOD"))
  FinalAnomScore <-  apply(dfLofNormalized,1,function(x) round(max(x,na.rm = TRUE),2) )#feature bagging for outlier detection
  FinalAnomScore <- data.frame(index = rownames(daymat), score = FinalAnomScore)
  FinalAnomScore$status <- FinalAnomScore$score >= 0.80
  AnomDays <- as.character(FinalAnomScore[FinalAnomScore$score >= 0.80,]$index)
  mats <- data.frame(df_matrix) # converting matrix to data frame
  
  if(length(AnomDays)!= 0){
    dbSubset <- mats[-which(rownames(mats) %in% AnomDays),] # remove anomaloous days from further operations
  }else{
    dbSubset <- mats
  }
  dbSubset <- t(dbSubset)
  colnames(dbSubset) <- paste0('D',dim(dbSubset)[2]:1)
  return(dbSubset)
}


split_hourwise <- function(tempday,windowsize) {
  #http://stackoverflow.com/a/41765212/3317829
  #tempday <- prac_day[[1]]
  .index(tempday) <- .index(tempday) - 60*30
  tempx <-split.xts(tempday,"hours",k=windowsize)
  tt <-lapply(tempx,function(x) {.index(x) <- .index(x) + 60*30;  return (x)})
  return(tt)
}

separate_weekend_weekday<- function(train_data,test_days) {
  # this function splits traindata into weekday or weekend according to the test day. if testday is weekend then only weekend training days are used 
  ydat <- split.xts(test_days,"days",k=1)[[1]]
  daytype <- unique(lubridate::wday(ydat)) #(1-> sunday, 7-> saturday)
  train_data$wday <- lubridate::wday(train_data)
  if(daytype == 1 | daytype == 7) { # saturday or sunday
    newtraindata <- train_data[train_data$wday %in% c(1,7)]
  } else {
    newtraindata <- train_data[!train_data$wday %in% c(1,7)]
  }
  return(newtraindata[,-4]) # removing wday column
}



regression_procedure <- function(train_data,test_data,hourwindow){
  days <- as.numeric( last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  #days <- length(unique(lubridate::day(index(test_data))))
  result <- list()
  for (i in 1:days) {
    # third parameter represents the length(hours) of window
    result[[i]] <- compute_regression_model(train_data,test_data,hourwindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  finresult <- do.call(rbind,result)
  return(finresult)
}

neuralnetwork_procedure <- function(train_data,test_data,hourwindow,daywindow){
  #days <- length(unique(lubridate::day(index(test_data))))
  days <- as.numeric( last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  result <- list()
  for (i in 1:days) {
   # browser()
    result[[i]] <- compute_Neural_model(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  #browser()
  finresult <- do.call(rbind,result)
  return(finresult)
}

plot_graph <- function(test_data,reg_result,neural_result) {
  # takes input data from regression and neural network predictions
  library(plotly)
  res <- cbind(actual = test_data$power, regression = reg_result, neural = neural_result)
  long <- reshape2::melt(fortify(res),id.vars="Index")
  p <- ggplot(long,aes(Index,value,col=variable)) + geom_line()
  ggplotly(p)
}

plot_withhistoricaldata_graph <- function(train_data, test_data,reg_result=NULL,neural_result=NULL) {
  # input: data from regression and neural network predictions
  # input: train data(historical data), and test data of which we have predictions
  # output: ggplot with three lines
  library(plotly)
  # setting training data of some historical days for which we don't have prediction
  traindat <- split.xts(train_data,f="days",k=1)
  train_temp <- do.call(rbind,tail(traindat,5))
  if(is.null(reg_result)){
    reg_result <- test_data}
  if(is.null(neural_result)) {
    neural_result = test_data$power
    }
  train <- cbind(actual = train_temp$power, regression = rep(NA,NROW(train_temp)), neural = rep(NA,NROW(train_temp))) 
  
  # setting predicted values
  res <- cbind(actual = test_data$power, regression = reg_result, neural = neural_result)
  res <- rbind(train,res) # append res to train
  long <- reshape2::melt(fortify(res),id.vars="Index")
  p <- ggplot(long,aes(Index,value,col=variable)) + geom_line()
  ggplotly(p)
}

plot_table <- function(test_data, reg_result, neural_result, intent){
  # this function lists all the prediction results
  #1 Regression  result
  # intent shows whethe we want to print or we want to return
  resobject <- cbind(reg_result,test_data$power)
  colnames(resobject) <- c("fit","actual")
  reg_list <- list(smape = metric_SMAPE(resobject), rmse = metric_RMSE(resobject), mase = metric_MASE(resobject))
  #2 Neural network result
  resobject <- cbind(neural_result,test_data$power)
  colnames(resobject) <- c("fit","actual")
  neural_list <- list(smape = metric_SMAPE(resobject), rmse = metric_RMSE(resobject), mase = metric_MASE(resobject))
  # Combine results
  #browser()
    tab <- rbind(unlist(reg_list),unlist(neural_list))
  rownames(tab) <- c("Regression","NNet")
  if(intent=="print"){
  print(round(tab,3))
  } else{
    return(round(tab,3))
  }
}


plot_singlepredictor_graph <- function(train_data, test_data,pred_result=NULL) {
  # takes input data from regression and neural network predictions
  library(plotly)
  # setting training data of some historical days
 # browser()
  traindat <- split.xts(train_data,f="days",k=1)
  train_temp <- do.call(rbind,tail(traindat,5))
  train <- cbind(actual = train_temp$power, prediction = rep(NA,NROW(train_temp))) 
  
  # setting predicted values
  res <- cbind(actual = test_data$power, prediction = pred_result$fit)
  res <- rbind(train,res) # append res to train
  long <- reshape2::melt(fortify(res),id.vars="Index")
  p <- ggplot(long,aes(Index,value,col=variable)) + geom_line()
  p <- p + scale_x_datetime(date_breaks="1 day")
  ggplotly(p)
}

plot_anomalous_days_with_recent_normaldays_kdd_paper <- function() {
  library(plotly)
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/KDD_2017/plots/")
  #path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
  
  filename <- "house4_10min.csv"
  inspec_date <- "2012-09-24"
  sel_date <- "2012-08-13/2012-08-18"
  df <- fread(paste0(path1,filename))
  df_xts <- xts(df[,2:dim(df)[2]], fastPOSIXct(df$localminute)-19800)
  sel_data <- df_xts[sel_date]
  spec_data <- df_xts[inspec_date]
  plot_anomalous_day(filename,inspec_date,sel_data)
  daywise_visualization(spec_data)
  
  plot_anomalous_day <- function(filename,inspec_date,sel_data) {
    use_data <- sel_data$total
    day_data <- split.xts(use_data,f="days",k=1)
    day_mat <- lapply(day_data,function(x) coredata(x))
    day_mat <- as.data.frame(do.call(cbind,day_mat))
    colnames(day_mat) <- paste0("c",1:dim(day_mat)[2])
    day_mat$timestamp <- index(day_data[[1]])
    df_long <- reshape2::melt(day_mat,id.vars="timestamp")
    
    g <- ggplot(df_long,aes(timestamp,value/1000,group=variable,alpha=variable)) + geom_line()
    g <- g + scale_alpha_manual(values = c(rep(0.2,dim(day_mat)[2]-2),1))
    g <- g + theme(legend.position="none",axis.text=element_text(color="black",size=10)) + scale_x_datetime(labels=date_format("%H:%M",tz="Asia/kolkata")) + labs(x= "Hour of the day", y = "Power (kW)")
    savename <- paste0(inspec_date,"_",strsplit(filename,'[.]')[[1]][1],".pdf")
    ggsave(file = savename,width = 3, height = 3)
  }
  
  
  
  daywise_visualization <- function(plot_data) {
    # VISUALIZE PORTION OF DATA ON GIVEN DATE
    #http://novyden.blogspot.in/2013/09/how-to-expand-color-palette-with-ggplot.html
    
    dframe <- plot_data
    dframe <- data.frame(timeindex = index(dframe), coredata(dframe))
    # dframe$dataid <- NULL ; dframe$air1 <-NULL ; dframe$use<- NULL ; dframe$drye1 <- NULL
    df_long <- reshape2::melt(dframe,id.vars = "timeindex")
    colourCount = length(unique(df_long$variable))
    getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount) # brewer.pal(8, "Dark2") or brewer.pal(9, "Set1")
    g <- ggplot(df_long,aes(timeindex,value,col=variable,group=variable))
    g <- g + geom_line() + scale_colour_manual(values=getPalette)
    ggplotly(g)
  }
}

recurrent_neural_procedure <- function(train_data,test_data,hourwindow,daywindow){
  days <- length(unique(lubridate::day(index(test_data))))
  result <- list()
  for (i in 1:days) {
    result[[i]] <- compute_recurrent_Neural_model(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  finresult <- do.call(rbind,result)
  return(finresult)
}



find_anomalous_status <- function(test_data,result,anomaly_window,anomalythreshold_len){
  # this checks if the anomaly_window (in terms of hours) contain anomaly of specific time duration (defined by anomalythreshold_len (minutes 4 mean 40))
  #browser()
  suppressWarnings(rm("status"))
  df_comp  <- cbind(result,test_data)
  df_days <- split.xts(df_comp,f="days",k=1) # daywise dataframe
  pastanomaly_vec <- as.vector("numeric") # keeps track of possible anomalies in last window
  # Loop i: Divides in terms of days
  for( i in 1:length(df_days)) {
    # Loop j: Divides in terms of anomaly window [one hour or 2 hour etc]
    day_hour <- split_hourwise(df_days[[i]],windowsize = anomaly_window)
    for( j in 1:length(day_hour)) {
      decision_vector <- as.vector(ifelse(day_hour[[j]]$power > day_hour[[j]]$upr,1,0))
      decision_vector <- c(pastanomaly_vec,decision_vector)
      length_list <- rle(decision_vector) # run length encoding
      is_anom <- any(length_list$lengths >= anomalythreshold_len & length_list$values == 1) #http://stackover
      #Logic to store only meaningful information from this window for the subsequent future window
      cnt <- 0; lcs <- 0 #last common sequence with status as 1
      last <- decision_vector[length(decision_vector) - cnt]
      while(last == 1){
        lcs <- lcs + 1
        cnt <- cnt +1
        if(lcs >= anomalythreshold_len)
          break
        last <- decision_vector[length(decision_vector)-cnt]
      }
      pastanomaly_vec <- rep(1,lcs)
      if(length(pastanomaly_vec) >= anomalythreshold_len)
        pastanomaly_vec <- as.numeric() # this sequence is already detected in current
      # create anomaly status frame
      if(!exists("status")){
        status <- xts(is_anom,index(last(day_hour[[j]])))
      } else {
        temp <- xts(is_anom,index(last(day_hour[[j]])))
        status <- rbind(status,temp)
      }
    }
  }
  return(status)
} 


find_anomalous_status_without_rle_logic <- function(test_data,result,anomaly_window,anomalythreshold_len,rle_logic=FALSE){
  # this checks if the anomaly_window (in terms of hours) contain anomaly of specific time duration (defined by anomalythreshold_len (minutes 4 mean 40))
 # browser()
  suppressWarnings(rm("status"))
  df_comp  <- cbind(result,test_data)
  df_days <- split.xts(df_comp,f="days",k=1) # daywise dataframe
  pastanomaly_vec <- as.vector("numeric") # keeps track of possible anomalies in last window
  # Loop i: Divides in terms of days
  for( i in 1:length(df_days)) {
    # Loop j: Divides in terms of anomaly window [one hour or 2 hour etc]
    day_hour <- split_hourwise(df_days[[i]],windowsize = anomaly_window)
    for( j in 1:length(day_hour)) {
      decision_vector <- as.vector(ifelse(day_hour[[j]]$power > day_hour[[j]]$upr,1,0))
      decision_vector <- c(pastanomaly_vec,decision_vector)
      length_list <- rle(decision_vector) # run length encoding
      if(rle_logic){
        is_anom <- any(length_list$lengths >= anomalythreshold_len & length_list$values == 1) #http://stackover
      }else{
        p <- table(decision_vector)
        p <- as.numeric(p['1'])
        is_anom <- p >= anomalythreshold_len
      }
      #Logic to store only meaningful information from this window for the subsequent future window
      cnt <- 0; lcs <- 0 #last common sequence with status as 1
      last <- decision_vector[length(decision_vector) - cnt]
      while(last == 1){
        lcs <- lcs + 1
        cnt <- cnt +1
        if(lcs >= anomalythreshold_len)
          break
        last <- decision_vector[length(decision_vector)-cnt]
      }
      pastanomaly_vec <- rep(1,lcs)
      if(length(pastanomaly_vec) >= anomalythreshold_len)
        pastanomaly_vec <- as.numeric() # this sequence is already detected in current
      # create anomaly status frame
      if(!exists("status")){
        status <- xts(is_anom,index(last(day_hour[[j]])))
      } else {
        temp <- xts(is_anom,index(last(day_hour[[j]])))
        status <- rbind(status,temp)
      }
    }
    
  }
  return(status)
} 




get_appliance_features <- function (path1,file1) {
   # this function will return both appliane rating and the the dissagrigated energy of a home
  powerdata <- fread(paste0(path1,file1),header=TRUE,sep=",")
  powerdata_xts <- xts(powerdata[,2:dim(powerdata)[2]],as.POSIXct(strptime(powerdata$localminute,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 

  appliance_data_xts <- subset(powerdata_xts,select = -c(use))
  app_rating <- sort(apply(appliance_data_xts,2,max),decreasing = TRUE)

  return(list(home_data = powerdata_xts, appliance_rating = app_rating))
  
}


find_anomaly_appliance_from_livedata <- function(test_data, result, anom_status, appliance_features, window_minutes) {
 # appliance_features <- home_details$appliance_rating
  # anom_status <- res_neu
  #  result <- neural_result
  day_dat <- split.xts(anom_status,"days",k=1)
  rm("final_result")
  # find anomalies in different intervals
  for (i in 1:length(day_dat)) {
    df <- day_dat[[i]]
    # times of day at which anomaly was found
    anom_intervals <- index(df[df %in% c("TRUE"),] )
    if(length(anom_intervals) != 0) { # if there are anomalies
      for (j in 1:length(anom_intervals)) { # for each anomaly interval
        # finding range of anomaly detection time interval
        upr_lmt <- index(test_data[anom_intervals[j],])
        lwr_lmt <- upr_lmt - 10  * 60 * window_minutes # last one hour
        actual <- test_data[paste0(lwr_lmt,'/',upr_lmt)]$power # actual power consumed
        baseline <- result[paste0(lwr_lmt,'/',upr_lmt)]$fit   # predicted power consumed
        # we know this interval is anomalous so actual will be much higher than the baseline
        delta <- baseline - actual
        magnitude <- abs(min(delta))
        # find the applaince to which negative numbers match most
        anom_app <- which(abs(appliance_features - magnitude) == min(abs(appliance_features - magnitude)))
        if(!exists("final_result")){
          final_result <- xts(names(anom_app),upr_lmt)
        } else{
          temp <- xts(names(anom_app),upr_lmt)
          final_result <- rbind(final_result,temp)
        }
      }
    }
  }
  return(final_result)
}

find_anomaly_appliance_with_stored_Results <- function(pred_results, anom_status, appliance_features, window_minutes) {
  # this function returns the name of possible anomalou applaince
  day_dat <- split.xts(anom_status,"days",k=1)
  suppressWarnings(rm("final_result"))
  # find anomalies in different intervals
  for (i in 1:length(day_dat)) {
    df <- day_dat[[i]]
    # times of day at which anomaly was found
    anom_intervals <- index(df[df %in% c("TRUE"),] )
    if(length(anom_intervals) != 0) { # if there are anomalies
      for (j in 1:length(anom_intervals)) { # for each anomaly interval
        # finding range of anomaly detection time interval
        upr_lmt <- index(pred_results[anom_intervals[j],])
        lwr_lmt <- upr_lmt - 10  * 60 * window_minutes # last one hour
        actual <- pred_results[paste0(lwr_lmt,'/',upr_lmt)]$power # actual power consumed
        baseline <- pred_results[paste0(lwr_lmt,'/',upr_lmt)]$fit   # predicted power consumed
        # we know this interval is anomalous so actual will be much higher than the baseline
        delta <- baseline - actual
        magnitude <- abs(min(delta))
        # find the applaince to which negative numbers match most
        temp <- abs(as.numeric(appliance_features) - magnitude)
        names(temp) <- names(appliance_features)
        anom_app <- which.min(temp)
        #anom_app <- which(abs(appliance_features - magnitude) == min(abs(appliance_features - magnitude)))
        if(!exists("final_result")){
          final_result <- xts(names(anom_app),upr_lmt)
        } else{
          temp <- xts(names(anom_app),upr_lmt)
          final_result <- rbind(final_result,temp)
        }
      }
    }
  }
  return(final_result)
}

find_regrassivedays_with_BIC <- function(regression_days) {
  # I assume that I will recieve more than 21 days of data
  # Using this function we find how many historical days should we use in regression
  # Lower BIC ensures optimal estimate 
  data <- regression_days
 # browser()
  daydat <- split.xts(data,"days")
  mat <- sapply(daydat,function(x) return(coredata(x$power)))
  df_x <- as.data.frame(mat)
  optimal_hist_day <- vector("numeric") # for each different test day, this will store how may historical should we use ideally 
  index <- 1
  for(day in 21:dim(df_x)[2]){ #  for different day of input data
    
    #index <- 1
    testday <- df_x[,day] 
    bic_vector <- vector("numeric")
    
    for (i in 1:(day-1)) { # for same day with different number of historical days
      df_temp <- data.frame(df_x[,(day-1):(day-i)])
      colnames(df_temp) <- paste0("D",dim(df_temp)[2]:1)
      df_temp$y <- testday
      mod <- lm(y ~.,df_temp)
      bic_vector[i] <- BIC(mod)
    }
    optimal_daylimit <- which(bic_vector == min(bic_vector)) # How many days I should consider for minimal bic
    optimal_hist_day[index] <- optimal_daylimit
    index <- index + 1
  }
  return(optimal_hist_day)
}

find_regrassivedays_matrix_with_BIC <- function(regression_days) {
  # I assume that I will recieve more than 21 days of data
  # Using this function we find how many historical days should we use in regression
  # Lower BIC ensures optimal estimate 
  data <- regression_days
  # browser()
  daydat <- split.xts(data,"days")
  mat <- sapply(daydat,function(x) return(coredata(x$power)))
  df_x <- as.data.frame(mat)
  optimal_hist_day <- vector("numeric") # for each different test day, this will store how may historical should we use ideally 
  index <- 1
  for(day in 21:dim(df_x)[2]){ #  for different day of input data
    
    #index <- 1
    testday <- df_x[,day] 
    bic_vector <- vector("numeric")
    
    for (i in 1:(day-1)) { # for same day with different number of historical days
      df_temp <- data.frame(df_x[,(day-1):(day-i)])
      colnames(df_temp) <- paste0("D",dim(df_temp)[2]:1)
      df_temp$y <- testday
      mod <- lm(y ~.,df_temp)
      bic_vector[i] <- BIC(mod)
    }
    #optimal_daylimit <- which(bic_vector == min(bic_vector)) # How many days I should consider for minimal bic
    #optimal_hist_day[index] <- optimal_daylimit
    #index <- index + 1
    if(!exists("bicmat")){
      bicmat <- bic_vector
    }else{
      bicmat <- rbind(bicmat,bic_vector)
    }  
    
    
  }
  return(bicmat)
}

incremental_lof_procedure <- function(train_data,test_data,hourwindow,daywindow){
  # hourwindow = chunk (#hours) for which LOf is run 
  # daywindow = no. of historical days used
  days <- as.numeric(last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  #days <- length(unique(lubridate::day(index(test_data))))
  result <- list()
  for (i in 1:days) {
    # third parameter represents the length(hours) of window
    result[[i]] <- compute_LOF_scores(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  finresult <- do.call(rbind,result)
  return(finresult)
}


twitter_anomaly_detection_implemenatation <- function() {
  # implemenatation of real-time anomaly detection paper
  
  #pathxx  <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  pathxx <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/mutated/"
  file1 <- "1037.csv"
  
  data_ob <- fread(paste0(pathxx,file1))
  dsub <- data.frame(timestamp = data_ob$localminute,count = data_ob$use)
  dsub$timestamp <- as.POSIXct(strptime(dsub$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")
  timelow <- "2014-06-01"
  timeup <- "2014-08-30 23:59:59"
  
  data_subset <- dsub[(dsub$timestamp >= timelow & dsub$timestamp <= timeup),]
  data_subset_xts <- xts(data_subset$count,data_subset$timestamp)
  system.time(
  results <-  incremental_twitter_anomdetection_procedure(data_subset_xts,20))
  
  
  incremental_twitter_anomdetection_procedure <- function(orig_data,train_days){
    #days <- as.numeric( last(as.Date(index(orig_data))) - first(as.Date(index(orig_data))) )
    #orig_data: input timetstamp and power dataframe
    #train_days: number (how many days for training)
    library(AnomalyDetection)
    result <- list()
    orig_data_days <- split.xts(orig_data, f="days",k=1)
    train_data <- orig_data_days[1:train_days]
    test_data <- orig_data_days[(train_days+1):(length(orig_data_days))]
    #browser()
    for (i in 1:length(test_data)) {
      train_data_use <- do.call(rbind,train_data[i:length(train_data)])
      test_day <- test_data[[i]]
      test_object <- fortify(rbind(train_data_use,test_day))
      result[[i]] <- AnomalyDetectionTs(test_object,only_last = "day")$anoms
      train_data[[length(train_data)+1]] <- test_day # update train data
    }
    finresult <- do.call(rbind,result)
    return(finresult)
  }
  
}

return_LOF_anomaly_scores <- function(df_matrix) {
  # this function takes input matrix of energy conusmption of n days and outputs anomaly score of last day as this runs in incremental fashion
  library(Rlof) # FOR LOF
  library(HighDimOut)
  df_matrix <- t(df_matrix)# transpose as it works row wise
  dist_matrix <- dist(df_matrix, method = "euclidean", diag = TRUE, upper = TRUE)
  fit <- cmdscale(dist_matrix, eig = TRUE, k = 2)
  daymat <- fit$points
  k <- NROW(daymat)
  dfLof <- lof(daymat, c((k-2):(k-1)), cores = 2)
  #dfLof <- lof(daymat, c(4:5), cores = 2)
  dfLofNormalized <- apply(dfLof,2,function(x) Func.trans(x,method = "FBOD"))
  FinalAnomScore <-  apply(dfLofNormalized,1,function(x) round(max(x,na.rm = TRUE),2) )#feature bagging for outlier detection
  return(last(FinalAnomScore))
}

print_single_prediction_method_table <- function(test_data, method_result){
  # this function lists all the prediction results
  #method_result <- reg_result$fit
  resobject <- cbind(method_result,test_data$power)
  colnames(resobject) <- c("fit","actual")
  reg_list <- list(smape = metric_SMAPE(resobject), rmse = metric_RMSE(resobject), mase = metric_MASE(resobject))
  #print results
  tab <- unlist(reg_list)
  print(round(tab,3))
}


averaging_Case <- function() {
  # in this approach, we predict by simple averaging of the past consumption
  train_data <- sampled_ob['2014-06-05/2014-06-25']
  test_data <- sampled_ob['2014-06-26/2014-06-28']
  backupcopy <- test_data
  days <- length(unique(lubridate::day(index(test_data))))
  for (i in 1:days) {
    # third parameter represents the length(hours) of window
    result[[i]] <- compute_averaging_model(train_data,test_data)
    #plot(result[[i]])
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  avgresult <- do.call(rbind,result)
  #res_avg <- compute_averaging_model(train_data,test_data)
  lines(avgresult,col="green")
}



daywise_anomaly_inspection <- function(home_details, anom_loc) {
  # browser()
  library("RColorBrewer")
  # VISUALIZE PORTION OF DATA ON GIVEN DATE
  #http://novyden.blogspot.in/2013/09/how-to-expand-color-palette-with-ggplot.html
  days <- unique(format(index(anom_loc),"%Y-%m-%d"))
  data_10min <- home_details
  cat(paste0("Anomalous days:: ",length(days)))
  for (i in 1:length(days)){
    #browser()
    cat("enter number")
    x = scan(what= numeric(),n =1)
    sel_date <- days[x]
    fulldata <- data_10min[paste0(as.POSIXct(paste0(sel_date," ",'00:00:00')),"/",as.POSIXct(paste0(sel_date," ",'23:59:59')))]
    dframe <- fulldata
    dframe <- data.frame(timeindex = index(dframe), coredata(dframe))
    # dframe$dataid <- NULL ; dframe$air1 <-NULL ; dframe$use<- NULL ; dframe$drye1 <- NULL
    df_long <- reshape2::melt(dframe,id.vars = "timeindex")
    colourCount = length(unique(df_long$variable))
    getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount) # brewer.pal(8, "Dark2") or brewer.pal(9, "Set1")
    #cat("rached g")
    g <- ggplot(df_long,aes(timeindex,value,col=variable,group=variable))
    g <- g + geom_line() + scale_colour_manual(values=getPalette) + ggtitle(as.Date(df_long$timeindex[15]))
    g <- g + scale_x_datetime(labels = date_format("%H",tz="Asia/Kolkata"), breaks = pretty_breaks(n=24))
    plot(g)
    #cat("loop done")
  }
}

sensitivity_analysis_KDD_window_size <- function() {
  # function used to plot senstivity analysis of anomaly detection window
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/KDD_2017/figures/")
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  pathxx <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_prediction/"
  save_path <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
  #fls <- mixedsort(list.files(pathxx,pattern = ".csv"))
  anom_threshold_length <- seq(2,10,by=1)
  file1 <- "115.csv"
  
  data_ob <- fread(paste0(pathxx,file1))
  data_xts <- xts(data_ob[,c("power","fit","lwr","upr")],as.POSIXct(strptime(data_ob$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01"))
  test_data_orig <- data_xts$power
  neu_result  <- subset(data_xts,select = -power)
  
  anom_readings_online <- list()
  for (i in 1:length(anom_window_length)) {
    anomaly_status <- find_anomalous_status(test_data=test_data_orig,result=neu_result,anomaly_window = 2,anomalythreshold_len = anom_threshold_length[i])
    anom_readings_online[[i]] <- anomaly_status[anomaly_status == TRUE]
    #  write.csv(x = fortify(anom_readings_online),file = paste0(save_path,"online_",file1),row.names = FALSE)
  }
  sapply(anom_readings_online, length)
  
  # now plotting time
  
  Home1 <- c(137,80,58,34,20,14,11,6,6) # 101.csv
  Home2 <- c(152,86,51,24,15,9,6,5,4) # 114.csv
  Home3 <- c(221,60,28,11,5,2,2,2,1) # 115.csv
  Minutes <- c(20,30,40,50,60,70,80,90,100)
  df <- data.frame(Minutes,Home1,Home2,Home3)
  # format data frame according to ggplot plotting library
  df_melt <- melt(df,id = "Minutes")
  g <- ggplot(df_melt,aes(Minutes,value,fill=variable)) + geom_bar(position="dodge",stat="identity")
  g <- g +  labs(x = "Anomaly threshold window,S (minutes) ", y="No. of detected anomalies") + theme_grey(base_size = 16) 
  g <- g + theme(axis.text = element_text(color="Black"),legend.position = c(0.80,0.80),legend.title=element_blank(),legend.background = element_rect(fill = alpha('white',0.3)),legend.text = element_text(size = 10)) + scale_x_continuous(breaks = seq(min(df$Minutes), max(df$Minutes), by = 10))
  g
  # next command saves your plot and you can specify the size as well
  # ggsave("threshold_anomaly_window.pdf", width = 4.5, height = 4) 
}

sensitivity_analysis_KDD_BIC_value <- function(){
  file1 <- "2094.csv"
  #house_no <- "house1_10min.csv"
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
  merge_start_date <- as.POSIXct(strptime('2014-06-05',format = "%Y-%m-%d"))
  merge_end_date   <- as.POSIXct(strptime('2014-08-30',format = "%Y-%m-%d"))
  confirm_validity(data_ob,merge_start_date,merge_end_date)
  my_range<- paste0(merge_start_date,'/',merge_end_date)
  sampled_ob <- combine_energy_weather(data_ob,my_range)
  regression_days <- sampled_ob['2014-06-23/2014-07-30']
  bic_matrix <- find_regrassivedays_matrix_with_BIC(regression_days)
  #matplot(t(bic_matrix),t="l")
  
  df <- as.data.frame(t(bic_matrix))
  colnames(df) <- paste0("c",1:dim(df)[2])
  df$rowind <- 1:dim(df)[1]
  df_long <- reshape2::melt(df,id.vars="rowind")
  g <- ggplot(df_long,aes(rowind,value,group=variable,color=variable)) + geom_line()
  ggplotly(g)
  # NOW PLOTTING ONLY SELCECTIVE VALUES
  #df_selec <- df[,colnames(df) %in% c("c6","c11","c13","c15","c17","rowind")]
  df_selec <- df[,colnames(df) %in% c("c6","c11","c13","c17","rowind")]
  colnames(df_selec) <- c("Dataport","AMPds","ECO","REFIT","rowind")
  df_long <- reshape2::melt(df_selec,id.vars="rowind")
  g <- ggplot(df_long,aes(rowind,value,group=variable,color=variable)) + geom_line()
  g <- g +  labs(x = "Number of Days ", y=" BIC value") + theme_grey(base_size = 16) 
  g <- g + theme(axis.text = element_text(color="Black"),legend.position = c(0.80,0.85),legend.title=element_blank(),legend.background = element_rect(fill = alpha('white',0.3)),legend.text = element_text(size = 10)) + scale_x_continuous(breaks = seq(1, 21, by = 2))
  g
  #  ggsave("BIC_threshold.pdf", width = 4.5, height = 4)
}

f_score_kdd_paper <- function() {
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/KDD_2017/figures/")
  
  MUAD <- c(0.62,0.74,0.67,0.74) # 101.csv
  CCS <- c(0.66,0.76,0.68,0.74) # 114.csv
  TAD <- c(0.64,0.82,0.70,0.70) # 115.csv
  RAD <- c(0.69,0.80,0.68,0.76)
  NASAL <- c(0.80,0.89,0.76,0.84)
  name <- c("Dataport","AMPds","ECO","REFIT")
  df <- data.frame(name,MUAD,CCS,TAD,RAD,NASAL)
  # format data frame according to ggplot plotting library
  df_melt <- reshape2::melt(df,id = "name")
  
  g <- ggplot(df_melt,aes(name,value,fill=variable)) + geom_bar(position="dodge",stat="identity",width = 0.6 )
  g <- g +  labs(x = "Dataset ", y="F-score", fill="Dataset") + theme_grey(base_size = 16) 
  g <- g + theme(axis.text = element_text(color="Black"),legend.text = element_text(size = 10)) + scale_y_continuous(breaks = seq(0, 1, by = 0.2))
  g
  # ggsave("f_score.pdf", width = 5, height = 4)
}


nnet_paper_implemenatation <- function() {
  # RAD paper as mentioned in the paper
  # implemenatation of real-time anomaly detection paper
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
 # pathxx  <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  pathxx <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/mutated/"
  #save_path <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_AD/"
  #fls <- mixedsort(list.files(pathxx,pattern = ".csv"))
  # anom_threshold_length <- seq(2,10,by=1)
  file1 <- "1037_iiit.csv"
  
  data_ob <- fread(paste0(pathxx,file1))
  dsub <- data.frame(timestamp = data_ob$localminute,count = data_ob$use)
  dsub$timestamp <- as.POSIXct(strptime(dsub$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")
  timelow <- "2014-06-01"
  timeup <- "2014-08-30"
  
  data_subset <- dsub[(dsub$timestamp > timelow & dsub$timestamp < timeup),]
  data_subset_xts <- xts(data_subset$count,data_subset$timestamp)
  results <-  nnetar_prediction_procedure(data_subset_xts,21)
  online_res <- find_anomalous_status_without_rle_logic(test_data=results$power,result = results,anomaly_window = 3,anomalythreshold_len = 10,rle_logic=FALSE)
  online_res[online_res==TRUE]
  
  nnetar_prediction_procedure <- function(orig_data,train_days){
    library(forecast)
    res <- list()
    orig_data_days <- split.xts(orig_data, f="days",k=1)
    train_data <- orig_data_days[1:train_days]
    test_data <- orig_data_days[(train_days+1):(length(orig_data_days))]
    
    for (i in 1:length(test_data)) {
      train_data_use <- do.call(rbind,train_data[i:length(train_data)])
      test_day <- test_data[[i]]
      #test_object <- rbind(train_data_use,test_day)
      nnet_model <- nnetar(train_data_use,p=40)
      result <- forecast(nnet_model,h=144)
      result <- as.numeric(result$mean)
      test_object <- train_data_use
      fin_result <- compute_predictionband_nnetar(test_object,result,2)
      fin_result_xts <- xts(fin_result,index(test_day))
      res[[i]] <- cbind(test_day,fin_result_xts)
      train_data[[length(train_data)+1]] <- test_day # update train data
    }
    finresult <- do.call(rbind,res)
    colnames(finresult) <- c("power","lwr","upr","fit")
    return(finresult)
  }
  
  compute_predictionband_nnetar <- function(rec_data,predvalue,predictionband){
    # used to compute prediction band for neural networks setup
    df_days <- split.xts(rec_data,f="days")
    df_days <- lapply(df_days, function(x) coredata(x))
    df_mat <- do.call(cbind,df_days[2:length(df_days)])
    sd_val <- apply(df_mat,1,sd)
    dfs <- data.frame(lwr = predvalue - (predictionband * sd_val),upr = predvalue + (predictionband * sd_val))
    dfs$fit <- predvalue
    return(dfs)
  }
}

convert_ECO_occupancydata <- function(df_temp) {
  # function makes eco occupancy readable
  time_vec <-  as.vector(df_temp[,1]) # extract index date column
  # convert dates in interpret date format
  vec2 <- list()
  for ( i in 1:dim(time_vec)[1]){
    vec2[[i]] <- as.character(strptime(time_vec[i,1],format="%d-%b-%Y"))
  }
  date_column <- do.call(rbind,vec2)
  # create new matrix by combining date column and data_column
  newmat <- cbind(date_column,df_temp[,2:dim(df_temp)[2]])
  l <- reshape2::melt(newmat,id.vars = "V1")
  
  time_vec <- as.vector(df_temp[,1])
  l$timestamp <- paste0(l$V1," ",l$variable)
  l$timestamp <- fasttime::fastPOSIXct(l$timestamp)-19800
  occupancy_dframe <- subset(l,select=c(timestamp,value))
  occupancy_dframe_xts <- xts(occupancy_dframe$value,occupancy_dframe$timestamp)
  return(occupancy_dframe_xts)
}

neuralnetwork_procedure_occupancy <- function(train_data,test_data,hourwindow,daywindow){
  #days <- length(unique(lubridate::day(index(test_data))))
  days <- as.numeric( last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  result <- list()
  for (i in 1:days) {
    # browser()
    result[[i]] <- compute_Neural_model_occupancy(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  #browser()
  finresult <- do.call(rbind,result)
  return(finresult)
}

compute_Neural_model_occupancy <- function(train_data,test_days,windowsize,trainingdays) {
  #browser()
  #get train data acc to test day nature(train or test day)
  # train_data <- separate_weekend_weekday(train_data,test_days) 
  library(caret)
  split_train <- split.xts(train_data,f="days",k = 1)
  prac_day <- xts::last(split_train)
  temp <- tail(split_train,trainingdays)
  temp <- temp[1:length(temp)-1] # recent seven without last one, already used for testing
  xdat <- create_feature_matrix(temp)
  colnames(xdat) <- paste0('D',dim(xdat)[2]:1)
  #pdatsplit <- split(prac_day[[1]],lubridate::hour(index(prac_day[[1]])))
  pdatsplit <- split_hourwise(prac_day[[1]],windowsize)
  
  #browser()
  hhmodels <- list()
  for (i in 1:length(pdatsplit)) {
    #browser()
    testinterval <- pdatsplit[[i]]
    #next lines ensures that all values are not same otherwise scale function fails
    testinterval$temperature <- testinterval$temperature + rnorm(NROW(testinterval),0.01,0.01)
    testinterval$humidity <- testinterval$humidity + rnorm(NROW(testinterval),0.01,0.01)
    temp <- xdat[lubridate::hour(xdat) %in% unique(lubridate::hour(testinterval))]
    temp <- subset(temp,select = -D1) # temporary fix. otherwise code creates sometimes problems
    # browser()
    stat <- apply(temp,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp <- temp[,!stat]
    temp_N_anom <- get_selected_nonanomalousdays(temp)
    temp_N_anom <- temp_N_anom[,(dim(temp_N_anom)[2]):(dim(temp_N_anom)[2] - 5)] # USING ONLY 5 DAYS FOR TRAINING
    datas <- cbind(coredata(temp_N_anom),coredata(testinterval))
    datas <- as.data.frame(datas)
    t_dat <- scale(datas[,!colnames(datas)%in% c("power","occupancy")])
    t_dat <- cbind(t_dat,occupancy = datas$occupancy)
    #datas_temp <- scale(datas[,!colnames(datas)%in% c("power")]) # scaling only regressors
    # datas_temp <- cbind(datas_temp,power=datas$power)# combining regressor and predictor
    #modelformula <- as.formula(log(power) ~  D1 + D2 + D3 +D4 + temperature + humidity)
    hhmodels[[i]] <- avNNet(x = t_dat, y = datas$power, size = 10, decay = 0.05, 
                            linout = TRUE, maxit = 500)
  }
  print("NN training done")
  
  #  NOW PREDICT FOR ACTUAL TEST DAY
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  temp2 <- tail(split_train,trainingdays)
  xdat2 <- create_feature_matrix(temp2)
  colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  
  #ydatsplit <- split(ydat,lubridate::hour(index(ydat)))
  ydatsplit <- split_hourwise(ydat,windowsize)
  #browser()
  pred_value <- list()
  for (i in 1:length(ydatsplit)) {
    # browser()
    testinterval2 <- ydatsplit[[i]]
    testinterval2$temperature <- testinterval2$temperature + rnorm(NROW(testinterval2),0.01,0.01)
    testinterval2$humidity <- testinterval2$humidity + rnorm(NROW(testinterval2),0.01,0.01)
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    stat2 <- apply(temp3,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp3 <- temp3[,!stat2]
    temp3_N_anom <- get_selected_nonanomalousdays(temp3) # removing anomalous days
    temp3_N_anom <- temp3_N_anom[,(dim(temp3_N_anom)[2]):(dim(temp3_N_anom)[2] - 5)] #USING ONLY 5 DAYS FOR TRAINING
    datas2 <- cbind(coredata(temp3_N_anom),coredata(testinterval2))
    datas2 <- as.data.frame(datas2)
    datas2_copy <- datas2  # replica used afterwards
    
    t_dat2 <- scale(datas[,!colnames(datas2)%in% c("power","occupancy")])
    t_dat2 <- cbind(t_dat2,occupancy = datas2$occupancy)
    # datas_temp2 <- scale(datas2[,!colnames(datas2)%in% c("power")]) # scaling only regressors
    # datas_temp2 <- cbind(datas_temp2,power=datas2$power)# combining regressor and predictor
    # browser()
    print(last(index(testinterval2)))
    pred_value[[i]] <- predict(hhmodels[[i]], newdata = t_dat2)
    pred_value[[i]] <- ifelse(pred_value[[i]]<0,100+rnorm(1,2,2),pred_value[[i]])
    bands <- compute_predictionband(datas2_copy,pred_value[[i]],2) #computing prediction bands
    pred_value[[i]] <- cbind(fit=pred_value[[i]],bands)
    
  }
  pred_energy <- xts(do.call(rbind,pred_value),index(ydat))
  #pred_energy <- xts(unlist(pred_value),index(ydat))
  return(pred_energy)
}


combine_energy_weather_occupancy_ECOdata <- function(data_ob,occup_dat){
  # this functions simply cbinds energy and weather data
  power_data <- data_ob$power_data
  weather_data <- data_ob$weather_data
  power_weather <- merge(power_data,weather_data)
  power_sub <- power_weather[paste0(first(index(occup_dat)),"/",last(index(occup_dat)))]
  ret_obj <- merge(power_sub,occup_dat)
  ret_obj <- round(na.approx(ret_obj[!is.na(ret_obj$power)]),2)
  return(ret_obj)
}


ECO_prediction_with_occupancy <- function() {
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/")
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
  fls <- list.files(path1,pattern = "*.csv")
  file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/ZurichWeather/weather_Zurich_complete.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  day_pred_result <- list()
  
  for (i in 4:length(fls)) {
    file1 <- fls[1]
    data_ob <- create_weather_power_object_from_ECO_dataset(path1,file1,file2)
    occupancy_path <- paste0(path1,"01_occupancy/01_summer.csv")
    df_occupancy <- fread(occupancy_path)
    df_occupancy_xts <- convert_ECO_occupancydata(df_occupancy)
    sampled_ob <- combine_energy_weather_occupancy_ECOdata(data_ob,df_occupancy_xts)
    colnames(sampled_ob) <- c("power","temperature","humidity","occupancy")
    
    
    print("DATA RANGES ARE:")
    print(paste0("Data,","start: ",index(first(sampled_ob))," end: ",index(last(sampled_ob))))
    
    train_data <- sampled_ob['2012-07-15/2012-07-25']
    test_data <- sampled_ob['2012-07-26/2012-08-25']
    #regression_days <- sampled_ob['2014-06-26/2014-07-30']
    #reg_result <- regression_procedure(train_data,test_data,hourwindow = 6)
    #print("Regression done")
    
    neural_result <- neuralnetwork_procedure_occupancy(train_data,test_data,hourwindow = 6, daywindow = 15)
    print("N-network done")
    print_single_prediction_method_table(test_data, method_result = neural_result$fit)
    plot_singlepredictor_graph(train_data,test_data,neural_result$fit)
    
    
    l <- as.data.frame(plot_table(test_data,reg_result$fit,neural_result$fit,intent = "return"))
    l$home <- strsplit(file1,'[.]')[[1]][1]
    day_pred_result[[i]] <- l
  }
  agg_result <- do.call(rbind,day_pred_result)
  # write.csv(agg_result,file="prediction_result_eco_dataset.csv")
}

neuralnetwork_procedure_withoutweather <- function(train_data,test_data,hourwindow,daywindow){
  #days <- length(unique(lubridate::day(index(test_data))))
  days <- as.numeric( last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  result <- list()
  for (i in 1:days) {
    # browser()
    result[[i]] <- compute_Neural_model_withoutweather(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  #browser()
  finresult <- do.call(rbind,result)
  return(finresult)
}

compute_Neural_model_withoutweather <- function(train_data,test_days,windowsize,trainingdays) {
  #browser()
  #get train data acc to test day nature(train or test day)
  # train_data <- separate_weekend_weekday(train_data,test_days) 
  library(caret)
  split_train <- split.xts(train_data,f="days",k = 1)
  prac_day <- xts::last(split_train)
  temp <- tail(split_train,trainingdays)
  temp <- temp[1:length(temp)-1] # recent seven without last one, already used for testing
  xdat <- create_feature_matrix(temp)
  colnames(xdat) <- paste0('D',dim(xdat)[2]:1)
  #pdatsplit <- split(prac_day[[1]],lubridate::hour(index(prac_day[[1]])))
  pdatsplit <- split_hourwise(prac_day[[1]],windowsize)
  
  #browser()
  hhmodels <- list()
  for (i in 1:length(pdatsplit)) {
    #browser()
    testinterval <- pdatsplit[[i]]
    #next lines ensures that all values are not same otherwise scale function fails
    #testinterval$temperature <- testinterval$temperature + rnorm(NROW(testinterval),0.01,0.01)
    #testinterval$humidity <- testinterval$humidity + rnorm(NROW(testinterval),0.01,0.01)
    temp <- xdat[lubridate::hour(xdat) %in% unique(lubridate::hour(testinterval))]
    temp <- subset(temp,select = -D1) # temporary fix. otherwise code creates sometimes problems
    # browser()
    stat <- apply(temp,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp <- temp[,!stat]
    temp_N_anom <- get_selected_nonanomalousdays(temp)
    temp_N_anom <- temp_N_anom[,(dim(temp_N_anom)[2]):(dim(temp_N_anom)[2] - 5)] # USING ONLY 5 DAYS FOR TRAINING
    datas <- cbind(coredata(temp_N_anom),coredata(testinterval))
    datas <- as.data.frame(datas)
    datas_temp <- scale(datas[,!colnames(datas)%in% c("power")]) # scaling only regressors
    # datas_temp <- cbind(datas_temp,power=datas$power)# combining regressor and predictor
    #modelformula <- as.formula(log(power) ~  D1 + D2 + D3 +D4 + temperature + humidity)
    hhmodels[[i]] <- avNNet(x = datas_temp, y = datas$power, size = 10, decay = 0.05, 
                            linout = TRUE, maxit = 500)
  }
  print("NN training done")
  
  #  NOW PREDICT FOR ACTUAL TEST DAY
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  temp2 <- tail(split_train,trainingdays)
  xdat2 <- create_feature_matrix(temp2)
  colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  
  #ydatsplit <- split(ydat,lubridate::hour(index(ydat)))
  ydatsplit <- split_hourwise(ydat,windowsize)
  #browser()
  pred_value <- list()
  for (i in 1:length(ydatsplit)) {
    #browser()
    testinterval2 <- ydatsplit[[i]]
    #testinterval2$temperature <- testinterval2$temperature + rnorm(NROW(testinterval2),0.01,0.01)
    #testinterval2$humidity <- testinterval2$humidity + rnorm(NROW(testinterval2),0.01,0.01)
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    stat2 <- apply(temp3,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp3 <- temp3[,!stat2]
    temp3_N_anom <- get_selected_nonanomalousdays(temp3) # removing anomalous days
    temp3_N_anom <- temp3_N_anom[,(dim(temp3_N_anom)[2]):(dim(temp3_N_anom)[2] - 5)] #USING ONLY 5 DAYS FOR TRAINING
    datas2 <- cbind(coredata(temp3_N_anom),coredata(testinterval2))
    datas2 <- as.data.frame(datas2)
    datas2_copy <- datas2  # replica used afterwards
    datas_temp2 <- scale(datas2[,!colnames(datas2)%in% c("power")]) # scaling only regressors
    # datas_temp2 <- cbind(datas_temp2,power=datas2$power)# combining regressor and predictor
    # browser()
    print(last(index(testinterval2)))
    pred_value[[i]] <- predict(hhmodels[[i]], newdata = datas_temp2)
    pred_value[[i]] <- ifelse(pred_value[[i]]<0,100+rnorm(1,2,2),pred_value[[i]])
    bands <- compute_predictionband(datas2_copy,pred_value[[i]],2) #computing prediction bands
    pred_value[[i]] <- cbind(fit=pred_value[[i]],bands)
    
  }
  pred_energy <- xts(do.call(rbind,pred_value),index(ydat))
  #pred_energy <- xts(unlist(pred_value),index(ydat))
  return(pred_energy)
}

incremental_multiuser_procedure <- function(train_data,test_data,hourwindow,daywindow){
  # hourwindow = chunk (#hours) for which LOf is run 
  # daywindow = no. of historical days used
  days <- as.numeric(last(as.Date(index(test_data),tz="Asia/Kolkata")) - first(as.Date(index(test_data),,tz="Asia/Kolkata")) )
  #days <- length(unique(lubridate::day(index(test_data))))
  result <- list()
  for (i in 1:days) {
    # third parameter represents the length(hours) of window
    result[[i]] <- compute_Multiuser_scores(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  finresult <- do.call(rbind,result)
  return(finresult)
}

compute_Multiuser_scores <- function(train_data,test_days,windowsize,trainingdays) {
  # for each window (hour or complete day), this function calls LOF and computes anomaly score
  ydat <- split.xts(test_days,"days",k=1)[[1]] # data of current test day only
  split_train <- split.xts(train_data,f="days",k=1)
  temp2 <- tail(split_train,trainingdays)
  xdat2 <- do.call(rbind,temp2)
  # res <- anomaly_detection_main_10_minute_data_rate(temp_data)
  #xdat2 <- create_feature_matrix(temp2)
  #colnames(xdat2) <- paste0('D',dim(xdat2)[2]:1)
  ydatsplit <- split_hourwise(ydat,windowsize)
  #browser()
  anom_score_xts <- list()
  for (i in 1:length(ydatsplit)) {
    #browser()
    testinterval2 <- ydatsplit[[i]]
    temp3 <- xdat2[lubridate::hour(xdat2) %in% unique(lubridate::hour(testinterval2))]
    temp3_update <- rbind(temp3,testinterval2)
    
    anom_score <- anomaly_detection_main_10_minute_data_rate(temp3_update,readings_per_day = windowsize*6)
    # since 6 readings fit in each window 
    # method defined in  Samy_support.R and folder is Matrix_divison
    anom_score_xts[[i]] <- xts(anom_score,last(index(testinterval2)))
  }
  combine_scores <- do.call(rbind,anom_score_xts)
  return(combine_scores)
}

parameter_sensitivity <- function(){
  # effect of S and M
  neu_res <- split.xts(neural_result,f="months",k=1)
  tes <- split.xts(test_data,f="months",k=1)
  #REMEMBER TO CHANGE list indices inside loop
  steps = c(3,6,9,12,15)
  anoms <- vector("numeric")
  for(i in 1:length(steps)){
    res_neu <- find_anomalous_status_without_rle_logic(tes[[2]], result = neu_res[[2]], anomaly_window = 3,anomalythreshold_len =steps[i],rle_logic=TRUE)
    anoms[i] <- length(res_neu[res_neu==TRUE])
  }
  anoms
}

plot_s_parameter_sensitivity <- function(){
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Localize/")
  # df = data.frame(House_1=c(27,8,3,0,0),
  #                  House_2=c(34,8,1,1,0),
  #                 House_3=c(24,9,2,1,0),
  #                  House_4=c(30,8,1,0,0),
  #                  House_5=c(19,8,3,0,0),
  #                  House_6=c(29,5,0,0,0))
  
  # df = data.frame(House_1=c(0.46,.52,0.58,.65,.68,.73,.77,.82,.83,.86,.88,.9,.92,.94,0.94,1,1,1),
  #                 House_2=c(0.2,.27,0.35,.4,.44,.52,.58,.61,.64,.71,.78,.85,.91,.92,.94,1,1,1),
  #                 House_3=c(0.17,.24,0.37,.45,.56,.64,.71,.73,.76,.81,.84,.87,.92,.95,0.97,1,1,1),
  #                 House_4=c(0.17,.22,0.29,.36,.43,.51,.59,.63,.68,.77,.82,.85,.9,.92,.95,.97,.97,.97),
  #                 House_5=c(0.16,.23,0.32,.37,.43,.5,.57,.59,.66,.73,.76,.8,.88,.93,0.97,1,1,1))
  
  df = data.frame(House_1=c(0.47,.53,0.58,.65,.71,.76,.81,.87,.89,.89,.91,.92,.95,.97,0.97,1,1,1),
                  House_2=c(0.52,.57,0.62,.65,.67,.72,.75,.78,.83,.89,.89,.92,.93,.95,.96,.98,1,1),
                  House_3=c(0.49,.50,0.51,.54,.60,.66,.77,.87,.90,.95,.97,1,1,1,1,1,1,1),
                  House_4=c(0.52,.55,0.57,.62,.67,.71,.75,.78,.81,.85,.87,.91,.95,.96,.97,1,1,1),
                  House_5=c(0.45,.49,0.51,.56,.58,.61,.67,.77,.83,.88,.92,.92,.96,.99,1,1,1,1))
  
  
  p_df <- t(df)
  colnames(p_df) <- seq(10,180,10)
  p_cast <- reshape2::melt(p_df)
  g <- ggplot(p_cast,aes(Var2,value,col=Var1))+ geom_line() 
  g <- g + theme(legend.position=c(0.85,0.4),legend.title = element_blank(),legend.text=element_text(size=10),axis.text=element_text(color="black",size=10))  + labs(x= "S value [minutes]", y = "F1-score") + scale_x_continuous(breaks=seq(10,180,20)) 
  g
  savename <- "s_parameter_hour_3_2.pdf"
  # ggsave(file = savename,width = 4, height = 2.5)
  # 
  # df = data.frame(House_1=c(0.51,.62,0.70,.75,.80,.82,.85,.89,.90,.94,.98,.99),
  #                 House_2=c(0.27,.39,0.49,.56,.63,.68,.75,.80,.85,.90,.94,.98),
  #                 House_3=c(0.23,.39,0.53,.65,.74,.78,.82,.85,.83,.93,.97,.99),
  #                 House_4=c(0.24,.34,0.45,.55,.61,.67,.71,.75,.84,.90,.96,.97),
  #                 House_5=c(0.23,.36,0.47,.55,.63,.67,.71,.77,.84,.92,.98,.99))
  
  df = data.frame(House_1=c(0.53,.62,0.70,.75,.80,.82,.85,.89,.90,.94,.98,.99),
                  House_2=c(0.57,.64,0.69,.74,.77,.83,.88,.91,.93,.96,.97,.98),
                  House_3=c(0.50,.53,0.59,.68,.80,.89,.97,.98,1,1,1,1),
                  House_4=c(0.55,.60,0.65,.71,.77,.81,.86,.90,.94,.98,.98,.98),
                  House_5=c(0.48,.54,0.60,.65,.73,.78,.83,.88,.91,.96,.98,.99))
  p_df <- t(df)
  colnames(p_df) <- seq(10,120,10)
  p_cast <- reshape2::melt(p_df)
  g <- ggplot(p_cast,aes(Var2,value,col=Var1))+ geom_line() 
  g <- g + theme(legend.position=c(0.85,0.4),legend.title = element_blank(),legend.text=element_text(size=10),axis.text=element_text(color="black",size=10))  + labs(x= "S value [minutes]", y = "F1-score") + scale_x_continuous(breaks=seq(10,120,20)) 
  g
  savename <- "s_parameter_hour_2_2.pdf"
  # ggsave(file = savename,width = 4, height = 2.5)
  
  # df = data.frame(House_1=c(0.64,.76,0.86,.92,.96,.98),
  #                 House_2=c(0.48,.63,0.73,.83,.88,.95),
  #                 House_3=c(0.44,.66,0.81,.89,.94,.97),
  #                 House_4=c(0.44,.59,0.73,.82,.90,.94),
  #                 House_5=c(0.44,.61,0.73,.81,.89,.95))
  
  
  df = data.frame(House_1=c(0.67,.79,0.86,.93,.97,.97),
                  House_2=c(0.68,.77,0.85,.90,.94,.98),
                  House_3=c(0.57,.68,0.83,.96,.99,1.0),
                  House_4=c(0.64,.73,0.81,.88,.92,.95),
                  House_5=c(0.59,.70,0.80,.87,.94,.97))
  p_df <- t(df)
  colnames(p_df) <- seq(10,60,10)
  p_cast <- reshape2::melt(p_df)
  g <- ggplot(p_cast,aes(Var2,value,col=Var1))+ geom_line() 
  g <- g + theme(legend.position=c(0.85,0.4),legend.title = element_blank(),legend.text=element_text(size=10),axis.text=element_text(color="black",size=10))  + labs(x= "S value [minutes]", y = "F1-score") + scale_x_continuous(breaks=seq(10,60,10)) 
  g
  savename <- "s_parameter_hour_1_2.pdf"
  # ggsave(file = savename,width = 4, height = 2.5)
  
  
}



compute_groundtruth_main <- function(data_ob,past_days = 6,weekday_context = TRUE){
  # main function used to establish ground truth, i.e., weather day is anomalous or not
  # data_ob : basically read from file CCD_maincode2017.r
  # past_days: no. of past days to establish ground truth
  power_df <- data_ob$power_data
  power_df$weekday <- ifelse(!weekdays(index(power_df)) %in% c("Saturday","Sunday"),1,0)
  observe_data <- power_df['2014-06-04/2014-06-30 23:59:59']
  label_data <-  power_df['2014-07-01/2014-08-30 23:59:59']
  #past_days = 6
  #weekday_context = TRUE # meaning weekday will use only weekdays for prediction
  ob_days <- split.xts(observe_data,f="days",k=1)
  lab_days <- split.xts(label_data,f="days",k=1)
  annotated_days <- list()
  for(i in 1:length(lab_days)){
    if(!weekday_context){
      # simple case : without weekday and weekend divison
      op_days <- tail(ob_days,past_days)
      annotated_days[[i]] <- establish_anomaly_groundtruth(op_days,lab_days[[i]])
      ob_days[[length(ob_days)+1]] <- lab_days[[i]] #update observation_days
      # return(ob_days)
    } else {
      if(all(lab_days[[i]]$weekday==1)){ # is weekday
        ind <- sapply(ob_days,function(x) unique(x$weekday))
        ind_weekday <- which(ind == 1)
        my_ob_days <- ob_days[tail(ind_weekday,past_days)]
        annotated_days[[i]] <- establish_anomaly_groundtruth(my_ob_days,lab_days[[i]])
        ob_days[[length(ob_days)+1]] <- lab_days[[i]] #update observation_days
      } else { # if weekend
        week_days_no <- floor(past_days * 0.50) # 50% of weekdays and remaining weekend day
        week_end_no <-  floor(past_days * 0.50)
        ind <- sapply(ob_days,function(x) unique(x$weekday))
        ind_weekday <- which(ind == 1)
        ind_weekend <- which(ind == 0)
        temp_ind <- sort(c(tail(ind_weekday,week_days_no),tail(ind_weekend,week_end_no)))
        my_ob_days <- ob_days[temp_ind]
        annotated_days[[i]] <- establish_anomaly_groundtruth(my_ob_days,lab_days[[i]])
        ob_days[[length(ob_days)+1]] <- lab_days[[i]] #update observation_days
      }
    } 
  }
  gt_data <- do.call(rbind,annotated_days)
  return(gt_data)
}

establish_anomaly_groundtruth <- function(tdata,tgt_data) {
  # called from compute_groundtruth_main
  #browser()
  matdata <- as.data.frame(sapply(tdata,function(x) coredata(x$power)))
  mean_f <- apply(matdata,1,median,na.rm=TRUE)
  # SD <- apply(matdata,1,sd,na.rm=TRUE)
  #pred <- mean + mean * 0.90 # 50% of the base consumption
  pred <- 2 * mean_f # 50% of the base consumption
  anom <- ifelse(coredata(tgt_data$power) > pred,1,0)
  mat_xts <- xts(data.frame(tgt_data$power,anom),index(tgt_data))
  colnames(mat_xts) <- c("power","anomaly")
  return(mat_xts)
}

compute_accuracy_metrices <- function(test_data,neural_result,gt_data,anomalythreshold_len, anomaly_window){
  # this compute precision, recall and f_score by using test day data, prediction results, grouond truth, S and W  windows
  if(!index(last(gt_data)) >= index(last(test_data))) {
    stop("Ground truth not collected till end as required,")
  }
  if(!index(first(gt_data)) <= index(first(test_data))) {
    stop("Ground truth not collected from the start as required")
  }
  d_frame <- cbind(test_data$power,neural_result$upr)
  d_frame$isanom <- ifelse(d_frame$power > d_frame$upr,1,0)
  agg_frame <- cbind(d_frame,gt_data$anomaly)
  time_ranges <- range(index(test_data)) # ensure continous data for all columns
  agg_frame <- agg_frame[paste0(as.character(time_ranges[1]),'/',as.character(time_ranges[2]))]
  colnames(agg_frame) <- c("actual_consump","upr","found_anom","gt_anom")
  
  df_days <- split.xts(agg_frame,f="days",k=1) # daywise dataframe
  pastanomaly_vec <- as.vector("numeric") # keeps track of possi
  
  tp = vector('numeric');tn = vector('numeric');fp = vector('numeric');fn = vector('numeric')
  delta  <- 1 #counter
  for( i in 1:length(df_days)) {
    # Loop j: Divides in terms of anomaly window [one hour or 2 hour etc]
    day_hour <- split_hourwise(df_days[[i]],windowsize = anomaly_window)
    for( j in 1:length(day_hour)) {
      gt_len <- table(day_hour[[j]]$gt_anom )
      gt_len <- ifelse(is.na(as.numeric(gt_len['1'])),0,as.numeric(gt_len['1']))
      ob_len <- table(day_hour[[j]]$found_anom)
      ob_len <- ifelse(is.na(as.numeric(ob_len['1'])),0,as.numeric(ob_len['1']))
      if(ob_len >= anomalythreshold_len | gt_len >= anomalythreshold_len) {
        gt = day_hour[[j]]$gt_anom # ground truth
        ob = day_hour[[j]]$found_anom
        tp[delta] = sum(gt==ob & gt==1)
        tn[delta] = sum(gt==ob & gt==0)
        fn[delta] = sum(gt==1 & ob==0)
        fp[delta] = sum(gt==0 & ob==1)
        delta <- delta+1
      }else{ # algo worked perfectely
        tp[delta] = 1
        tn[delta] = 1
        fn[delta] = 0
        fp[delta] = 0
        delta <- delta+1
      } # if ends
    } # j ends
  } # day i ends
  #browser()
  tp <- sum(tp)
  fp <- sum(fp)
  fn <- sum(fn)
  precison <- round(tp/(tp+fp),2)
  recall <- round(tp/(tp+fn),2)
  f_score <- round(2 * (precison * recall)/ (precison + recall),2)
  print(paste0(precison,":",recall,":",f_score))
  return(list(precison=precison,recall=recall,f_score=f_score))
} # method ends

note_f1_score_result <- funtion(){
  # i repeated this sinnpet to save accuracy results on my notebook
  # required objects:
  # gt_data: eithe compute it online or read from direc as shown below
  # neural_result: eith compute online or read from directory
  # test_data: read online using CCD_maincode2017.R
  # 
  file1="101.csv"
  read_dir <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_neural_results/"
  gt_dir <- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dport_gt_results/"
  neu_result <- fread(paste0(read_dir,file1))
  neural_result <- xts(neu_result[,2:NCOL(neu_result)],fasttime::fastPOSIXct(neu_result$Index)-19800)
  gt <- fread(paste0(gt_dir,file1))
  gt_data <- convert gt to xts// during paper writing I took it online
  
  dat_range <- "2014-07-01/2014-07-30 23:59:59"
  anomaly_window = 1 #CHANGE IT: CONTROLS SIZE OF RUNNING WINDOW
  steps = seq(1,anomaly_window*6,1)  # S PARAMETER
  for(i in 1:length(steps)) {
    res <- compute_accuracy_metrices(test_data[dat_range],
                                     neural_result[dat_range],
                                     gt_data,anomalythreshold_len = steps[i],anomaly_window = anomaly_window)
  }
}
