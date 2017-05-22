
compute_Neural_model_without_weather_with_outliers <- function(train_data,test_days,windowsize,trainingdays) {
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
   # temp <- subset(temp,select = -D1) # temporary fix. otherwise code creates sometimes problems
    # browser()
    stat <- apply(temp,2,function(x) length(unique(x))==1) # remove columns which have same values -> result in NA at scale()
    temp <- temp[,!stat]
    #temp_N_anom <- get_selected_nonanomalousdays(temp)
    temp_N_anom <- temp[,(dim(temp)[2]):(dim(temp)[2] - 5)] # USING ONLY 5 DAYS FOR TRAINING
    
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
   # temp3_N_anom <- get_selected_nonanomalousdays(temp3) # removing anomalous days
    temp3_N_anom <- temp3[,(dim(temp3)[2]):(dim(temp3)[2] - 5)] #USING ONLY 5 DAYS FOR TRAINING
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

compute_Neural_model_without_weather_without_outliers <- function(train_data,test_days,windowsize,trainingdays) {
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

neuralnetwork_procedure_without_weather_with_outliers   <- function(train_data,test_data,hourwindow,daywindow){
  #days <- length(unique(lubridate::day(index(test_data))))
  days <- as.numeric( last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  result <- list()
  for (i in 1:days) {
    # browser()
    result[[i]] <- compute_Neural_model_without_weather_with_outliers(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  #browser()
  finresult <- do.call(rbind,result)
  return(finresult)
}

neuralnetwork_procedure_without_weather_without_outliers  <- function(train_data,test_data,hourwindow,daywindow){
  #days <- length(unique(lubridate::day(index(test_data))))
  days <- as.numeric( last(as.Date(index(test_data))) - first(as.Date(index(test_data))) )
  result <- list()
  for (i in 1:days) {
    # browser()
    result[[i]] <- compute_Neural_model_without_weather_without_outliers(train_data,test_data,hourwindow,daywindow)
    testsplit <- split.xts(test_data,"days",k=1)
    train_data <- rbind(train_data,testsplit[[1]]) # update train data
    test_data <- do.call(rbind, testsplit[2:length(testsplit)])# update test data
  }
  #browser()
  finresult <- do.call(rbind,result)
  return(finresult)
}