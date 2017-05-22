#Steps:
#1. Fetch data month wise or day wise
#2. See if major chunk of data is missing. If yes, try often to download it once more and then combine or rbind with major chunk of data
#3 check for duplicates and remove them
#4 fill missing data by interpolation

# ZURICH stations LSZH 
# These functions are taken from weatherundrground_fetch.R
library(weatherData)
getdetailedData <- function(){
  # this function is used to fetch data from servers
  path <-"/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/ZurichWeather/weather_Zurich_complete.csv"
  start = "2013-01-01"
  end = "2013-01-31"
  data = getWeatherForDate("LSZH", start, end,opt_detailed = T, opt_custom_columns = T, custom_columns = c(2,4))
  write.csv(data, file = paste0(path,"1_zurich2013.csv"),row.names = FALSE,quote = FALSE)

  }

remove_duplicates <- function(){ 
  # assume xts_df is original xts_object
  row_no <- which(duplicated(index(xts_df)))# extract duplicate row no.s
  clear_ob <- xts_df[-row_no,] # remove duplicates
  
  xts_final <- rbind(clear_ob,xts_d1)
  mean_ob <- apply.daily(clear_ob,mean)
  NROW(mean_ob)
}

remove_duplicates_version2 <- function(){ 
  path <-"/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/ZurichWeather/weather_Zurich_complete.csv"
  df <- fread(path,header = TRUE)
  xts_df <-  xts(data.frame(TemperatureC=as.numeric(df$TemperatureC),Humidity=as.numeric(df$Humidity)),as.POSIXct(strptime(df$timestamp,format= "%Y-%m-%d %H:%M:%S")))
  row_no <- which(duplicated(index(xts_df)))# extract duplicate row no.s
  clear_ob <- xts_df[-row_no,] # remove duplicates
  write.csv(data.frame(timestamp = index(clear_ob), coredata(clear_ob)),file = path, row.names=  FALSE)
}

remove_duplicates <- function(){ 
  # removes duplicates in the downloaded csv file
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/"
  file <- list.files(path,pattern = "*_Mumbai2016.csv")
  #file2 <- file[20:21]
  lapply(file, function(x) {
    df <- fread(paste0(path,x),header = TRUE)
    xts_df <-  xts(data.frame(TemperatureC=as.numeric(df$TemperatureC),Humidity=as.numeric(df$Humidity)),as.POSIXct(strptime(df$Time,format= "%Y-%m-%d %H:%M:%S")))
    row_no <- which(duplicated(index(xts_df)))# extract duplicate row no.s
    if(length(row_no) > 0) {
      clear_ob <- xts_df[-row_no,] # remove duplicates
    } else{
      clear_ob <- xts_df 
    }
    write.csv(data.frame(Time=index(clear_ob),coredata(clear_ob)),file=paste0(path,"temp/",x),row.names = FALSE)
    cat(x)
    return(0)
  })
}

combine_files <- function(){
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/ZurichWeather/"
  file <- list.files(path,pattern = "*_zurich*")
  dat <- lapply(file, function(x) {
    df <- fread(paste0(path,x),header = TRUE)
    xts_df <-  xts(data.frame(TemperatureC=as.numeric(df$TemperatureC),Humidity=as.numeric(df$Humidity)),as.POSIXct(strptime(df$Time,format= "%Y-%m-%d %H:%M:%S")))
    return(xts_df)
  })
  temp <- do.call(rbind,dat)
  write.csv(data.frame(timestamp=index(temp),coredata(temp)),file=paste0(path,"weather_Zurich_complete.csv"),row.names = FALSE)
}

create_hourly <- function() {
  # function used to create hourly samples from half
  file <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/weather_complete2016.csv"
  df <- fread(file,header=TRUE)
  df_xts <- xts(data.frame(df$TemperatureC,df$Humidity),fastPOSIXct(df$Time) - 19800)
  sd <- resample_data(df_xts,60)
  
  newtimestamp <- vector()
  for (i in c(1:dim(sd)[1])) {
    if(.indexmin(sd[i,]) %in% c(30)) {
      newtimestamp[i] <-  index(sd[i,]) + 60*60*0.5
    } else{
      newtimestamp[i] <-  index(sd[i,]) + 0
    }
  }
  sd2 <- xts(coredata(sd),as.POSIXct(newtimestamp,origin = "1970-01-01"))
  sd2 <- data.frame( index(sd2), coredata(sd2) )
  
  colnames(sd2) <- c("timestamp","TemperatureC","Humidity")
  # write.csv(sd2,file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/hourlyweather_complete2016.csv",row.names = FALSE)
  
  resample_data <- function(df_xts, xminutes){
    
    ds_data <- period.apply(df_xts,INDEX = endpoints(index(df_xts)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) 
    # align data to nearest time boundary
    align_data <- align.time(ds_data, xminutes*60 - 3600*0.5) # aligning to x minutes
    return(align_data)
  }
  
  resample_data_daywise <- function(xts_datap,xdays) {
    datas <- split.xts(xts_datap,"days",k=xdays)
    daydata <- lapply(datas,function(x){
      xts(mean(x),lubridate::date(x[1])) # 
    })
    ds_data <- do.call(rbind,daydata)
    return(ds_data)
  }
}

create_daily <- function() {
  # function used to create daily samples from half
  file <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/weather_complete2016.csv"
  df <- fread(file,header=TRUE)
  df_xts <- xts(data.frame(df$TemperatureC,df$Humidity),fastPOSIXct(df$Time) - 19800)
  sd <- resample_weather_data_daywise(df_xts,1)
  sd <- data.frame( index(sd), coredata(sd) )
  
  colnames(sd) <- c("timestamp","TemperatureC","Humidity")
  # write.csv(sd,file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/dailyweather_complete2016.csv",row.names = FALSE)
  
  resample_weather_data_daywise <- function(xts_datap,xdays) {
    datas <- split.xts(xts_datap,"days",k=xdays)
    # browser()
    daydata <- lapply(datas,function(x){
      xts(data.frame(round(mean(x[,1]),3),round(mean(x[,2]),3)),lubridate::date(x[1,]))
    })
    ds_data <- do.call(rbind,daydata)
    # colnames(ds_data)
    return(ds_data)
  }
}

interpolate_hourly_Series <- function() {
  # In this function, I create a complete series by interpolating missing values
  weatherdat <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/hourlyweather_complete2016.csv"
  df_xts <- fread(weatherdat)
  df_xts <- xts(df_xts[,2:dim(df_xts)[2]],fastPOSIXct(df_xts$timestamp)-19800)
  timerange = seq(start(df_xts),end(df_xts), by = "hour")# assuming original object is hourly sampled
  temp = xts(rep(NA,length(timerange)),timerange)
  complete_xts = merge(df_xts,temp)[,1:2]
  tt <- na.approx(complete_xts)
  # write.csv(data.frame(timestamp = index(tt),coredata(tt)),file= "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/hourlyweather_complete2016.csv",row.names = FALSE)
  
}
