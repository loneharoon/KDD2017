library(rhdf5)

read_homes_withappliances <- function(appliances){
  #this function returns homes numbers with specific no. of appliances
  homedetails <- read.csv("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/home_dictionary.csv")
  homes_app <- homedetails[homedetails$columns==appliances,]$home
  homes_app <- paste0(homes_app,".csv")
  return(homes_app)
}

resample_data<- function(xts_datap,xminutes){
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xminutes*60) # aligning to x minutes
  # return(ds_data)
  rm(ds_data)
  return(align_data)
  #return(ds_data)
}

extract_homes_with_N_appliances <- function() {
  # this function gets homes with N appliances first, and then removes car (if present) and stores them as 10 minute CSVs
filename <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/dataport_minutely.h5"
writepath <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
homes_9app <- read_homes_withappliances(appliances=9)
readhandle = H5Fopen(filename)
#list of files stored in object: h5ls(readhandle)$name
#h5read(readhandle,"101.csv")
for(i in 1:length(homes_9app)){
  data <- h5read(readhandle,homes_9app[i])
  xts_dat <- xts(data[,2:dim(data)[2]],as.POSIXct(data[,1],tz="Asia/Kolkata"))
  if(colnames(xts_dat) %in% c("car1")) {
    use <- xts_dat$use - xts_dat$car1
    new_dat <- subset(xts_dat,select = -car1)
    new_dat$use <- use
    xts_dat <- new_dat
  }
  samp_dat <- resample_data(xts_dat,10)
  df <- fortify(samp_dat)
  colnames(df) <- c("localminute",colnames(df)[2:dim(df)[2]])
  write.csv(df,file=paste0(writepath,homes_9app[i]),row.names = FALSE)
}

}

plot_daywise_data_facetform <- function() {
  
  read_dirX <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  fls <- list.files(read_dirX)
  
  for (i in 1:length(fls)){
    filep <- fls[i]
    ten_min_data <- fread(file = paste0(read_dirX,filep))
    xts_data <- xts(ten_min_data[,2:dim(ten_min_data)[2]],as.POSIXct(strptime(ten_min_data$localminute,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01"))
    dataport_10minutely_plotdaywise(filep,datap = xts_data)  
  }
  
  
  dataport_10minutely_plotdaywise <- function(filep,datap){
    setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/10min_plots_new/")
    
    readdata_dataport3_10minutely <- function(datap, subsetyear, subsetmonth) {
      # takes input xts 10 minutely data
      library(lubridate)
      xts_datap <- datap$use 
      subset_data <- xts_datap[year(xts_datap) == subsetyear & month(xts_datap) == subsetmonth]
      #ful_daydata <- split.xts(subset_data,f="days",drop = FALSE,K=1)
      return(subset_data)
    }
    
    plot_facetgrid <- function(day_data,name,savename) {
      # browser()
      # day_data <- cbind(day_data,day=.indexmday(day_data),time_minutes= (.indexhour(day_data)*60+.indexmin(day_data))/30)
      #day_data <- cbind(day_data,day=.indexmday(day_data),time_minutes= (.indexhour(day_data)*60 + .indexmin(day_data)))
      day_data <- cbind(day_data,day=.indexmday(day_data),time_minutes= (.indexhour(day_data)+.indexmin(day_data)/60))
      #browser()
      colnames(day_data) <- c('power','day',"time_minutes")
      df <- data.frame(timestamp=index(day_data),coredata(day_data))
      g <- ggplot(df,aes(x=time_minutes,y=power/1000,group=day)) + geom_line()+ facet_wrap(~day) + theme(axis.text= element_text(color="black"))
      g <- g + labs(x="Hour of the Day",y = "Power (kW)")#+ggtitle(name)
      g
      ggsave(file=savename,width=14,height = 10)
    }
    #filep <- "101.csv"
    #datap <- data_10min
    subsetyear <- 2014
    for(i in 6:8) {
      subsetmonth <- i
      samplingrate <- "10minutely"
      #day_data <- readdata(read_dir,filep, subsetyear,subsetmonth)
      day_data <- readdata_dataport3_10minutely(datap, subsetyear,subsetmonth)
      name <-  paste(filep,"_",subsetmonth,"-",subsetyear,"_",samplingrate,sep="")
      savename <- paste(strsplit(filep,"[.]")[[1]][1],"_",subsetmonth,"-",subsetyear,".pdf",sep="")
      plot_facetgrid(day_data,name,savename) # this plots data per day in a facet manner
    }

  }
  
}