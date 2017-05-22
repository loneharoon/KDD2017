# This script does following
# 1. Reads formatted csv files,i.e., data is complete time-series, no missing data
# 2. subsets selected data out of input two year data files
# 3. prints daily energy data in a grid fashion
rm(list=ls())
Sys.setenv(TZ="Asia/Kolkata")
library(data.table)
library(xts)

library(ggplot2)
library(gridExtra)
setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/sensys2016/hhourlydata") 



xts_normalizedata <- function(x){
  minval= min(x)
  maxval= max(x)
  xx <- (x-minval)/(maxval-minval)
  return (xx)
}

xts_normalize2 <- function(data, minlimit=0,maxlimit=1){
  #  http://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
  maxval <- max(data)
  minval <- min(data)
  #xx <- vector(mode="numeric",length=length(data))
  #   for (i in 1:length(data)){
  #     xx[i] <- (maxlimit-minlimit)/(maxval-minval)*(data[i]-maxval) + maxlimit
  #     # or xx[i] <- (maxlimit-minlimit)/(maxval-minval)*(data[i]-minval) + minlimit
  #   }
  xx <- (maxlimit-minlimit)/(maxval-minval)*(data-maxval) + maxlimit
  return (xx)
}

plot_ggplot<- function(x){ 
  #x<- day_data[[1]]
  datframe<- data.frame(timestamp=index(x),power=coredata(x))
  names(datframe)= c("timestamp","power")
  lab=as.Date(datframe$timestamp[5],tz="Asia/Kolkata")
  g <- ggplot(datframe)+geom_line(aes(x=timestamp,y=power),color="black") + 
    theme(axis.text= element_text(size = 5),plot.title=element_text(margin=margin(90,0,-90,0)),plot.margin=unit(c(-0.3,-0.1,-0.8,-0.3),"lines") )+
    ggtitle(lab)+ xlab("")+ ylab("")
  return(g)
}

readdata <- function(read_dir,filep,subsetyear,subsetmonth){
  library(lubridate)
  datap <- fread(paste0(read_dir,filep),header=TRUE,sep=",")
  #browser()
  xts_datap <- xts(datap$power,as.POSIXct(strptime(datap$timestamp,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01")) 
  subset_data <- xts_datap[year(xts_datap)==subsetyear & month(xts_datap)==subsetmonth]
  #ful_daydata <- split.xts(subset_data,f="days",drop = FALSE,K=1)
  return(subset_data)
}

readdata_dataport <- function(read_dir2,  filep, subsetyear, subsetmonth){
  # used to work with dataport data only
  # This function assumes that dataset contain only two columns i.e, timestamp and power
  # LOOK AT ANOTHER FUNCTION WHICH PROCESS DATAPORT DATA
  library(lubridate)
  datap <- fread(paste0(read_dir2,filep),header=TRUE,sep=",")
  xts_datap<-xts(datap$power,as.POSIXct(strptime(datap$timestamp,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01")) 
  subset_data <- xts_datap[year(xts_datap)==subsetyear & month(xts_datap)==subsetmonth]
  #ful_daydata <- split.xts(subset_data,f="days",drop = FALSE,K=1)
  return(subset_data)
  
}

readdata_dataport2 <- function(read_dir3,  filep, subsetyear, subsetmonth){
  # READS ONLY USE AND LOCALMINUTES COLUMNS
  library(lubridate)
  datap <- fread(paste0(read_dir3,filep),select = c("localminute","use"),header=TRUE,sep=",")
  xts_datap <- xts(datap$use,as.POSIXct(strptime(datap$localminute,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01")) 
  subset_data <- xts_datap[year(xts_datap) == subsetyear & month(xts_datap) == subsetmonth]
  #ful_daydata <- split.xts(subset_data,f="days",drop = FALSE,K=1)
  return(subset_data)
}

plot_facetgrid <- function(day_data, name, savename) {
  day_data <- cbind(day_data,day=.indexmday(day_data),time_minutes= (.indexhour(day_data)*60+.indexmin(day_data))/30)
  colnames(day_data) <- c('power','day',"time_minutes")
  df <- data.frame(timestamp=index(day_data),coredata(day_data))
  ggplot(df,aes(x=time_minutes,y=power/1000,group=day)) + geom_line()+ facet_wrap(~day)+ggtitle(name) + theme(axis.text= element_text(color="black"))
  ggsave(file=savename,width=14,height = 10)
  #DENSITY PLOTS of h_hourly consumptions
  #  ggplot(df,aes(group=time_minutes))+geom_density(aes(power),fill="black")+facet_wrap(~time_minutes)+ggtitle("apFac_701_may2015_hhourly_density.pdf")
}

read_dir <- "/Volumes/MacintoshHD2/Users/haroonr/Desktop/802_apartment/default/"
#read_dir2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/10minutes/"
read_dir3 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
# SAVE DIRECTORY
setwd("/Volumes/MacintoshHD2/Users/haroonr/Desktop/802_apartment/default/") 
#setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/Buildys_CCD_2016/windowplots/")
filep <- "data_802_30min.csv"
subsetyear <- 2016
for(i in 7:9){
  subsetmonth <- i
  samplingrate <- "30minutely"
  day_data <- readdata(read_dir,filep, subsetyear,subsetmonth)
  #day_data <- readdata_dataport2(read_dir3,filep, subsetyear,subsetmonth)
  name <-  paste(filep,"_",subsetmonth,"-",subsetyear,"_",samplingrate,sep="")
  savename <- paste(strsplit(filep,"[.]")[[1]][1],"_",subsetmonth,"-",subsetyear,".pdf",sep="")
  plot_facetgrid(day_data,name,savename) # this plots data per day in a facet manner
  
  #plot_list1<-lapply(day_data, plot_ggplot) # please uncomment split.xts in readdata for this function to work.
}

read_dirX <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
fls <- list.files(read_dirX)

for (i in 36:length(fls)){
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


ggsave(do.call(grid.arrange, c(plot_list1[1:15],ncol=3)), file="apfac_701_2015_october1.pdf", width = 10 ,height= 14,units= "in")

ggsave(do.call(grid.arrange, c(plot_list1[16:30],ncol=3)), file="apfac_701_2015_october2.pdf", width = 10 ,height= 14,units= "in")

resample_data<- function(xts_datap,xminutes){
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap), on = "minutes", k = xminutes ),FUN= mean)
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xminutes*60) # aligning to x minutes
  # return(ds_data)
  return(align_data)
}

read_data_changefrequency <- function() {
file <- "apFac_802.csv"
dir <- "/Volumes/MacintoshHD2/Users/haroonr/Desktop/802_apartment/default/"
df <-  fread(paste0(dir,file), header = TRUE)
cat(colnames(df))
df_xts <- xts(subset(df, select = -1),as.POSIXct(strftime(df$timestamp,format = "%Y-%m-%d %H:%M:%S")))# removing localminutes column

#df_xts_slice <- df_xts["2014-06-01/2014-08-30"]
seqs <- seq(from= index(first(df_xts)), to = index(tail(df_xts,1)), by ="1 min")
temp <- xts(1:length(seqs),seqs)
if(NROW(df_xts)!=NROW(temp)){
  cat("INTERPOLATION DONE")
  cat("before interpolation", NROW(df_xts))
  df_xts <- na.approx(cbind(temp[,-1],df_xts))
  cat("after interpolation", NROW(df_xts))
}
data_10min <- resample_data(df_xts,10) # sample to 10 mintues
data_10min <- data.frame(timestamp = index(data_10min),power=coredata(data_10min))
data_30min <- resample_data(df_xts,30) # sample to 10 mintues
data_30min <- data.frame(timestamp = index(data_30min),power=coredata(data_30min))

write.csv(data_10min, file = paste0(dir,"data_802_10min.csv"),row.names = FALSE )
write.csv(data_30min, file = paste0(dir,"data_802_30min.csv"),row.names = FALSE )

}