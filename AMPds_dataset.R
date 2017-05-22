

# this script is used to handle AMPds data set
# steps done : Read original dataset and create readable time stamp acc. to Vancouver time
# Read climate (temperature and humidity) from given file 
# combine power and climate and then interpolate climate variables. Store this file
# Resample above frame to 10 minutes rate 

path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/Electricity_P.csv"
df <- read.csv(path)
Sys.setenv(TZ='') # resets system to GMT
df$UNIX_TS <- as.POSIXct(df$UNIX_TS,origin="1970-01-01") - 7 *3600 # subtracts 7 hours so that it sets to vancouer timings
df_power <- xts(df[,2:dim(df)[2]],df[,1])

# process weather file
path2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/Climate_HourlyWeather.csv"
df_weather <- read.csv(path2)
sub_dat <- df_weather[,c("Date.Time","Temp..C.","Rel.Hum....")]
colnames(sub_dat) <- c("timestamp","temperature","humidity")
sub_dat$timestamp <- as.POSIXct(strftime(sub_dat$timestamp,format="%Y-%m-%d %H:%M:%S"))
df_weather <- xts(sub_dat[,2:3],sub_dat[,1])

# combine weathe and power
combine_df <- cbind(df_power,df_weather)
temp <- round(na.approx(combine_df),3)
temp_df <- fortify(temp)
colnames(temp_df) <- c("timestamp",colnames(temp_df)[2:dim(temp_df)[2]])
#write.csv(temp_df,file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/Electricity+weather.csv",row.names = FALSE)
# RESAMPLE TO 10 MINUTES LEVEL
tenmin_dat <- resample_GMT_data (temp,10)
temp_df2 <- fortify(tenmin_dat)
colnames(temp_df2) <- c("timestamp",colnames(temp_df2)[2:dim(temp_df2)[2]])
write.csv(temp_df2,file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/ten_minutes_data.csv",row.names = FALSE)

resample_GMT_data<- function(xts_datap,xminutes) {
  # works when timezone is GMT or Sys.setenv('TZ'='') 
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap), on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xminutes*60) # aligning to x minutes
  # return(ds_data)
  rm(ds_data)
  return(align_data)
}

plot_daywise_data_facetform <- function() {
  
  read_dirX <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/"
  filep <- "ten_minutes_data.csv"
  df <- fread(paste0(path1,file1),header=TRUE, sep=",") # both power and weather data
  df_sub <- xts(df$WHE,as.POSIXct(strptime(df$timestamp,format="%Y-%m-%d %H:%M:%S"),origin="1970-01-01")) 
  colnames(df_sub) <- c("Aggregate")
    AMPds_10minutely_plotdaywise(filep="AMPds",datap = df_sub)  
  }
  
  
  AMPds_10minutely_plotdaywise <- function(filep, datap){
    setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/AMPds/ten_min_plots/")
    
    readdata_ampds_10minutely <- function(datap, subsetyear, subsetmonth) {
      # takes input xts 10 minutely data
      library(lubridate)
      xts_datap <- datap$Aggregate
      subset_data <- xts_datap[year(xts_datap) == subsetyear & month(xts_datap) == subsetmonth]
      #ful_daydata <- split.xts(subset_data,f="days",drop = FALSE,K=1)
      return(subset_data)
    }
    
    plot_facetgrid <- function(day_data,name,savename) {
      day_data <- cbind(day_data,day=.indexmday(day_data),time_minutes= (.indexhour(day_data)+.indexmin(day_data)/60))
      #browser()
      colnames(day_data) <- c('power','day',"time_minutes")
      df <- data.frame(timestamp=index(day_data),coredata(day_data))
      g <- ggplot(df,aes(x=time_minutes,y=power/1000,group=day)) + geom_line()+ facet_wrap(~day) + theme(axis.text= element_text(color="black"))
      g <- g + labs(x="Hour of the Day",y = "Power (kW)") + ggtitle(name)
      return(g)
      #ggsave(file=savename,width=14,height = 10)
    }
    
    no_year <- unique(lubridate::year(datap))
    for (j in 1:length(no_year)){ # year loop
      no_months <- unique(lubridate::month(datap[lubridate::year(datap) %in% c(no_year[j])]))
      subsetyear <- no_year[j]
      p <- list()
      k <- 1
      for(i in 1:length(no_months)) { # month loop
        # browser()
        subsetmonth <- no_months[i]
        samplingrate <- "10minutely"
        day_data <- readdata_ampds_10minutely(datap, subsetyear,subsetmonth)
        name <-  paste(filep,"_",subsetmonth,"-",subsetyear,"_",samplingrate,sep="")
        savename <- paste(strsplit(filep,"[.]")[[1]][1],"_",subsetmonth,"-",subsetyear,".pdf",sep="")
        p[[k]] <- plot_facetgrid(day_data,name,savename) # this plots data per day in a facet manner
        k <- k+1
      }
      pdf(savename, onefile = TRUE, width = 14, height = 10)
      for (i in seq(length(p))) {
        plot(p[[i]])
      }
      dev.off()
    } # j loop
  } # ampds_10minutely_plotdaywise
  
