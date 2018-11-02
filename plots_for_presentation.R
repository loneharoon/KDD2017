
library(xts)
library(data.table)
library(gtools)
library(dplyr)
library(ggplot2)
# In this script I plot figures for the BuildSys presentation.
set.seed(10)
timerange = seq(as.POSIXct("2018-06-01"),as.POSIXct("2018-06-01 23:59:59"), by = '1 min') # assuming original object is hourly sampled
Day1 = c(rnorm(360,100,5), rnorm(720,1000,10), rnorm(360,100,5))
temp1 = xts(Day1,timerange)
#plot(temp)

Day2 = c(rnorm(180,700,5), rnorm(170,110,2), rnorm(740,1010,10), rnorm(350,110,5))
temp2 = xts(Day2,timerange)
#plot(temp)

fseries = cbind(temp1,temp2)
datalong = fortify(fseries) 
colnames(datalong) = c('Index','Day1','Day2')
datalong_melt = reshape2::melt(datalong,id = c("Index"))
g <- ggplot(datalong_melt,aes(Index,value,color = variable )) + geom_line()
g <- g + scale_x_datetime(labels = date_format("%H",tz="Asia/Kolkata"),
                          date_breaks = "4 hours")
g <- g + labs(x= "Day hour", y = "Power (W)") + scale_color_manual(values = c('black','blue'))
g <- g + theme(legend.title = element_blank(),axis.text = element_text(colour='black',size = 12))
g
ggsave("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/buildsys18_final/presentation_plots/demodata2.pdf",width = 8, height = 3,units = "in")

#%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%
 # Showing the effect of weather on energy consumption [Summer vs winter consumption]
library(data.table)
dpath = "/Volumes/DATA_DSK/Datasets_Macbook/Aug15-Nov15/default/apFac_602.csv"
data = fread(dpath)
data_xts = xts(data$power,fasttime::fastPOSIXct(data$timestamp)-19800)
data_low = resample_data_minutely(data_xts,30)
plot(data_low['2015-08-20'])

data_aug = data_low['2015-08-20']
data_nov = xts(coredata(data_low['2015-11-20']),index(data_aug))
data_fin =  cbind(data_aug,data_nov)
data_lob = fortify(data_fin)
colnames(data_lob) = c('Index','Summer','Autumn')
datalong_melt = reshape2::melt(data_lob,id = c("Index"))
g <- ggplot(datalong_melt,aes(Index,value,color = variable )) + geom_line()
g <- g + scale_x_datetime(labels = date_format("%H",tz="Asia/Kolkata"),
                          date_breaks = "4 hours")
g <- g + labs(x= "Day hour", y = "Power (W)") + scale_color_manual(values = c('black','blue'))
g <- g + theme(legend.title = element_blank(),axis.text = element_text(colour='black',size = 12),legend.position = c(0.5,0.8),legend.text = element_text(size = 12))
g
ggsave("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/buildsys18_final/presentation_plots/weather.pdf",width = 4, height = 3,units = "in")
#%%
# showing the effect of occupancy on weather consumption
# code borrowed from IIIT dataset project
library(ggplot2)
library(data.table)
library(xts)
library(dplyr)
Sys.setenv(TZ='Asia/Kolkata')

#def_path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/IIIT_dataset/processed_phase_3/"
def_path <- "/Volumes/DATA_DSK/Datasets_Macbook/IIIT_dataset/processed_phase_3/"
meter <- "acad_build_mains.csv"
data <- fread(paste0(def_path,meter))
data$timestamp <- as.POSIXct(data$timestamp,tz="Asia/Kolkata",origin = "1970-01-01")
#data$timestamp <- fasttime::fastPOSIXct(data$timestamp)+19800
start_date <- as.POSIXct("2017-04-03")
end_date <- as.POSIXct("2017-04-04 23:59:59")
data_sub <- data[data$timestamp >= start_date & data$timestamp <= end_date,]
data_sub_xts <- xts(data_sub$power,data_sub$timestamp)
data_sampled <- resample_data_minutely(data_sub_xts,30)
plot(data_sampled)
#ggplot(data,aes(timestamp,power))+ geom_line()

#occupancy_path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/IIIT_occupancy/final_processed_data/ACB.csv"
occupancy_path <- "/Volumes/DATA_DSK/Datasets_Macbook/IIIT_occupancy/IIITD_occupancy_dataset/ACB.csv"
occu_df <- fread(occupancy_path)
occu_df$timestamp <- as.POSIXct(occu_df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01")
#occu_df$timestamp <- occu_df$timestamp + 19800 # adding 5:30 hours
occu_sub <- occu_df[occu_df$timestamp >= start_date & occu_df$timestamp <= end_date,]
occu_xts <- xts(occu_sub$occupancy_count, occu_sub$timestamp) 
occu_sampled <- resample_occupancy_minutely(occu_xts,30)
plot(occu_sampled)

temp <- cbind(data_sampled,occu_sampled)
temp_df <- fortify(temp)
colnames(temp_df) <- c("timestamp","power","occupancy")
p <- ggplot(temp_df,aes(timestamp,power/1000)) + geom_line(aes(colour="Power"))
p <- p + geom_line(aes(y=occupancy/10,colour="Occupancy"))
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Occupancy count"))
p <- p + scale_colour_manual(values = c("blue", "red")) 
p <- p + labs(y = "Power (kW)", x = "",colour = "") + scale_x_datetime(breaks=scales::date_breaks("1 day"),labels = scales::date_format("%d-%b"))
p <- p + theme(legend.position='none',axis.text = element_text(colour='black',size = 12))
p <- p + theme(axis.text.y.right=element_text(colour = "blue"),axis.title.y.right=element_text(colour = "blue"),axis.title.y.left = element_text(colour = "red"),axis.text.y.left = element_text(color = "red"))
p

ggsave("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/buildsys18_final/presentation_plots/occu_power_buildsys.pdf",height = 3,width = 4,units = c("in"))
#%%%%%%
#%%%%%%%
#%% showing the effect of day hour on consumption
def_path <- "/Volumes/DATA_DSK/Datasets_Macbook/IIIT_dataset/processed_phase_3/"
meter <- "acad_build_mains.csv"
data <- fread(paste0(def_path,meter))
data$timestamp <- as.POSIXct(data$timestamp,tz="Asia/Kolkata",origin = "1970-01-01")
#data$timestamp <- fasttime::fastPOSIXct(data$timestamp)+19800
start_date <- as.POSIXct("2017-04-04")
end_date <- as.POSIXct("2017-04-04 23:59:59")
data_sub <- data[data$timestamp >= start_date & data$timestamp <= end_date,]
data_sub_xts <- xts(data_sub$power,data_sub$timestamp)
data_sampled <- resample_data_minutely(data_sub_xts,15)
#plot(data_sampled)
#temp <- cbind(data_sampled,occu_sampled)
temp_df <- fortify(data_sampled)
colnames(temp_df) <- c("timestamp","power")
r <- ggplot(temp_df,aes(timestamp,power/1000)) + geom_line()
r <- r + labs(y = "Power (kW)", x = "Day hour") + scale_x_datetime(labels = date_format("%H",tz="Asia/Kolkata"),date_breaks = "3 hours")
r <- r + theme(axis.text = element_text(colour='black',size = 12))
r
ggsave("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/buildsys18_final/presentation_plots/tod.pdf",height = 3,width = 4,units = c("in"))
########################
######################## showing the intution of the equation satisfying appliance wattage for anomaly detection
dpath = "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/results/dataport_prediction/"
meterid = '101.csv'
data <- fread(paste0(dpath,meterid))
data$timestamp <- as.POSIXct(data$timestamp,tz="Asia/Kolkata",origin = "1970-01-01")
#data$timestamp <- fasttime::fastPOSIXct(data$timestamp)+19800
data_xts <- xts(data[,2:5],data$timestamp)

mydata <- data_xts['2014-07-16']
long_data <- reshape2::melt(fortify(mydata),id=c('Index'))
bandata <- data.frame(index(mydata),mydata$lwr,mydata$upr)
colnames(bandata) <- c("Index",'lwr','upr')
df_sel <- data.frame(index(mydata),mydata$power,mydata$fit)
colnames(df_sel) <- c("Index",'power','fit')
long_data <- reshape2::melt(df_sel,id=c('Index'))
h <- ggplot(bandata) + geom_ribbon(aes(x = Index, ymin = lwr, ymax = upr), alpha = 0.4)
h <- h + geom_line(data = long_data, aes(Index, value, colour = variable))
h

