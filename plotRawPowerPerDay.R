## Using this, I plot power consumption  (both raw, fft and smoothed versions) for a particular time period of any meter

library(xts)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(scales)
library(timeSeries)
Sys.setenv(TZ="Asia/Kolkata")
rm(list=ls())
read_defualt<- function() {
path1<- "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/hp-paper/hp-xerox-poster/apFac_601.csv"
#path2<- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aug15-Nov15/hourly/apFac_601.csv"
dset=read.csv(file=path1,header=TRUE,sep=",")
data    <-  xts(dset$power,as.POSIXct(dset$timestamp)) 
#data<- Mod(Re(fft(data)))
mindata <-  period.apply(data,endpoints(data,"minutes"), mean)
index(mindata) <- as.POSIXct(trunc(index(mindata),units="mins"))#forcing seconds to zero to remove ambiguity afterwards
daydata <-  split.xts(mindata,f="days",drop=FALSE,K=1)
}

read_lowfrequency <- function() {
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/"
  file1 <- "IIITD_Jan14_Dec15/10minutes/chiller2_3months.csv"

  dset=read.csv(file=paste0(path,file1),header=TRUE,sep=",")
  data    <-  xts(dset$power,as.POSIXct(dset$timestamp)) 
  #mindata <-  period.apply(data,endpoints(data,"minutes"), mean)
  #index(mindata) <- as.POSIXct(trunc(index(mindata),units="mins"))#forcing seconds to zero to remove ambiguity afterwards
  daydata <-  split.xts(data,f="days",drop=FALSE,K=1)

  plot_list1 <- lapply(daydata, plot_ggplot)
  do.call(grid.arrange, c(plot_list1,ncol=5))
}

# create complete series , i.e., show missing values as NA and then fill approx values 
ful_daydata <-    lapply(daydata,function(x) {
  timebounds<- paste(as.Date(start(x),tz="Asia/Kolkata"),"/",as.Date(end(x),tz="Asia/Kolkata")," 23:59",sep="")
  xtstime<-timeBasedSeq(timebounds)
  ob<-xts(1:length(xtstime), xtstime) 
  output<- na.approx(cbind(ob[,-1],x))# filling NA values
  output
})

# FFT in next step
#ful_daydata <- lapply(ful_daydata,function(x) abs(fft(x-mean(x)))^2/length(x)) #APPLYING FFT
 plot_ggplot<- function(x){ 
   datframe<- data.frame(timestamp=index(x),power=coredata(x))
   names(datframe)= c("timestamp","power")
   lab=as.Date(datframe$timestamp[5],tz="Asia/Kolkata")
   g <- ggplot(datframe)+geom_line(aes(x=timestamp,y=power),color="black") + 
     theme(axis.text= element_text(size = 5),plot.title=element_text(vjust = -3,size = 12),plot.margin=unit(c(-0.3,-0.1,-0.8,-0.3),"lines") )+
     ggtitle(lab)+ xlab("")+ ylab("")
  return(g)
 }
 
plot_list1<-lapply(ful_daydata, plot_ggplot)
    multiplot(plotlist=plot_list1[1:15],cols = 3)
    multiplot(plotlist=plot_list1[16:30],cols = 3)

do.call(grid.arrange, c(plot_list1,ncol=5))

# smoothening plots
smooth_data <- lapply(ful_daydata, function(x) { 
  #1. smoothing uses timeSeries format 2.use only spline output part 3. create xts
  y <- xts(smoothSpline(as.timeSeries(coredata(x)),spar=0.5)$spline, index(x) )
  y
})

plot_list2<-lapply(smooth_data, plot_ggplot)
multiplot(plotlist=plot_list2[1:15],cols = 3)
multiplot(plotlist=plot_list2[16:30],cols = 3)


## create a slice of data by using window function####
start_date = as.POSIXct('2015-02-1 0:00:00',format="%Y-%m-%d %H:%M:%S",origin = '1970-01-01')
end_date =  as.POSIXct('2015-02-4 23:59:59',format="%Y-%m-%d %H:%M:%S",origin = '1970-01-01')
sublist = lapply(data2,function(x) window(x,start=start_date,end=end_date))
#create matrix of required data
meter_data <- na.omit(do.call(merge,lapply(sublist, function(x) x)))# REMOVING ENTRIES WITH NA VALUE
#  fit<-Mclust(meter_data) 
#  plot(fit)
#  summary(fit)
meter_data <- data2$apFac_601
meter_data <- scale(meter_data) ## SCALING
p <- fortify(meter_data) # coerce to data frame
melob <- melt(p,id.vars="Index")# convert form wide to long data frame 
names(melob) <- c("timestamp","facet_var","power")
pdf("feb2015_days1_4_Normal_hourly.pdf")
ggplot(melob) + geom_line(aes(x=timestamp,y=power)) + facet_grid(facet_var ~ ., scales = "fixed")+
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  scale_x_datetime(breaks="2 hour")
dev.off()

