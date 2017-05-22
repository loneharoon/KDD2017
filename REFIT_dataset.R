
# FUNCTIONS FOR REFIT DATASET ONLY

format_downloaded_REFIT_dataset <- function() {
library(gtools)
path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/REFITPower/"
savepath1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/dataset/"
savepath2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/dataset_10mins/"
fls <- mixedsort(list.files(path))
house_names <- function (){
house1 <- c("Aggregate", "Fridge", "Freezer_1", "Freezer_2", "WasherDryer",
"WashingMachine", "Dishwasher", "Computer", "TelevisionSite", "ElectricHeater")
house2 <- c("Aggregate", "Fridge-Freezer", "WashingMachine", "Dishwasher", "TelevisionSite",
"Microwave", "Toaster", "Hi-Fi", "Kettle", "OverheadFan")
house3 <- c("Aggregate", "Toaster", "Fridge-Freezer", "Freezer", "TumbleDryer",  
"Dishwasher", "WashingMachine", "TelevisionSite", "Microwave", "Kettle")
house4 <- c("Aggregate", "Fridge", "Freezer", "Fridge-Freezer", "WashingMachine_1",
"WashingMachine_2", "DesktopComputer", "TelevisionSite", "Microwave", "Kettle")
house5 <- c("Aggregate","Fridge-Freezer", "TumbleDryer", "WashingMachine", "Dishwasher",
"DesktopComputer", "TelevisionSite", "Microwave", "Kettle", "Toaster")
house6 <- c("Aggregate", "Freezer", "WashingMachine", "Dishwasher", "MJYComputer",
"TV_Satellite", "Microwave", "Kettle", "Toaster", "PGM_Computer")
house7 <- c("Aggregate", "Fridge", "Freezer_1", "Freezer_2", "TumbleDryer",
"WashingMachine", "Dishwasher", "TelevisionSite", "Toaster", "Kettle")
house8<- c("Aggregate", "Fridge", "Freezer", "WasherDryer", "WashingMachine",
"Toaster", "Computer", "TelevisionSite", "Microwave", "Kettle")
house9 <- c("Aggregate", "Fridge-Freezer", "WasherDryer", "WashingMachine", "Dishwasher",
"TelevisionSite", "Microwave", "Kettle", "Hi-Fi", "ElectricHeater")
house10 <- c("Aggregate", "Magimix_Blender", "Toaster", "ChestFreezer", "Fridge-Freezer",
"WashingMachine", "Dishwasher", "TelevisionSite", "Microwave", "Mix")
house11 <- c("Aggregate", "Firdge", "Fridge-Freezer", "WashingMachine", "Dishwasher",
"ComputerSite", "Microwave", "Kettle", "Router", "Hi-Fi")
house12 <- c("Aggregate", "Fridge-Freezer", "noname1","noname2", "ComputerSite",
"Microwave", "Kettle", "Toaster", "Television", "noname3")
house13 <- c("Aggregate", "TelevisionSite", "Freezer", "WashingMachine", "Dishwasher",
"noname", "NetworkSite", "Microwave", "Microwave", "Kettle")
house15 <- c("Aggregate", "Fridge-Freezer", "TumbleDryer", "WashingMachine", "Dishwasher",
"ComputerSite", "TelevisionSite", "Microwave", "Hi-Fi", "Toaster")
house16 <- c("Aggregate", "Fridge-Freezer_1", "Fridge-Freezer_2", "ElectricHeater_1",
"ElectricHeater_2", "WashingMachine", "Dishwasher", "ComputerSite",
"TelevisionSite", "Dehumidifier")
house17 <- c("Aggregate", "Freezer", "Fridge-Freezer", "TumbleDryer", "WashingMachine",
"ComputerSite", "TelevisionSite", "Microwave", "Kettle", "TVSite")
house18 <- c("Aggregate", "Fridge_garag", "Freezer_garage", "Fridge-Freezer",
"WasherDryer_garage", "WashingMachin", "Dishwasher", "DesktopComputer",
"TelevisionSite", "Microwave")
house19 <- c("Aggregate", "FridgeFreezer", "WashingMachine", "TelevisionSite", "Microwave", 
  "Kettle", "Toaster", "Bread-maker","GamesConsole", "Hi-Fi")
house20<- c("Aggregate", "Fridge", "Freezer", "TumbleDryer", "WashingMachine", "Dishwasher", 
 "ComputerSite", "TelevisionSite", "Microwave", "Kettle")
house21<- c("Aggregate", "Fridge-Freezer", "TumbleDryer", "WashingMachine", "Dishwasher", 
 "FoodMixer", "Television", "noname", "Vivarium", "PondPump")
house_names <- list(house1,house2,house3,house4,house5,house6,house7,house8,house9,house10,
     house11,house12,house13,house15,house16,house17,house18,house19,house20,house21)
}
for(i in 1:length(fls)) {
  # this reads files and then adds header first, later saves again file
  df <- fread(paste0(path,fls[i]))
  df_xts <- xts(df[,2:dim(df)[2]], as.POSIXct(df$V1,origin="1970-01-01",tz="UTC") )
  colnames(df_xts) <- house_names[[i]]
  df_10mins <- resample_GMT_data(df_xts,10)
  df_def <- data.frame(localminute=index(df_xts),round(coredata(df_xts),2))
  df_ten_min <- data.frame(localminute=index(df_10mins),round(coredata(df_10mins)))
  write.csv(df_def,paste0(savepath1,fls[i]),row.names = FALSE)
  write.csv(df_ten_min,paste0(savepath2,fls[i]),row.names = FALSE)
}
}

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
  
  library(gtools)
  read_dirX <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/dataset_10mins/"
  fls <- mixedsort(list.files(read_dirX))
  
  for (i in 6:length(fls)){
    filep <- fls[i]
    ten_min_data <- fread(file = paste0(read_dirX,filep))
    xts_data <- xts(ten_min_data[,2:dim(ten_min_data)[2]],as.POSIXct(strptime(ten_min_data$localminute,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01"))
    REFIT_10minutely_plotdaywise(filep,datap = xts_data)  
  }
  
  
  REFIT_10minutely_plotdaywise <- function(filep,datap){
    setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/REFITT/plots_10min/")
    
    readdata_refit_10minutely <- function(datap, subsetyear, subsetmonth) {
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
      no_months <- unique(lubridate::month(xts_data[lubridate::year(xts_data) %in% c(no_year[j])]))
      subsetyear <- no_year[j]
      p <- list()
      k <- 1
      for(i in 1:length(no_months)) { # month loop
       # browser()
        subsetmonth <- no_months[i]
        samplingrate <- "10minutely"
        day_data <- readdata_refit_10minutely(datap, subsetyear,subsetmonth)
        name <-  paste(filep,"_",subsetmonth,"-",subsetyear,"_",samplingrate,sep="")
        savename <- paste(strsplit(filep,"[.]")[[1]][1],"_",subsetmonth,"-",subsetyear,".pdf",sep="")
        p[[k]] <- plot_facetgrid(day_data,name,savename) # this plots data per day in a facet manner
        k <- k+1
      }
      pdf(savename, onefile = TRUE, width = 14, height = 10)
      for (i in seq(length(p))) {
       # do.call("grid.arrange", p[[i]])  
        plot(p[[i]])
      }
      dev.off()
    } # j loop
  } # REFIT_10minutely_plotdaywise
  
}