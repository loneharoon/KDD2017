
# this script is used to process dataport data explicitly
library(xts)
library(data.table)
#Sys.setenv(TZ="CDT")
dir <- "/Volumes/MacintoshHD2/Users/haroonr/Downloads/"
file <- "dataport-export.csv"
df <- fread(paste0(dir,file),header = TRUE)
df_xts <- xts(data.frame(df$dataid,df$use),as.POSIXct(strptime(df$localminute,format="%Y-%m-%d %H:%M:%S")))

connect_dataport <- function() {
  # this neve worked using R
  library("DBI")
  library("RPostgreSQL")
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="dataport.pecanstreet.org", port = 5434, dbname = "Dataport Database", user="CjvprYvaGPKN", password= "l1GYJH9YV47n")
}

findsuitable_dataset <- function(){
  # this function is used to find the house id's which satisfy some conditions
  dir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/"
  file <- "dataport-metadata.csv"
  df <- read.csv(paste0(dir,file),header = TRUE)
  # names obtained via dput(colnames(df))

  intrested_names <- c("dataid", "active_record", "building_type",
                       "city", "state", "pv", "date_enrolled", "date_withdrawn",
                       "egauge_min_time",
                       "egauge_max_time", "use", "grid", "gen", "air1", "air2", "air3",
                       "airwindowunit1", "aquarium1", "bathroom1", "bathroom2", "bedroom1",
                       "bedroom2", "bedroom3", "bedroom4", "bedroom5", "car1", "clotheswasher1",
                       "clotheswasher_dryg1", "diningroom1", "diningroom2", "dishwasher1",
                       "disposal1", "drye1", "dryg1", "freezer1", "furnace1", "furnace2",
                       "garage1", "garage2", "heater1", "housefan1", "icemaker1", "jacuzzi1",
                       "kitchen1", "kitchen2", "kitchenapp1", "kitchenapp2", "lights_plugs1",
                       "lights_plugs2", "lights_plugs3", "lights_plugs4", "lights_plugs5",
                       "lights_plugs6", "livingroom1", "livingroom2", "microwave1",
                       "office1", "outsidelights_plugs1", "outsidelights_plugs2", "oven1",
                       "oven2", "pool1", "poollight1", "poolpump1", "pump1", "range1",
                       "refrigerator1", "refrigerator2", "security1", "shed1", "sprinkler1",
                       "utilityroom1", "venthood1", "waterheater1", "waterheater2",
                       "winecooler1")

  df2 <- df[,c(intrested_names)]
  #df2$egauge_min_time <- as.POSIXct(df2$egauge_min_time)
  #df2$egauge_max_time <- as.Date(df2$egauge_max_time)
  #df2[((df2$eguage_min_time >= as.Date("2015-01-01")) & (df2$date_withdrawn == NA)), ]
  df2$egauge_min_time <-as.POSIXct(strptime(df2$egauge_min_time,format="%Y-%m-%d %H:%M:%S"))
  df2$egauge_max_time <-as.POSIXct(strptime(df2$egauge_max_time,format="%Y-%m-%d %H:%M:%S"))
  df2[((df2$egauge_min_time >= as.Date("2015-01-01")) & (df2$egauge_max_time >= as.Date("2015-12-31"))), ]
}

get_dataport_information <- function(){
  # this function shows which appliances are used in each home
  library(data.table)
  fdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
  files = list.files(fdir,pattern="*.csv")
  cols = lapply(files,function(x) {
    df = fread(paste0(fdir,x),nrows = 0)
    df = c(as.numeric(strsplit(x,"[.]")[[1]][1]),colnames(df)) # first column provides apartment number
    return(df)}
  )
  #maximum number of appliances in any home
  maxcolumns <- max(unlist(lapply(cols,function(x) length(x))))
  # create empty matrix to store the appliances used in each home
  df <- matrix(data = NA, ncol = maxcolumns, nrow = length(cols))
  for (i in c(1:length(cols)))
  {
    # this loop writes the names of appliance in each home to a csv file row wise
    df[i, ] <- c(cols[[i]], rep(NA,maxcolumns - length(cols[[i]])))
  }
  write.csv(df,file = paste0(fdir,"AppliancesUsed2.csv"),row.names = FALSE )
}

remove_NA_columns <- function(){
  # this function takes dataframe downloaded from dataport and removes NA columns
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Desktop/"
  file <- "1202.csv"
  dt <- fread(paste0(readdir,file))
  dt <- subset(dt,select = -c(V1,dataid,grid)) # remove unnecessary columns
  dfsub <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))),with=F]
  write.csv(dfsub,paste0(readdir,file),row.names = FALSE)
}

find_suitable_homes <- function() {
    # using this function we can find the homes which use minimal number of devices mentioned as lengthlimit
    fdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
    filename <- paste0(fdir,"AppliancesUsed2")
    fl <- fread(filename)
    #apply(fl,1, function(x) length(x[!is.na(x)]))
    # find list of homes how have only x appliance
    lengthlimit <- 3 + 3 # homename + localminute+ use
    unlist(apply(fl,1,function(x) {
      if(length(x[!is.na(x)]) == lengthlimit)
        return (x[[1]]) }
    ))
}

read_data_of_home <- function() {
# Using above function, find the devices of said homes
library(rhdf5)
dir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
fname <- paste0(dir,"dataport_minutely.h5")
readhandle = H5Fopen(fname)
dframe <- h5read(readhandle,"1629.csv")
colnames(dframe)
}

create_dataport_metadata <- function() {
  # this function lists homeno, no. of appliances used, start and end-date of dataset
  library(data.table)
  #fdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
  fdir2 <- "/home/hrashid/Dataport_data/"
  files = list.files(fdir2,pattern="*.csv")
  #files <- files[1:2]
  df <- data.frame(home = numeric(), columns = numeric(), start = character(), end = character())
  dictionary <- do.call(rbind,lapply(files,function(x) {
    frame = fread(paste0(fdir2,x))
    homeno <- as.numeric(strsplit(x,"[.]")[[1]][1])
   # browser()
    df <- rbind(df,data.frame(home = homeno, columns = ncol(frame), start = head(frame,1)$V1, end = last(frame)$V1))
  }
  ))
#write.csv(dictionary,paste0(fdir2,"home_dictionary.csv"),row.names=FALSE)
}

plot_daywise_data_facetform <- function() {
  
  library(gtools)
  read_dirX <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/10minutes_all/"
  fls <- mixedsort(list.files(read_dirX))
  
  for (i in 2:length(fls)){
    filep <- fls[i]
    ten_min_data <- fread(file = paste0(read_dirX,filep))
    xts_data <- xts(ten_min_data$use,as.POSIXct(strptime(ten_min_data$localminute,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01"))
    colnames(xts_data) <- "use"
    dataport_10minutely_plotdaywise(filep,datap = xts_data)  
  }
  
  
  dataport_10minutely_plotdaywise <- function(filep,datap){
    setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/plots_10min/")
    
    readdata_dataport_10minutely <- function(datap, subsetyear, subsetmonth) {
      # takes input xts 10 minutely data
      library(lubridate)
      xts_datap <- datap$use
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
     #browser()
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
        day_data <- readdata_dataport_10minutely(datap, subsetyear,subsetmonth)
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
  } # dataport_10minutely_plotdaywise
  
}

