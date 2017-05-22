library(data.table)
library(xts)

# Missing readings with -1
combine_aggregate_plus_appliance_data <- function() {
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
  #***CHANGE PATH FOR EACH HOUSE********************
  savepath <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/house6/"
  sm_path <- paste0(path,"06_sm/")
  plugs_path <-  paste0(path,"06_plugs")
  #***********************************************
  fls <- list.files(sm_path)
  
  for( i in 1:length(fls)) {
    filename <- fls[i]
    df <- fread(paste0(sm_path,"/",filename))
    temp_frame <- df[,1]
    dirs <- list.dirs(plugs_path,recursive = FALSE)
    for(j in 1:length(dirs)){
      file_path <- paste0(dirs[j],"/",filename)
      # if day data exists
      if(file.exists(file_path)){ 
        df2 <- fread(file_path)
      }else{
        df2 <- rep(NA,length = NROW(temp_frame) )
      }
      temp_frame <- cbind(temp_frame,df2)
    }
    colnames(temp_frame) <- c("total","lamp","laptop","router","coffee","entertainment","fridge","kettle")
    # attach timestamp
    day <-  as.Date(strsplit(filename,'[.]')[[1]][1])
    timestamp <- seq(from = as.POSIXct(strptime( paste0(day, " 00:00:00"),format = "%Y-%m-%d %H:%M:%S")), to = as.POSIXct(strptime( paste0(day, " 23:59:59"),format = "%Y-%m-%d %H:%M:%S")), by = "1 sec")
    temp_frame <- cbind(timestamp,temp_frame)
    write.csv(temp_frame,file = paste0(savepath,filename),row.names = FALSE) 
    rm(temp_frame,df2)
  }
}

combine_dayfiles <- function() {
  library(fasttime)
  # this function reads days data and creates a aggregate file
  path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/house2/"
  #writepath <- "D:/ecodatafiles/"
  dirs <- list.dirs(path,full.names =TRUE,recursive = FALSE)
  
  for (j in 1:length(dirs)) {
    #fls <- list.files(dirs[j])
    fls <- list.files(path)
     rm("aggregate_dat")
    for (i in 1:length(fls)){
      df <- fread(paste0(path,fls[i]))
      # xts_df <- xts(df[,2:dim(df)[2]],as.POSIXct(strftime(df$timestamp,format="%Y-%m-%d %H:%M:%S")))
      #xts_df <- xts(df[,2:dim(df)[2]], fastPOSIXct(df$timestamp,tz="GMT"))
      if(!exists("aggregate_dat")){
        aggregate_dat <- df
        #print("1")
      }else{
        aggregate_dat <- rbind(aggregate_dat,df)
        #print("2")
      }
    }
     xts_dat <- xts(aggregate_dat[,2:dim(aggregate_dat)[2]], fastPOSIXct(aggregate_dat$timestamp,tz="GMT"))
    data_10min <- resample_GMT_data(xts_dat,10)
    min_data <-  data.frame(localminute = index(data_10min), round(coredata(data_10min),2))
    write.csv(aggregate_dat, file = paste0(writepath,"house",j,".csv"),row.names = FALSE) 
    write.csv(min_data,file = paste0(writepath,"house",j,"_10min.csv"),row.names = FALSE)
    cat(j)
  }
  
}

convert_days_into_months_stage1 <- function() {
  path <- "D:/ecodata"
  writepath <- "D:/ecodatafiles/"
  dirs <- list.dirs(path,full.names =TRUE,recursive = FALSE)
  
  for (j in 1:length(dirs)){ # no of homes
    fls <- list.files(dirs[j])
    sub_d <- split(fls,rep(1:10,each=30))
    file_no <- 1
    for(i in 1:length(sub_d)){ # dividing days of a home into groups
      rm("aggregate_dat")
      if(length(sub_d[[i]])==0)
          break
      for(k in 1:length(sub_d[[i]])) { # for each group member
        if(length(sub_d[[i]])==0)
          break
        df <- fread(paste0(dirs[j],"/",sub_d[[i]][k] ))
        if(!exists("aggregate_dat")){
          aggregate_dat <- df
          #print("1")
        }else{
          aggregate_dat <- rbind(aggregate_dat,df)
          #print("2")
        }
      }
      write.csv(aggregate_dat, file = paste0(writepath,"house",j,"_",file_no,".csv"),row.names = FALSE) 
      print(paste0(j,"::",i))
      file_no <- file_no + 1
    }
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
  read_dirX <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/"
  fls <- mixedsort(list.files(read_dirX,pattern="*.csv"))
  
  for (i in 2:length(fls)){
    filep <- fls[i]
    ten_min_data <- fread(file = paste0(read_dirX,filep))
    xts_data <- xts(ten_min_data$total,as.POSIXct(strptime(ten_min_data$localminute,format="%Y-%m-%d %H:%M:%S"), origin="1970-01-01"))
    colnames(xts_data) <- "total"
    eco_10minutely_plotdaywise(filep,datap = xts_data)  
  }
  
  
  eco_10minutely_plotdaywise <- function(filep,datap){
    setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/ECO_dataset/ten_min_plots/")
    
    readdata_eco_10minutely <- function(datap, subsetyear, subsetmonth) {
      # takes input xts 10 minutely data
      library(lubridate)
      xts_datap <- datap$total
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
        day_data <- readdata_eco_10minutely(datap, subsetyear,subsetmonth)
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
  } # eco_10minutely_plotdaywise
  
}




columnarnames <- function (){
  ## Data column names of different houses
  first <- c("total","fridge","dryer","coffee_mc","kettle","washing_mc","pc","freezer")
  second <- c("total","tablet","dishwasher","air_exhaust","fridge","entertainment","freezer","kettle","lamp","laptops","stove","tv","stereo")
  third <- c("total","tablet","freezer","coffee_mc","pc","fridge","kettle","entertainment")
  fourth <- c("total","fridge","kitchen","lamp","stereo","freezer","tablet","entertainment","microwave")
  fifth <- c("total", "tablet","coffee_mc","fountain","microwave","fridge","entertainment", "pc", "kettle")
  sixth <- c("total","lamp","laptop","router","coffee","entertainment","fridge","kettle")
}