library(data.table)
library(xts)
library(ggplot2)

resample_data <- function(xts_datap,xseconds) {
  # 
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap), on = "seconds", k = xseconds ), FUN = mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xseconds) # aligning to x minutes
  # return(ds_data)
  rm(ds_data)
  return(align_data)
}


path <- "/Volumes/MacintoshHD/uk_dale_selected_meters/house_1/"
save_directory <- "/Volumes/MacintoshHD/uk_dale_selected_meters/processed/"
#file = "channel_1.dat"
fls = list.files(path)
avoid_fls = c("labels.dat", "mains.dat", "README.txt")
new_fls <- fls[!(fls %in% avoid_fls)]
#df = fread(paste0(path,file))
my_files = list()
#dframes = list()
dframes <- lapply(new_fls,function(x){
  df = fread(paste0(path,x))
  df_xts <- xts(df[,2],as.POSIXct(df$V1,origin="1970-01-01"))
  df_xts <- df_xts["2013-01-01/2013-12-30 23:59:59"]
  name = strsplit(x,'[.]')[[1]][1]
  colnames(df_xts) <- name
  df_xts <- resample_data(df_xts,30) # 30 seconds
  return(df_xts)
})
res <- do.call(cbind,dframes)

## if HOUSE_2
updated_names <- c("aggregate","w_mc","dish_w","fridge","m_wave","kettle") #channels: 1,12,13,14,15,8

## if HOUSE_1
updated_names <- c("aggregate","kettle","fridge","m_wave","w_mc","dish_w") #channels: 1,10,12,13,5,6


colnames(res) <- updated_names



res_temp <- subset(res,select=c("w_mc","dish_w","fridge","m_wave","kettle"))
appliance_agg <- apply(res_temp,1,sum,na.rm=TRUE)
res$miscel <- res$aggregate - appliance_agg
res_csv <- fortify(res)
res_csv$Index <- as.numeric(res_csv$Index)
#write.csv(res_csv,file=paste0(save_directory,"house_1_year2013only.csv"),row.names = FALSE)
