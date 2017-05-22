
file1 <- "1037.csv"
#house_no <- "house1_10min.csv"
path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
df <- fread(paste0(path1,file1))
df_xts <- xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$localminute)-19800)
head(df,2)[,1]
head(df_xts,2)[,2]

df_sub <- df_xts["2014-06-1/2014-08-30"]

dat = df_sub$air1
dat <-dat["2014-07-01/2014-07-10"]
dat2 <- fortify(dat)
colnames(dat2) <- c("Index","power")
g <- ggplot(dat2,aes(Index,power)) + geom_line()
ggplotly(g)

dataframe_visualize_all_columns(df_sub["2014-07-18"])
visualize_context_data_facet_form(df_sub["2014-07-01/2014-07-30"],"use")


# handle dryer case:
dat_new <- subtract_column_from_df(df_xts,"drye1")
dataframe_visualize_all_columns(dat_new["2014-06-30"])
visualize_context_data_facet_form(dat_new["2014-07-01/2014-07-30"],"use")

dat = dat_new$air1
dat <-dat["2014-08-28"]
dat2 <- fortify(dat)
colnames(dat2) <- c("Index","power")
g <- ggplot(dat2,aes(Index,power)) + geom_line()
ggplotly(g)

subtract_column_from_df <- function(datframe,colname){
  # this function subtracts the required column from the dataframe
  #use <- datframe$use
  keep <- colnames(datframe)[!colnames(datframe) %in% colname]
  datframe$use <- datframe$use - datframe[,colname]
  datframe <- subset(datframe,select=keep)
  return(datframe)
}

dataframe_visualize_all_columns <- function(dframe) {
  library(RColorBrewer)# to increase no. of colors
  library(plotly)
  # VISUALIZE SPECiFIC PORTION OF DATA
  #http://novyden.blogspot.in/2013/09/how-to-expand-color-palette-with-ggplot.html
  #dframe <- data_10min["2014-08-9"]
  dframe <- data.frame(timeindex=index(dframe),coredata(dframe))
  # dframe$dataid <- NULL ; dframe$air1 <-NULL ; dframe$use<- NULL ; dframe$drye1 <- NULL
  df_long <- reshape2::melt(dframe,id.vars = "timeindex")
  colourCount = length(unique(df_long$variable))
  getPalette = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount) # brewer.pal(8, "Dark2") or brewer.pal(9, "Set1")
  g <- ggplot(df_long,aes(timeindex,value,col=variable,group=variable))
  g <- g + geom_line() + scale_colour_manual(values=getPalette)
  ggplotly(g)
}

visualize_context_data_facet_form <- function(df,column_name){
  month_data <- df
  month_data <- month_data[,column_name]
  colnames(month_data) <- "power"
  #browser()
  month_data$day <- lubridate::day(index(month_data))
  month_data$time <- lubridate::hour(index(month_data)) * 60 + lubridate::minute(index(month_data))
  # df_long <- reshape2::melt(coredata(month_data),id.vars=c("time","day"))
  g <- ggplot(as.data.frame(coredata(month_data)),aes(time,power)) + geom_line() + facet_wrap(~day,ncol=7) 
  print(g)
}