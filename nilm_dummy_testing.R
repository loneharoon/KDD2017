# Create dummy data and check each algorithm at first hand
timeseq <- seq(from=as.POSIXct(strptime("2015-04-03 1:00:00",format="%Y-%m-%d %H:%M:%S")),to=as.POSIXct(strptime("2015-04-03 14:00:00",format="%Y-%m-%d %H:%M:%S")),by="1 hour")
df <- data.frame(use=c(40,50,100,100,200,210,210,40,40,500,510,500,200,210))
dfxts <- xts(df,timeseq)
edge_detection_nilm(dfxts)
plot(dfxts)


