# I redrew some of plots for KDD 2018 paper. so this script shows all of those plots

f_score_kdd_paper <- function() {
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/KDD2018/figures/")
MUAD <- c(0.62,0.74,0.67,0.74) # 101.csv
CCS <- c(0.66,0.76,0.68,0.74) # 114.csv
TAD <- c(0.64,0.82,0.70,0.70) # 115.csv
RAD <- c(0.69,0.80,0.68,0.76)
RIMOR <- c(0.80,0.89,0.76,0.84)
name <- c("Dataport","AMPds","ECO","REFIT")
df <- data.frame(name,MUAD,CCS,TAD,RAD,RIMOR)
# format data frame according to ggplot plotting library
df_melt <- reshape2::melt(df,id = "name")

# plots with grid lines
# g <- ggplot(df_melt,aes(name,value,fill=variable)) + geom_bar(position="dodge",stat="identity",width = 0.6 )
# g <- g +  labs(x = "Dataset ", y="F-score", fill="Dataset") + theme_grey(base_size = 16) 
# g <- g + theme(axis.text = element_text(color="Black"),legend.text = element_text(size = 10)) + scale_y_continuous(breaks = seq(0, 1, by = 0.2))
# g

g <- ggplot(df_melt,aes(name,value)) + geom_bar(aes(fill=variable),position="dodge",stat="identity",width = 0.4 )
g <- g +  labs(x = "Dataset ", y="F-score \n (Higher is better)", fill="")  + scale_fill_brewer(palette="Set1")
g <- g + theme(axis.text = element_text(color="Black",size = 9),legend.text = element_text(size = 9)) + scale_y_continuous(breaks = seq(0, 1, by = 0.2))
g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "grey"))
g
#ggsave("f_score.pdf", width = 5, height = 2.5, units = "in")

 # plotting SMAPE while adding contexts
library(data.table)
df <- fread(file="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/prediction_contexts.csv")
colnames(df) <- c("Dataset","Energy","Energy+Weekday/Weekend","Energy+Weather","All")
df_melt <- reshape2::melt(df,id.vars=c("Dataset"))


g <- ggplot(df_melt,aes(Dataset,value,fill=variable)) + geom_bar(position="dodge",stat="identity",width = 0.4)
g <- g +  labs(x="Dataset",y = "SMAPE \n (Lower is better)",fill="") + scale_fill_brewer(palette = "Set1")
g <- g + theme(text = element_text(size=9), axis.text = element_text(color="Black",size=9),legend.position = 'top',legend.text = element_text(size = 9))
g  <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "grey"))
g

#ggsave("sensitivity_features_3.pdf", width = 5, height = 2.5, units="in") 

}


sensitivity_analysis_KDD_BIC_value <- function(){
  library(data.table)
  library(ggplot2)
  library(xts)
  file1 <- "2094.csv"
  #house_no <- "house1_10min.csv"
  path1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/without_car/9appliances/"
  file2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/weather/Austin2014/10minutely_Austinweather.csv"
  source("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/KDD2017/CCD_supportcode2017.R")
  data_ob <- create_weather_power_object_fromAggDataport(path1,file1,file2)
  merge_start_date <- as.POSIXct(strptime('2014-06-05',format = "%Y-%m-%d"))
  merge_end_date   <- as.POSIXct(strptime('2014-08-30',format = "%Y-%m-%d"))
  confirm_validity(data_ob,merge_start_date,merge_end_date)
  my_range<- paste0(merge_start_date,'/',merge_end_date)
  sampled_ob <- combine_energy_weather(data_ob,my_range)
  regression_days <- sampled_ob['2014-06-23/2014-07-30']
  bic_matrix <- find_regrassivedays_matrix_with_BIC(regression_days)
  #matplot(t(bic_matrix),t="l")
  
  df <- as.data.frame(t(bic_matrix))
  colnames(df) <- paste0("c",1:dim(df)[2])
  df$rowind <- 1:dim(df)[1]
  df_long <- reshape2::melt(df,id.vars="rowind")
  g <- ggplot(df_long,aes(rowind,value,group=variable,color=variable)) + geom_line()
  ggplotly(g)
  # NOW PLOTTING ONLY SELCECTIVE VALUES
  #df_selec <- df[,colnames(df) %in% c("c6","c11","c13","c15","c17","rowind")]
  df_selec <- df[,colnames(df) %in% c("c6","c11","c13","c17","rowind")]
  colnames(df_selec) <- c("Dataport","AMPds","ECO","REFIT","rowind")
  df_long <- reshape2::melt(df_selec,id.vars="rowind")
  colnames(df_long) <- c('rowind','Dataset','value')
  #setwd("/Volumes/MacintoshHD2/Users/haroonr/Downloads/ADMA_paper/figures/")
  
  g <- ggplot(df_long,aes(rowind,value,group=Dataset,color=Dataset)) + geom_line(size=0.3) + geom_point(aes(shape=Dataset))
  g <- g +  labs(x = "Number of Historical Days ", y="BIC value \n (Lower is better)") + scale_colour_brewer(palette = "Set1")
  g <- g + theme(text = element_text(size=9),axis.text = element_text(color="Black",size=8),legend.position = 'top',legend.text = element_text(size = 9),legend.title = element_blank()) + scale_x_continuous(breaks = seq(1, 21, by = 4)) 
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "grey"))
  g
  # ggsave("BIC_figure_ver3.pdf", width = 4, height = 2.5,units="in")
}

note_f1_score_result <- function() {
  
  # the origianl code of this function is given in CCD_supportcode2017.R. I saved generated that of that function once and now in this function I directly use that data for plotting
  
  rds_files <-"/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/KDD2018/fig_temp/"
  onehourfile= readRDS(paste0(rds_files,"onehour.rds"))
  twohourfile= readRDS(paste0(rds_files,"twohour.rds"))
  
  home = onehourfile # onehourfile
  filename <- "july_1_hour2_"
  precision <- sapply(home,function(x) return(x$precison))
  colnames(precision) <- paste0("Home_",1:NCOL(precision))
  recall <- sapply(home,function(x) return(x$recall))
  colnames(recall) <- paste0("Home_",1:NCOL(recall))
  f_score <- sapply(home,function(x) return(x$f_score))
  colnames(f_score) <- paste0("Home_",1:NCOL(f_score))
  #savepath= "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/Ubicomp/fig_temp"
  savepath="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/Submitted/KDD_2017/KDD2018/fig_temp/"
  plot_confusion_matrix_values(precision,"Precision",filename,savepath)
  plot_confusion_matrix_values(recall,"Recall",filename,savepath)
  plot_confusion_matrix_values(f_score,"F-score",filename,savepath)
  
}

plot_confusion_matrix_values <- function(df,y_label,filename,savepath) {
  #good positions of legned: c(0.3,0.85),c(0.7,0.15)
  savename <- paste0(filename,y_label,".pdf")
  row.names(df) <- seq(10,10*NROW(recall),10)
  p_cast <- reshape2::melt(df)
  
  g <- ggplot(p_cast,aes(Var1,value,col=Var2))+ geom_line(size=0.3) + geom_point(aes(shape=Var2)) + guides(col=guide_legend(nrow = 2))
  g <- g + theme(text= element_text(size = 8), legend.position = c(0.7,0.15), legend.title = element_blank(),legend.text=element_text(size=8),axis.text=element_text(color="black",size=8),legend.background = element_rect(fill = alpha('white',0.3)))  + labs(x= "S value [minutes]", y = y_label) + scale_x_continuous(breaks=unique(p_cast$Var1)) 
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "grey"))
  g
  ggsave(savename,width=2.9,height=2,path=savepath,units="in")
}

