# In this program aim is to identify individual devices usage in real-time from the aggregate meter level
# data
rm(list=ls())
library(xts)
librays(scales) # for changing x label format
library(ggplot2)

#### User defined functions####
edge_detection_nilm <- function(powerdata) {
  # EDGE DETECTION HART ALGORITHM #
  # IMPortant parameter minsamples: In orginal paper it is 2 seconds, but I set it as 1 minute, set it carefully  
  # step 1: Initilization
  steady_state_power = 0 # E
  last_steady_state_power = 0 # L
  last_power <- 0 # p
  power_changing_flag = FALSE # A flag
  power_changing_progress_flag = FALSE # C flag
  threshold <- 15
  noiselevel <- 70
  minsamples <- 1 # in original paper it is 2SECONDS
  time <- index(powerdata[1])
  suppressWarnings(rm("transition_df"))
  for (i in 1:NROW(powerdata)){
    # step 2: Extract power for this instance
    delta_change <- coredata(powerdata[i]) - last_power # M - P
    #  browser()
    if(abs(delta_change) > threshold)
      power_changing_flag = TRUE
    else
      power_changing_flag = FALSE
    if(power_changing_flag && !power_changing_progress_flag){
      prev_transition <- steady_state_power - last_steady_state_power # step 3A
      if(abs(prev_transition) > noiselevel){
        if(N > minsamples){
          if(!exists("transition_df")){
            transition_df <- data.frame(transition=prev_transition,time=time)
            steadystate_df <- data.frame(steady_state=steady_state_power,time=time)
          }else{
            transition_df <- rbind(transition_df,data.frame(transition=prev_transition,time=time))
            steadystate_df <- rbind(steadystate_df,data.frame(transition=steady_state_power,time=time))
          }
        }
      }
      last_steady_state_power <- steady_state_power # step 3B
      time <- index(powerdata[i]) # step 3C
    }
    if(power_changing_flag)
      N <- 0
    steady_state_power <- (N * steady_state_power + coredata(powerdata[i]))/(N+1)
    N <- N+1 # step 6
    power_changing_progress_flag <- power_changing_flag # step 7
    last_power <- powerdata[i] # step 8
  }
  return (list(transition_df = transition_df,steadstate_df = steadystate_df))
}

pair_buffer <- function(working_buffer) {
  buffsize = NROW(working_buffer) - 1 # buffersize of working buffer
  suppressWarnings(rm("matched_pairs"))
  
  for (i in c(1:buffsize)) {
    idx <- 1
    while (idx < NROW(working_buffer)) { # 1 loop 
      comp_idx <- idx + i
      if (comp_idx < NROW(working_buffer)) { # 2 loop
        val <- working_buffer[idx, ]
        if (val$use > 0 && val$matchflag == FALSE) { # 3 loop
          comp_val <- working_buffer[comp_idx, ]
          if (comp_val$matchflag == FALSE) {   # 4 loop
            v_sum <- val$use + comp_val$use
            tolerenceLimit <- get_tolerence(val$use, comp_val$use)
            if (abs(v_sum) < tolerenceLimit) { # 5 loop
              working_buffer[idx, ]$matchflag <- TRUE
              working_buffer[comp_idx, ]$matchflag <- TRUE
              # browser()
              if (!exists("matched_pairs")) {
                matched_pairs <- data.frame(working_buffer[idx, c(1:2)], working_buffer[comp_idx, c(1:2)])
                colnames(matched_pairs) <- c("leg1", "time1", "leg2", "time2")
              } else {
                tempdf <- data.frame(working_buffer[idx, c(1:2)], working_buffer[comp_idx, c(1:2)])
                colnames(tempdf) <- c("leg1", "time1", "leg2", "time2")
                matched_pairs <- rbind(matched_pairs, tempdf)
              }
              
            } # 5 loop ends here
          } # 4 loop ends here
        } # 3 loop ends here
        idx <- idx + 1 
      } else {
        break
      } # 2 loop ends here
    } #1 loop ends here [while loop]
  } # for loop ends here
  return(list(wrkng_buffer = working_buffer, matched_pairs = matched_pairs))
}

clean_buffer <- function(pre_buffer, working_buffer, next_idx, buffersize) {
  # Two cases:
  # 1: Rows with true flag, removing these will allow to copy entries from pre buffer to working buffer
  # 2: No row with true flag, here we remove oldest entries one by one repetatively
  if (any(working_buffer$matchflag == TRUE)) {
    cat("\n Removing true entries")
    working_buffer <- working_buffer[working_buffer$matchflag == FALSE, ]
    while ((NROW(working_buffer) < (buffersize+1)) && (next_idx <= NROW(pre_buffer))) {
      # copy elements till working buffer gets full
      pos <- NROW(working_buffer)+1
      working_buffer[pos,] <- pre_buffer[next_idx, ]
      next_idx <- next_idx + 1 # next_idx is used in pre_buffer traversal
    }
  } else {
    cat("\n Removing old entries")
    working_buffer <- working_buffer[-1, ] # remove first entry
    working_buffer <- rbind(working_buffer, pre_buffer[next_idx, ])
    next_idx <- next_idx + 1
  }
  row.names(working_buffer) <- c(1:NROW(working_buffer)) # re-name rows for consistency
  return(list(pre_buffer = pre_buffer, working_buffer = working_buffer, next_idx = next_idx))
}

get_tolerence <- function(val, comp_val) {
  # function used to decide tolerence limit for the received pair
  # below parameters are default parametes in HART 1985 paper
  maxTolerence = 35
  percentTolerence = 0.035
  largeTransition = 1000
  # Two cases:
  # 1: when both transitions of pair are less than largeTransition
  # 2: when one of the transitions is greater than largeTransition
  if(val <= largeTransition && comp_val <= largeTransition) {
    tolerenceLimit <- maxTolerence 
  } else {
    tolerenceLimit <- percentTolerence * max(val, comp_val)
  }
  return(tolerenceLimit)
}

visualize_data <- function(dframe) {
  # VISUALIZE SPECiFIC PORTION OF DATA
  #http://novyden.blogspot.in/2013/09/how-to-expand-color-palette-with-ggplot.html
  #dframe <- df_xts_slice
  library(RColorBrewer)# to increas
  dframe <- df_xts_slice[,which(!apply(df_xts_slice==0,2,all))] #appliances which have data
  dframe <- data.frame(timeindex=index(dframe),coredata(dframe))
  # dframe$dataid <- NULL ; dframe$air1 <-NULL ; dframe$use<- NULL ; dframe$drye1 <- NULL
  df_long <- reshape2::melt(dframe,id.vars = "timeindex")
  colourCount = length(unique(df_long$variable))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))(colourCount) # brewer.pal(8, "Dark2") or brewer.pal(9, "Set1")
  #lims <- dframe$timeindex
  g <- ggplot(df_long, aes(timeindex, value, col = variable, group = variable))
  g <- g + geom_line() + scale_colour_manual(values = getPalette)
  g <- g + scale_x_datetime(breaks = date_breaks("10 mins"), labels = date_format("%d %H:%M",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 90,hjust = 1))
  g 
}

## Program starts from here####

file <- "101.csv"
dir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/mix_homes/default/"
df <-  read.csv(paste0(dir,file),header = TRUE)
#df_tt <-  fread(paste0(dir,file),header = TRUE)
cat(colnames(df))
df_xts <- xts(df[,-1],as.POSIXct(strftime(df$localminute, format = "%Y-%m-%d %H:%M:%S")))
#df_xts_tt <- xts(df_tt[,-1],as.POSIXct(strftime(df_tt$localminute,format = "%Y-%m-%d %H:%M:%S")))
df_xts_slice <- df_xts["2014-06-04T02:55/2014-06-04T12:10"]
seqs <- seq(from= index(first(df_xts_slice)), to = index(tail(df_xts_slice, 1)), by = "1 min")
temp <- xts(1:length(seqs),seqs)
if(NROW(df_xts_slice)!=NROW(temp)){
  cat("INTERPOLATION DONE")
  cat("before interpolation", NROW(df_xts_slice))
  df_xts_slice <- na.approx(cbind(temp[,-1], df_xts_slice))
  cat("after interpolation", NROW(df_xts_slice))
}

visualize_data(df_xts_slice)

#sel_load <- df_xts_slice[,"use"]
#sel_load <- sel_load["2014-06-05"]
#plot(sel_load)

powerdata <- df_xts_slice[,"use"] #consider only aggregate reading column
ed_results <- edge_detection_nilm(powerdata) #detect transitions in energy consumption
gp_results <- get_pairs(ed_results)
pairings  <- gp_results$pairings
pairings$timedif <- pairings[,"time2"] - pairings[,"time1"]
pairings
unpaired <- gp_results$working_buffer

get_pairs <- function(ed_results) {
  # this function contains couple of functions doing various tasks
  # Copies transitions from pre buffer to working buffer
  # Cleans up working buffer timely
  transitions_df <- ed_results$transition_df
  transitions_df$matchflag <- FALSE # set matching flag to false initially
  suppressWarnings(rm("pre_buffer","working_buffer","next_idx","pairings"))
  pre_buffer <- transitions_df # put all the transitions in pre-buffer
  # According to Hart's paper, we have two buffer (1) pre-buffer : this buffer contains all the transitions found in the energy trace
  # (2) working buffer: this paper contains only limited number of transitions at a time. We use this buffer to find matching pairs. this buffer
  # gets its data from pre-buffer in several iterations
  buffersize <- 20 # size of working buffer
  
  # first time assignment of working buffer with pre-buffer
  if ((NROW(pre_buffer) != 0) && (NROW(pre_buffer) >= buffersize)) {
    working_buffer <- pre_buffer[1:buffersize, ]
  } else if (NROW(pre_buffer) < buffersize) {
    working_buffer <- pre_buffer
  } else {
    stop("pre_buffer does not contain any transitions!") }
  
  next_idx <- NROW(working_buffer) + 1 # index used to traverse in pre buffer
  pf_results <- pair_buffer(working_buffer) 
  pairings <- pf_results$matched_pairs # pairings contain all the matched pairs found
  
  # run this code till all the entries of pre-buffer get traversed in working buffer
  while (next_idx <= NROW(pre_buffer)) { # untill pre_buffer gets empty
    cb_results <- clean_buffer(pre_buffer, pf_results$wrkng_buffer, next_idx, buffersize)
    temp_results <- pair_buffer(cb_results$working_buffer)
    pairings <- rbind(pairings, temp_results$matched_pairs)
    next_idx <- cb_results$next_idx
  }
  if (NROW(cb_results$working_buffer) > 0) {
    temp_results <- pair_buffer(cb_results$working_buffer)
    pairings <- rbind(pairings, temp_results$matched_pairs)
    cat(paste0("\n Number of unpaired transitions in working buffer:", length(which(temp_results$wrkng_buffer$matchflag==FALSE)) ))
  }
  return(list(pairings = pairings, working_buffer = working_buffer))
}

