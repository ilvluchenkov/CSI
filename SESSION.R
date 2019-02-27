#-----------------------------------------------------------------------------------------------------------------------------------------------------------
clc <- function() {
  while (sink.number() > 0) sink(file=NULL)
  rm(list=ls()); gc()
  gc()
  cat("\014")
  cat("All files discriptors closed.\n")
  cat("Whole memory cleaned.\n")
}

clc()


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------SUBS_BS_DATA_SESSION_TRAIN----------------------------------------------------------------------------------------------
read_subs_bs_data_session_train <- function(subs_bs_data_session_train){
  
  subs_bs_data_session_train <- read_delim("dataset/train/subs_bs_data_session_train.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
  #subs_bs_data_session_train$START_TIME <- as.POSIXct(subs_bs_data_session_train$START_TIME, format = "%d.%m %H:%M:%S")
  # subs_bs_data_session_train$START_TIME <- as.POSIXct(subs_bs_data_session_train$START_TIME, format = "%d.%m")
  # subs_bs_data_session_train$MON <-as.integer(strftime(as.Date(subs_bs_data_session_train$START_TIME, "%Y-%m"), format = "%m"))
  # subs_bs_data_session_train$DAY <-as.integer(strftime(as.Date(subs_bs_data_session_train$START_TIME, "%Y-%m"), format = "%d"))
  # subs_bs_data_session_train$HOUR <- as.integer(strftime(as.POSIXct(subs_bs_data_session_train$START_TIME, format = "%y-%m-%d %H:%M:%S"), format = "%H"))
  
  subs_bs_data_session_train$T_DATE <- substr(subs_bs_data_session_train$START_TIME,1,5)
  subs_bs_data_session_train$START_TIME <- NULL
  subs_bs_data_session_train[,3] <- apply(subs_bs_data_session_train[,3],2,  function(x) as.numeric(gsub("^\\,", "", x)))
  
  #subs_bs_data_session_train <- data.frame(subs_bs_data_session_train[!duplicated(subs_bs_data_session_train),])
  subs_bs_data_session_train[is.na(subs_bs_data_session_train)] <- 0
  #subs_bs_data_session_train[,3] <- apply(subs_bs_data_session_train[,3], 2, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)})
  setDT(subs_bs_data_session_train)
  subs_bs_data_session_train <- subs_bs_data_session_train[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
  #subs_bs_data_session_train <- subs_bs_data_session_train %>% filter(subs_bs_data_session_train$MON %in% c(4,5))
  
  #subs_bs_data_session_train$DATA_VOL_MB <- substr(subs_bs_data_session_train$DATA_VOL_MB, 1,2)
  rownames(subs_bs_data_session_train) <- NULL
  
  return(subs_bs_data_session_train)
}

subs_bs_data_session_train <- read_subs_bs_data_session_train(subs_bs_data_session_train = subs_bs_data_session_train)

#saveRDS(subs_bs_data_session_train, file = "~/WORK/scoring/SUBS_BS_DATA_SESSION_TRAIN.RDS")
#subs_bs_data_session_train <- SUBS_BS_DATA_SESSION_TRAIN <-NULL


#-----------------------------------------SUBS_BS_VOICE_SESSION_TRAIN----------------------------------------------------------------------------------------------
clc()
read_subs_bs_voice_session_train <- function(subs_bs_voice_session_train){
  
  subs_bs_voice_session_train <- read_delim("dataset/train/subs_bs_voice_session_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  
  # subs_bs_voice_session_train$START_TIME <- as.POSIXct(subs_bs_voice_session_train$START_TIME, format = "%d.%m %H:%M:%S")
  # subs_bs_voice_session_train$MON <-as.integer(strftime(as.Date(subs_bs_voice_session_train$START_TIME, "%Y-%m"), format = "%m"))
  # subs_bs_voice_session_train$DAY <-as.integer(strftime(as.Date(subs_bs_voice_session_train$START_TIME, "%Y-%m"), format = "%d"))
  # subs_bs_voice_session_train$HOUR <- as.integer(strftime(as.POSIXct(subs_bs_voice_session_train$START_TIME, format = "%y-%m-%d %H:%M:%S"), format = "%H"))
  
  subs_bs_voice_session_train$T_DATE <- substr(subs_bs_voice_session_train$START_TIME,1,5)
  subs_bs_voice_session_train$START_TIME <- NULL
  subs_bs_voice_session_train[,3] <- apply(subs_bs_voice_session_train[,3],2,  function(x) as.numeric(gsub("^\\,", "", x)))
  
  #subs_bs_voice_session_train <- data.frame(subs_bs_voice_session_train[!duplicated(subs_bs_voice_session_train),])
  subs_bs_voice_session_train[is.na(subs_bs_voice_session_train)] <- 0
  #subs_bs_voice_session_train[,3] <- apply(subs_bs_voice_session_train[,3], 2, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)})
  
  #subs_bs_voice_session_train[,3] <- apply(subs_bs_voice_session_train[,3], 2, function(x){(x-min(x))/(max(x) - min(x))})
  
  setDT(subs_bs_voice_session_train)
  subs_bs_voice_session_train <- subs_bs_voice_session_train[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
  #subs_bs_voice_session_train <- subs_bs_voice_session_train %>% filter(subs_bs_voice_session_train$MON %in% c(4,5))
  
  #subs_bs_voice_session_train$VOICE_DUR_MIN <- substr(subs_bs_voice_session_train$VOICE_DUR_MIN, 1,2)
  
  rownames(subs_bs_voice_session_train) <- NULL
  
  return(subs_bs_voice_session_train)
  
}

subs_bs_voice_session_train <- read_subs_bs_voice_session_train(subs_bs_voice_session_train = subs_bs_voice_session_train)

#saveRDS(subs_bs_voice_session_train, file = "~/WORK/scoring/SUBS_BS_VOICE_SESSION_TRAIN.RDS")
#subs_bs_voice_session_train <- SUBS_BS_VOICE_SESSION_TRAIN <-NULL

#---------------------------------------INNER_JOIN VOICE AND DATA------------------------------------------------------------------------------------------------------------------
clc()
df_data_voice <- full_join(subs_bs_data_session_train, subs_bs_voice_session_train, by = c("SK_ID" = "SK_ID", 
                                                                                           "CELL_LAC_ID"="CELL_LAC_ID", 
                                                                                           "T_DATE" = "T_DATE"))
                                                                                            
subs_bs_data_session_train <- NULL
subs_bs_voice_session_train <- NULL
df_data_voice[is.na(df_data_voice)] <- 0
setDT(df_data_voice)
df_data_voice <- df_data_voice[, lapply(.SD, log), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
#df_data_voice <- df_data_voice[, lapply(.SD, function(x){(x-min(x))/(max(x) - min(x))}), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
#preprocessParams <- df_data_voice[, lapply(.SD, function(x) {preProcess(df_data_voice, method=c("center", "scale", "pca"))}), by = c("SK_ID","CELL_LAC_ID")]
#df_data_voice <- as.data.frame(df_data_voice)
df_data_voice$DATA_VOL_MB <- ifelse(grepl('-Inf', df_data_voice$DATA_VOL_MB), max(df_data_voice$DATA_VOL_MB), df_data_voice$DATA_VOL_MB)
df_data_voice$VOICE_DUR_MIN <- ifelse(grepl('-Inf', df_data_voice$VOICE_DUR_MIN), 0, df_data_voice$VOICE_DUR_MIN)

#-----------------------------------------SUBS_CSI_TRAIN----------------------------------------------------------------------------------------------

read_subs_csi_train <- function(subs_csi_train){
  
  subs_csi_train <- subs_csi_train <- read_delim("dataset/train/subs_csi_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  # subs_csi_train$CONTACT_DATE <- as.POSIXct(subs_csi_train$CONTACT_DATE, format = "%d.%m")
  # subs_csi_train$MON <-as.integer(strftime(as.Date(subs_csi_train$CONTACT_DATE, "%Y-%m"), format = "%m"))
  # subs_csi_train$DAY <-as.integer(strftime(as.Date(subs_csi_train$CONTACT_DATE, "%Y-%m"), format = "%d"))
  # subs_csi_train$CONTACT_DATE <- NULL
  #subs_csi_train <- data.frame(subs_csi_train[!duplicated(subs_csi_train),])
  subs_csi_train[is.na(subs_csi_train)] <- 0
  rownames(subs_csi_train) <- NULL
  return(subs_csi_train)
  
}

subs_csi_train <- read_subs_csi_train(subs_csi_train = subs_csi_train)

#---------------------------------------------------CSI & SESSION--------------------------------------------------------------------------------------
clc()                                                        
#setDT(df_data_voice)
#df_data_voice_lac <- df_data_voice_lac[, lapply(.SD, function(x){(x-mean(x, na.r = T))/sd(x, na.rm = T)}), by = c("CELL_LAC_ID")]

df_kpi$CELL_AVAILABILITY_4G <- NULL
df_kpi$RTWP_3G <- NULL

DF <- full_join(subs_csi_train, df_data_voice, by = c("SK_ID"="SK_ID"))#, "CONTACT_DATE" = "T_DATE"))
DF$T_DATE <- NULL
#DF$CONTACT_DATE <- NULL
#DF <-na.omit(DF)

# DF <- as.data.frame(DF)
# DF_t <- DF[,c(4:6)]
# DF[,c(4:6)]<-NULL
# DF_t[is.na(DF_t)] <- 0
# DF <- cbind(DF, DF_t)
# DF <-na.omit(DF)
# DF_t <- NULL

df_data_voice <- NULL
subs_csi_train <- NULL

clc()

setDT(DF)
DF <- DF[, lapply(.SD, sum), by = c("SK_ID","CSI", "CELL_LAC_ID","CONTACT_DATE")]
CSI <- full_join(DF, df_kpi, by = c("CONTACT_DATE"= "T_DATE", "CELL_LAC_ID" = "CELL_LAC_ID" ))

# CSI_t <- CSI[,-c(1:6)]
# CSI[,-c(1:6)] <- NULL
# CSI_t[is.na(CSI_t)] <- 0
# CSI <- cbind(CSI, CSI_t)
# CSI <- na.omit(CSI)
# CSI_t <- NULL

DF <- NULL

setDT(CSI)
CSI$T_DATE <- NULL
CSI <- CSI[, lapply(.SD, mean), by = c("SK_ID", "CSI")] #, "T_DATE","CELL_LAC_ID")]
clc()
#------------------------------------------TO CSI.R------








