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
#-----------------------------------------SUBS_BS_DATA_SESSION_train----------------------------------------------------------------------------------------------

read_subs_bs_data_session_train <- function(subs_bs_data_session_train){
  
  subs_bs_data_session_train <- read_delim("dataset/train/subs_bs_data_session_train.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
  subs_bs_data_session_train$T_DATE <- substr(subs_bs_data_session_train$START_TIME,1,5)
  subs_bs_data_session_train$START_TIME <- NULL
  subs_bs_data_session_train[,3] <- apply(subs_bs_data_session_train[,3],2,  function(x) as.integer(gsub("^\\,", "", x)))
  subs_bs_data_session_train[is.na(subs_bs_data_session_train)] <- 0
  setDT(subs_bs_data_session_train)
  subs_bs_data_session_train <- subs_bs_data_session_train[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
  rownames(subs_bs_data_session_train) <- NULL
  return(subs_bs_data_session_train)
}

subs_bs_data_session_train <- read_subs_bs_data_session_train(subs_bs_data_session_train = subs_bs_data_session_train)

#-----------------------------------------SUBS_BS_VOICE_SESSION_train----------------------------------------------------------------------------------------------

clc()
read_subs_bs_voice_session_train <- function(subs_bs_voice_session_train){
  
  subs_bs_voice_session_train <- read_delim("dataset/train/subs_bs_voice_session_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_bs_voice_session_train$T_DATE <- substr(subs_bs_voice_session_train$START_TIME,1,5)
  subs_bs_voice_session_train$START_TIME <- NULL
  subs_bs_voice_session_train[,3] <- apply(subs_bs_voice_session_train[,3],2,  function(x) as.integer(gsub("^\\,", "", x)))
  subs_bs_voice_session_train[is.na(subs_bs_voice_session_train)] <- 0
  setDT(subs_bs_voice_session_train)
  subs_bs_voice_session_train <- subs_bs_voice_session_train[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
  rownames(subs_bs_voice_session_train) <- NULL
  return(subs_bs_voice_session_train)
  
}

subs_bs_voice_session_train <- read_subs_bs_voice_session_train(subs_bs_voice_session_train = subs_bs_voice_session_train)

#---------------------------------------INNER_JOIN VOICE AND DATA------------------------------------------------------------------------------------------------------------------
clc()
df_data_voice <- full_join(subs_bs_data_session_train, subs_bs_voice_session_train, by = c("SK_ID" = "SK_ID", 
                                                                                           "CELL_LAC_ID"="CELL_LAC_ID", 
                                                                                           "T_DATE" = "T_DATE"))
subs_bs_data_session_train <- NULL
subs_bs_voice_session_train <- NULL
df_data_voice[is.na(df_data_voice)] <- 0

clc()
#-----------------------------------------SUBS_CSI_train----------------------------------------------------------------------------------------------

read_subs_csi_train <- function(subs_csi_train){
  
  subs_csi_train <- subs_csi_train <- read_delim("dataset/train/subs_csi_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_csi_train[is.na(subs_csi_train)] <- 0
  rownames(subs_csi_train) <- NULL
  return(subs_csi_train)
  
}

subs_csi_train <- read_subs_csi_train(subs_csi_train = subs_csi_train)

#---------------------------------------------------CSI & SESSION--------------------------------------------------------------------------------------
clc()                                                        
DF <- full_join(subs_csi_train, df_data_voice, by = c("SK_ID"="SK_ID", "CONTACT_DATE"= "T_DATE"))
clc()

df_data_voice$T_DATE <- NULL
setDT(df_data_voice)
df_data_voice <- df_data_voice[, lapply(.SD, sum), by = c("CELL_LAC_ID")]

preprocessParams <- preProcess(df_data_voice[,-c(1,2,3)], method=c("center", "scale", "pca"))
df_data_voice <- predict(preprocessParams, df_data_voice)
preprocessParams <- NULL



#----------------------------------------SUBS_BS_CONSUMPTION_train----------------------------------------------------------------------------------------------

read_subs_bs_consumption_train <- function(subs_bs_consumption_train){
  
  subs_bs_consumption_train <- read_delim("dataset/train/subs_bs_consumption_train.csv",";", escape_double = FALSE, trim_ws = TRUE)
  subs_bs_consumption_train$MON <- NULL
  subs_bs_consumption_train[is.na(subs_bs_consumption_train)] <- 0
  subs_bs_consumption_train[,-c(1:2)] <- as.data.frame(apply(subs_bs_consumption_train[,-c(1:2)], 2, function(x) as.integer(gsub("^\\,", "",x))))
  subs_bs_consumption_train[is.na(subs_bs_consumption_train)] <- 0
  
  setDT(subs_bs_consumption_train)
  subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD, sum), by = c("SK_ID")]
  rownames(subs_bs_consumption_train) <- NULL
  
  return(subs_bs_consumption_train)
}

subs_bs_consumption_train <- read_subs_bs_consumption_train(subs_bs_consumption_train = subs_bs_consumption_train)


#-----------------------------------------SUBS_BS_FEATURES_train----------------------------------------------------------------------------------------------

read_subs_features_train <- function(subs_features_train){
  
  subs_features_train <- subs_features_train <- read_delim("dataset/train/subs_features_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_features_train$SNAP_DATE <- NULL
  subs_features_train[is.na(subs_features_train)] <- 0
  subs_features_train[,-c(3)] <- apply(subs_features_train[,-c(3)], 2, function(x) as.integer(gsub("^\\,", "", x)))
  
  setDT(subs_features_train)
  subs_features_train[is.na(subs_features_train)] <- 0
  subs_features_train <- subs_features_train[, lapply(.SD, mean), by = c("SK_ID")]
  
  rownames(subs_features_train) <- NULL
  return(subs_features_train)
  
}

subs_features_train <- read_subs_features_train(subs_features_train = subs_features_train)

clc()
#--------------------------FEATURES + CONSUME BY SUBS AT 3 MONTH BEFORE RESPONDENT--------------------------------------------------------------------------------------------------------------------------------------

DF <-  inner_join(DF,df_kpi,  by = c("CELL_LAC_ID"= "CELL_LAC_ID"))




x_train_tbl <- inner_join(DF,subs_features_train,  by = c("SK_ID"= "SK_ID"))
x_train_tbl <- inner_join(x_train_tbl,subs_bs_consumption_train,  by = c("SK_ID"= "SK_ID"))


x_train_tbl$CELL_LAC_ID.x <- NULL
subs_features_train <-NULL
subs_bs_consumption_train <- NULL

x_train_tbl[is.na(x_train_tbl)] <- 0
setDT(x_train_tbl)
x_train_tbl <- x_train_tbl[, lapply(.SD, sum), by = c("SK_ID","CSI" ,"CONTACT_DATE")]
x_train_tbl <- x_train_tbl[, lapply(.SD, log), by = c("SK_ID", "CSI","CONTACT_DATE")]

#x_train_tbl <- apply(x_train_tbl, 2, function(x) ifelse(grepl('-Inf', x), 0 ,x))

x_train_tbl$DATA_VOL_MB <- ifelse(grepl('-Inf', x_train_tbl$DATA_VOL_MB), max(x_train_tbl$DATA_VOL_MB), x_train_tbl$DATA_VOL_MB)
x_train_tbl$VOICE_DUR_MIN <- ifelse(grepl('-Inf', x_train_tbl$VOICE_DUR_MIN), 0, x_train_tbl$VOICE_DUR_MIN)
x_train_tbl$SUM_DATA_MB <- ifelse(grepl('-Inf', x_train_tbl$SUM_DATA_MB), max(x_train_tbl$SUM_DATA_MB), x_train_tbl$SUM_DATA_MB)
x_train_tbl$SUM_DATA_MIN <- ifelse(grepl('-Inf', x_train_tbl$SUM_DATA_MIN), 0, x_train_tbl$SUM_DATA_MIN)

x_train_tbl[,6:40] <- ifelse(grepl('-Inf', x_train_tbl[,6:40]), 0, x_train_tbl[,6:40])


setDT(x_train_tbl)
x_train_tbl$CONTACT_DATE <- NULL
x_train_tbl <- x_train_tbl[, lapply(.SD, sum), by = c("SK_ID")]
#x_train_tbl <- as.data.frame(x_train_tbl)



x_train_tbl <- inner_join(subs_csi_train, x_train_tbl, by = c("SK_ID"= "SK_ID"))

x_train_tbl$CSI.y <-NULL

x_train <-x_train_tbl[,-c(13:15,17,20,22,23, 31:36, 39:41)]

preprocessParams <- preProcess(x_train[,-c(1:3)], method=c("center", "scale", "pca"))
transformed <- predict(preprocessParams, x_train)
transformed <- transformed[,-c(1:3)]
x_train_tbl <- cbind(x_train, transformed)
clc()

DF <- NULL
df_data_voice <- NULL
#-----------------RESULT DATA FRAME-----------------------------------------------------------------------------------------------------------------------------------------------

# df_cfk <- NULL
# subs_csi_train <- NULL

# x_train_tbl$DATA_VOL_MB <-log(x_train_tbl$DATA_VOL_MB)
# x_train_tbl$VOICE_DUR_MIN <-log(x_train_tbl$VOICE_DUR_MIN)
# 
# x_train_tbl[x_train_tbl$DATA_VOL_MB == -Inf,] <- 0
# x_train_tbl[x_train_tbl$VOICE_DUR_MIN == -Inf,] <- 0

#-----

# x_train_tbl[,c(2,3,4)] <- log(x_train_tbl[,c(2,3,4)])
# x_train_tbl[x_train_tbl$SUM_MINUTES == -Inf,] <- 0
# x_train_tbl[x_train_tbl$SUM_DATA_MB == -Inf,] <- 0
# x_train_tbl[x_train_tbl$SUM_DATA_MIN == -Inf,] <- 0
#-----------------to CSI_STEP2-----------------------------------------------------------------------------------------------------------------------------------------------














