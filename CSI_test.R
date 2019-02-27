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
#-----------------------------------------SUBS_BS_DATA_SESSION_test----------------------------------------------------------------------------------------------

read_subs_bs_data_session_test <- function(subs_bs_data_session_test){
  
  subs_bs_data_session_test <- read_delim("dataset/test/subs_bs_data_session_test.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
  subs_bs_data_session_test$T_DATE <- substr(subs_bs_data_session_test$START_TIME,1,5)
  subs_bs_data_session_test$START_TIME <- NULL
  subs_bs_data_session_test[,3] <- apply(subs_bs_data_session_test[,3],2,  function(x) as.integer(gsub("^\\,", "", x)))
  subs_bs_data_session_test[is.na(subs_bs_data_session_test)] <- 0
  setDT(subs_bs_data_session_test)
  subs_bs_data_session_test <- subs_bs_data_session_test[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
  rownames(subs_bs_data_session_test) <- NULL
  return(subs_bs_data_session_test)
}

subs_bs_data_session_test <- read_subs_bs_data_session_test(subs_bs_data_session_test = subs_bs_data_session_test)

#-----------------------------------------SUBS_BS_VOICE_SESSION_test----------------------------------------------------------------------------------------------

clc()
read_subs_bs_voice_session_test <- function(subs_bs_voice_session_test){
  
  subs_bs_voice_session_test <- read_delim("dataset/test/subs_bs_voice_session_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_bs_voice_session_test$T_DATE <- substr(subs_bs_voice_session_test$START_TIME,1,5)
  subs_bs_voice_session_test$START_TIME <- NULL
  subs_bs_voice_session_test[,3] <- apply(subs_bs_voice_session_test[,3],2,  function(x) as.integer(gsub("^\\,", "", x)))
  subs_bs_voice_session_test[is.na(subs_bs_voice_session_test)] <- 0
  setDT(subs_bs_voice_session_test)
  subs_bs_voice_session_test <- subs_bs_voice_session_test[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID", "T_DATE")]
  rownames(subs_bs_voice_session_test) <- NULL
  return(subs_bs_voice_session_test)
  
}

subs_bs_voice_session_test <- read_subs_bs_voice_session_test(subs_bs_voice_session_test = subs_bs_voice_session_test)

#---------------------------------------INNER_JOIN VOICE AND DATA------------------------------------------------------------------------------------------------------------------
clc()
df_data_voice <- full_join(subs_bs_data_session_test, subs_bs_voice_session_test, by = c("SK_ID" = "SK_ID", 
                                                                                         "CELL_LAC_ID"="CELL_LAC_ID", 
                                                                                         "T_DATE" = "T_DATE"))
subs_bs_data_session_test <- NULL
subs_bs_voice_session_test <- NULL
clc()

#-----------------------------------------SUBS_CSI_test----------------------------------------------------------------------------------------------

read_subs_csi_test <- function(subs_csi_test){
  
  subs_csi_test <- subs_csi_test <- read_delim("dataset/test/subs_csi_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_csi_test[is.na(subs_csi_test)] <- 0
  rownames(subs_csi_test) <- NULL
  return(subs_csi_test)
  
}

subs_csi_test <- read_subs_csi_test(subs_csi_test = subs_csi_test)

#---------------------------------------------------CSI & SESSION--------------------------------------------------------------------------------------
clc()                                                        

DF <- full_join(subs_csi_test, df_data_voice, by = c("SK_ID"="SK_ID", "CONTACT_DATE"= "T_DATE"))

# DF <- as.data.frame(DF)
# DF_t <- DF[,c(3:5)]
# DF[,c(3:5)]<-NULL
# DF_t[is.na(DF_t)] <- 0
# DF <- cbind(DF, DF_t)
# DF <-na.omit(DF)
# DF_t <- NULL
# 
# CSI_test <- inner_join(DF, df_kpi, by = c("CONTACT_DATE"= "T_DATE", "CELL_LAC_ID" = "CELL_LAC_ID" ))
# CSI_t <- CSI_test[,-c(1:5)]
# CSI_test[,-c(1:5)] <- NULL
# CSI_t[is.na(CSI_t)] <- 0
# CSI_test <- cbind(CSI_test, CSI_t)
# CSI_test <- na.omit(CSI_test)
# CSI_t <- NULL
# DF <- NULL
# df_data_voice <- NULL

clc()


#----------------------------------------SUBS_BS_CONSUMPTION_test----------------------------------------------------------------------------------------------

read_subs_bs_consumption_test <- function(subs_bs_consumption_test){
  
  subs_bs_consumption_test <- read_delim("dataset/test/subs_bs_consumption_test.csv",";", escape_double = FALSE, trim_ws = TRUE)
  subs_bs_consumption_test$MON <- NULL
  subs_bs_consumption_test[is.na(subs_bs_consumption_test)] <- 0
  subs_bs_consumption_test[,-c(1:2)] <- as.data.frame(apply(subs_bs_consumption_test[,-c(1:2)], 2, function(x) as.integer(gsub("^\\,", "",x))))
  subs_bs_consumption_test[is.na(subs_bs_consumption_test)] <- 0
  
  setDT(subs_bs_consumption_test)
  subs_bs_consumption_test <- subs_bs_consumption_test[, lapply(.SD, sum), by = c("SK_ID")]
  rownames(subs_bs_consumption_test) <- NULL
  
  return(subs_bs_consumption_test)
}

subs_bs_consumption_test <- read_subs_bs_consumption_test(subs_bs_consumption_test = subs_bs_consumption_test)


#-----------------------------------------SUBS_BS_FEATURES_test----------------------------------------------------------------------------------------------

read_subs_features_test <- function(subs_features_test){
  
  subs_features_test <- subs_features_test <- read_delim("dataset/test/subs_features_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_features_test$SNAP_DATE <- NULL
  subs_features_test[is.na(subs_features_test)] <- 0
  subs_features_test[,-c(3)] <- apply(subs_features_test[,-c(3)], 2, function(x) as.integer(gsub("^\\,", "", x)))
  
  setDT(subs_features_test)
  subs_features_test[is.na(subs_features_test)] <- 0
  subs_features_test <- subs_features_test[, lapply(.SD, mean), by = c("SK_ID")]
  
  rownames(subs_features_test) <- NULL
  return(subs_features_test)
  
}

subs_features_test <- read_subs_features_test(subs_features_test = subs_features_test)

clc()
#--------------------------FEATURES + CONSUME BY SUBS AT 3 MONTH BEFORE RESPONDENT--------------------------------------------------------------------------------------------------------------------------------------

x_test_tbl <- full_join(DF,subs_features_test,  by = c("SK_ID"= "SK_ID"))
x_test_tbl <- full_join(x_test_tbl,subs_bs_consumption_test,  by = c("SK_ID"= "SK_ID"))

x_test_tbl$CELL_LAC_ID.x <- NULL
subs_features_test <-NULL
subs_bs_consumption_test <- NULL

x_test_tbl[is.na(x_test_tbl)] <- 0
setDT(x_test_tbl)
x_test_tbl <- x_test_tbl[, lapply(.SD, sum), by = c("SK_ID", "CONTACT_DATE")]
x_test_tbl <- x_test_tbl[, lapply(.SD, log), by = c("SK_ID", "CONTACT_DATE")]

#x_test_tbl <- apply(x_test_tbl, 2, function(x) ifelse(grepl('-Inf', x), 0 ,x))

x_test_tbl$DATA_VOL_MB <- ifelse(grepl('-Inf', x_test_tbl$DATA_VOL_MB), max(x_test_tbl$DATA_VOL_MB), x_test_tbl$DATA_VOL_MB)
x_test_tbl$VOICE_DUR_MIN <- ifelse(grepl('-Inf', x_test_tbl$VOICE_DUR_MIN), 0, x_test_tbl$VOICE_DUR_MIN)
x_test_tbl$SUM_DATA_MB <- ifelse(grepl('-Inf', x_test_tbl$SUM_DATA_MB), max(x_test_tbl$SUM_DATA_MB), x_test_tbl$SUM_DATA_MB)
x_test_tbl$SUM_DATA_MIN <- ifelse(grepl('-Inf', x_test_tbl$SUM_DATA_MIN), 0, x_test_tbl$SUM_DATA_MIN)

DF <- NULL
df_data_voice <- NULL


x_test_tbl[,5:40] <- ifelse(grepl('-Inf', x_test_tbl[,5:40]), 0, x_test_tbl[,5:40])



setDT(x_test_tbl)
x_test_tbl$CONTACT_DATE <- NULL
x_test_tbl <- x_test_tbl[, lapply(.SD, sum), by = c("SK_ID")]
#x_train_tbl <- as.data.frame(x_train_tbl)

x_test_tbl <- inner_join(subs_csi_test, x_test_tbl, by = c("SK_ID"= "SK_ID"))

x_test <-x_test_tbl[,-c(12:14,16,19,21,22, 30:35, 38:40)]

preprocessParams <- preProcess(x_test[,-c(1,2)], method=c("center", "scale", "pca"))
transformed <- predict(preprocessParams, x_test)

transformed <- transformed[,-c(1:2)]
x_test_tbl <- cbind(x_test, transformed)


#-----------------RESULT DATA FRAME-----------------------------------------------------------------------------------------------------------------------------------------------

# df_cfk <- NULL
# subs_csi_test <- NULL

# x_test_tbl$DATA_VOL_MB <-log(x_test_tbl$DATA_VOL_MB)
# x_test_tbl$VOICE_DUR_MIN <-log(x_test_tbl$VOICE_DUR_MIN)
# 
# x_test_tbl[x_test_tbl$DATA_VOL_MB == -Inf,] <- 0
# x_test_tbl[x_test_tbl$VOICE_DUR_MIN == -Inf,] <- 0

#-----

# x_test_tbl[,c(2,3,4)] <- log(x_test_tbl[,c(2,3,4)])
# x_test_tbl[x_test_tbl$SUM_MINUTES == -Inf,] <- 0
# x_test_tbl[x_test_tbl$SUM_DATA_MB == -Inf,] <- 0
# x_test_tbl[x_test_tbl$SUM_DATA_MIN == -Inf,] <- 0
#-----------------to CSI_STEP2-----------------------------------------------------------------------------------------------------------------------------------------------














