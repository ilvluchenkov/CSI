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

#---------------------------------------INNER_JOIN VOICE AND DATA---------------------------------------------------------------------------------------
clc()
df_data_voice <- full_join(subs_bs_data_session_test, subs_bs_voice_session_test, by = c("SK_ID" = "SK_ID", "CELL_LAC_ID"="CELL_LAC_ID", "T_DATE" = "T_DATE"))
subs_bs_data_session_test <- NULL
subs_bs_voice_session_test <- NULL
df_data_voice[is.na(df_data_voice)] <- 0
df_data_voice$T_DATE <- NULL


setDT(df_data_voice)
df_data_voice <- df_data_voice[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID")]
preprocessParams <- preProcess(head(df_data_voice)[,-c(1,2)], method=c("center", "scale", "pca"))
df_data_voice <- predict(preprocessParams, df_data_voice)
preprocessParams <- NULL

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

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

df_feat_cons <- full_join(subs_features_test, subs_bs_consumption_test, by = c("SK_ID" = "SK_ID"))

subs_features_test <- NULL
subs_bs_consumption_test <- NULL

preprocessParams <- preProcess(df_feat_cons[,-c(1)], method=c("center", "scale", "pca"))
df_feat_cons <- predict(preprocessParams, df_feat_cons)
preprocessParams <- NULL


DF <- full_join(df_data_voice, df_feat_cons, by = c("SK_ID" = "SK_ID"))


#-----KPI
DF <- full_join(DF, df_kpi, by = c("CELL_LAC_ID" = "CELL_LAC_ID"))


DF[is.na(DF)] <- 0
DF <- DF[DF$SK_ID != 0,]
DF$CELL_LAC_ID <- NULL

DF <- as.data.frame(DF)
DF <- aggregate(DF, list(SK_ID = DF$SK_ID), mean)
DF[,2] <- NULL

 
#-----------------------------------------SUBS_CSI_test----------------------------------------------------------------------------------------------

read_subs_csi_test <- function(subs_csi_test){
  
  subs_csi_test <- subs_csi_test <- read_delim("dataset/test/subs_csi_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_csi_test[is.na(subs_csi_test)] <- 0
  rownames(subs_csi_test) <- NULL
  return(subs_csi_test)
  
}

subs_csi_test <- read_subs_csi_test(subs_csi_test = subs_csi_test)


#-----------------------------------------SUBS_CSI_test----------------------------------------------------------------------------------------------

x_test_tbl <- inner_join(subs_csi_test, DF, by = c("SK_ID" = "SK_ID"))



