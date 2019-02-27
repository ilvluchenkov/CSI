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

#---------------------------------------INNER_JOIN VOICE AND DATA---------------------------------------------------------------------------------------
clc()
df_data_voice <- full_join(subs_bs_data_session_train, subs_bs_voice_session_train, by = c("SK_ID" = "SK_ID", "CELL_LAC_ID"="CELL_LAC_ID", "T_DATE" = "T_DATE"))
subs_bs_data_session_train <- NULL
subs_bs_voice_session_train <- NULL
df_data_voice[is.na(df_data_voice)] <- 0
df_data_voice$T_DATE <- NULL


setDT(df_data_voice)
df_data_voice <- df_data_voice[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID")]
preprocessParams <- preProcess(head(df_data_voice)[,-c(1,2)], method=c("center", "scale", "pca"))
df_data_voice <- predict(preprocessParams, df_data_voice)
preprocessParams <- NULL

clc()
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

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

df_feat_cons <- full_join(subs_features_train, subs_bs_consumption_train, by = c("SK_ID" = "SK_ID"))

subs_features_train <- NULL
subs_bs_consumption_train <- NULL

preprocessParams <- preProcess(df_feat_cons[,-c(1)], method=c("center", "scale", "pca"))
df_feat_cons <- predict(preprocessParams, df_feat_cons)
preprocessParams <- NULL

DF <- full_join(df_data_voice, df_feat_cons, by = c("SK_ID" = "SK_ID"))
DF <- full_join(DF, df_kpi, by = c("CELL_LAC_ID" = "CELL_LAC_ID"))
DF[is.na(DF)] <- 0
DF <- DF[DF$SK_ID != 0,]
DF$CELL_LAC_ID <- NULL
DF <- as.data.frame(DF)
DF <- aggregate(DF, list(SK_ID = DF$SK_ID), mean)
DF[,2] <- NULL

#-----------------------------------------SUBS_CSI_train----------------------------------------------------------------------------------------------

read_subs_csi_train <- function(subs_csi_train){
  
  subs_csi_train <- subs_csi_train <- read_delim("dataset/train/subs_csi_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  subs_csi_train[is.na(subs_csi_train)] <- 0
  rownames(subs_csi_train) <- NULL
  return(subs_csi_train)
  
}

subs_csi_train <- read_subs_csi_train(subs_csi_train = subs_csi_train)


#-----------------------------------------SUBS_CSI_train----------------------------------------------------------------------------------------------

x_train_tbl <- inner_join(subs_csi_train, DF, by = c("SK_ID" = "SK_ID"))


# Drop 'DV' which 'Polarity' Column
bravo_name_df <- x_train_tbl[,-c(2,3)]
# Get Correlation Matrix 'M'
suppressPackageStartupMessages(library (corrplot)) 
M <- cor (bravo_name_df)
corrplot(M, type = 'upper', title = 'Correlation Matrix for IVs')



bravo_log <- cor (log (bravo_name_df))
corrplot (bravo_name_df, order = "hclust", addrect = 5,  method = 'ellipse', title = 'FPC: Log-Transform')


p.mat <- cor.mtest(bravo_name_df)$p
corrplot(bravo_name_df, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")




