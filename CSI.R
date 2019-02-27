#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
clc <- function() {
  while (sink.number() > 0) sink(file=NULL)
  rm(list=ls()); gc()
  gc()
  cat("\014")
  cat("All files discriptors closed.\n")
  cat("Whole memory cleaned.\n")
}

clc()
#------------------------------------------------------------------------
lapply(c("tidyverse", "HDtweedie", 
         "fields", "forcats", "ggalluvial", 
         "ggpubr", "modelr", "recipes", "rminer", 
         "tabplot", "webshot", "ISLR", "e1071", 
         "gamlss", "randomForest", "ROCR", "caret", 
         "rpart.plot", "Hmisc", "ggplot2","scorecard", "data.table", "dplyr", "readr", "bit64"), 
       require, character.only = T)

#as.integer64(.Machine$integer.max) + 1L


#----------------------------------------SUBS_BS_CONSUMPTION_TRAIN-----------------------------------------------------------------------------------------------

read_subs_bs_consumption_train <- function(subs_bs_consumption_train){
  
  subs_bs_consumption_train <- read_delim("dataset/train/subs_bs_consumption_train.csv",";", escape_double = FALSE, trim_ws = TRUE)
  
  #subs_bs_consumption_train$DAY <- strftime(as.Date(subs_bs_consumption_train$MON, "%d.%m"), format = "%d")
  # subs_bs_consumption_train$M <- as.integer(strftime(as.Date(subs_bs_consumption_train$MON, "%d.%m"), format = "%m"))
  # subs_bs_consumption_train <- subs_bs_consumption_train %>% filter(subs_bs_consumption_train$M %in% c(4,5))
  
  subs_bs_consumption_train$MON <- NULL
  subs_bs_consumption_train[is.na(subs_bs_consumption_train)] <- 0
  subs_bs_consumption_train[,-c(1:2)] <- as.data.frame(apply(subs_bs_consumption_train[,-c(1:2)], 2, function(x) as.integer(gsub("^\\,", "",x))))
  
  #subs_bs_consumption_train <- data.frame(subs_bs_consumption_train[!duplicated(subs_bs_consumption_train),])
  #subs_bs_consumption_train <- subs_bs_consumption_train %>% drop_na()
  #subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}), by = c("SK_ID", "CELL_LAC_ID" )]
  #subs_bs_consumption_train[4:6] <- apply(subs_bs_consumption_train[4:6], 2, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)})
  #subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD,function(x){mean(x, na.rm = T)}), by = c("SK_ID", "CELL_LAC_ID" )]
  subs_bs_consumption_train[is.na(subs_bs_consumption_train)] <- 0
  
  setDT(subs_bs_consumption_train)
  subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD, sum), by = c("SK_ID","CELL_LAC_ID")]
  
  # setDT(subs_bs_consumption_train)
  # subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD, log), by = c("SK_ID")]
  #subs_bs_consumption_train <- as.data.frame(apply(subs_bs_consumption_train, 2, function(x) sub("-Inf", "1", x)))
  
  rownames(subs_bs_consumption_train) <- NULL
  
  return(subs_bs_consumption_train)
}
  
subs_bs_consumption_train <- read_subs_bs_consumption_train(subs_bs_consumption_train = subs_bs_consumption_train)


#-----------------------------------------SUBS_BS_FEATURES_TRAIN----------------------------------------------------------------------------------------------

read_subs_features_train <- function(subs_features_train){
  
  subs_features_train <- subs_features_train <- read_delim("dataset/train/subs_features_train.csv", ";", escape_double = FALSE, trim_ws = TRUE)
  
  #subs_features_train$SNAP_DATE <- as.POSIXct(subs_features_train$SNAP_DATE , format = "%d.%m.%y")
  #subs_features_train$MON <- strftime(as.Date(subs_features_train$SNAP_DATE, "%Y-%m"), format = "%m")
  #subs_features_train$DAY <-as.integer(strftime(as.Date(subs_features_train$SNAP_DATE, "%Y-%m"), format = "%d"))
  #subs_features_train$SNAP_DATE <- NULL
  
  subs_features_train$SNAP_DATE <- NULL
  subs_features_train[is.na(subs_features_train)] <- 0
  subs_features_train[,-c(3)] <- apply(subs_features_train[,-c(3)], 2, function(x) as.integer(gsub("^\\,", "", x)))
  
  #subs_features_train <- data.frame(subs_features_train[!duplicated(subs_features_train),])
  #subs_features_train <- apply(subs_features_train[4:6], 2, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)})
  
  setDT(subs_features_train)
  #subs_features_train <- subs_features_train[, lapply(.SD, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}), by = c("SK_ID")]
  subs_features_train[is.na(subs_features_train)] <- 0
  subs_features_train <- subs_features_train[, lapply(.SD, mean), by = c("SK_ID")]
  
  rownames(subs_features_train) <- NULL
  return(subs_features_train)
  
}

subs_features_train <- read_subs_features_train(subs_features_train = subs_features_train)

clc()
#--------------------------FEATURES + CONSUME BY SUBS AT 3 MONTH BEFORE RESPONDENT--------------------------------------------------------------------------------------------------------------------------------------

CSI$SK_ID <- as.integer(CSI$SK_ID)

setDT(subs_bs_consumption_train)
subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD, log), by = c("SK_ID","CELL_LAC_ID")]
subs_bs_consumption_train <- as.data.frame(subs_bs_consumption_train)
subs_bs_consumption_train[is.na(subs_bs_consumption_train)] <- 0
subs_bs_consumption_train$SUM_DATA_MB <- ifelse(grepl('-Inf', subs_bs_consumption_train$SUM_DATA_MB), max(subs_bs_consumption_train$SUM_DATA_MB), subs_bs_consumption_train$SUM_DATA_MB) 
subs_bs_consumption_train$SUM_MINUTES <- ifelse(grepl('-Inf', subs_bs_consumption_train$SUM_MINUTES), 0, subs_bs_consumption_train$SUM_MINUTES) 
subs_bs_consumption_train$SUM_DATA_MIN <- ifelse(grepl('-Inf', subs_bs_consumption_train$SUM_DATA_MIN), 0, subs_bs_consumption_train$SUM_DATA_MIN) 
#---------------------------
setDT(subs_features_train)
subs_features_train_log <- subs_features_train[,c(1,7,12:15,17:24,28:33)] 
subs_features_train <- subs_features_train[,-c(1,7,12:15,17:24,28:33)] 
subs_features_train_log <- subs_features_train_log[, lapply(.SD, log), by = c("SK_ID")]
subs_features_train_log[is.na(subs_features_train_log)] <- 0
subs_features_train_log <- as.data.frame(subs_features_train_log)
subs_features_train_log <- apply(subs_features_train_log, 2, function(x) gsub("-Inf", 0, x))
subs_features_train_log <- as.data.frame(subs_features_train_log)
subs_features_train <- cbind(subs_features_train, subs_features_train_log)
subs_features_train_log <- NULL

subs_features_train$SK_ID <- as.integer(subs_features_train$SK_ID)
clc()


df_fcon <- full_join(subs_bs_consumption_train, subs_features_train, by = c("SK_ID" = "SK_ID"))
df_fcon <- inner_join(subs_csi_train, df_fcon, by = c("SK_ID" = "SK_ID"))



x_train_tbl <- inner_join(subs_features_train,CSI,  by = c("SK_ID"= "SK_ID"))

setDT(subs_bs_consumption_train)
subs_bs_consumption_train <- subs_bs_consumption_train[, lapply(.SD, sum), by = c("SK_ID")]
x_train_tbl <- full_join(x_train_tbl,subs_bs_consumption_train,  by = c("SK_ID"= "SK_ID"))


x_train_tbl$CELL_LAC_ID.x <- NULL
x_train_tbl$CELL_LAC_ID.y <- NULL
subs_features_train <-NULL
subs_bs_consumption_train <- NULL


# setDT(x_train_tbl)
# x_train_tbl <- x_train_tbl[, lapply(.SD, sum), by = c("SK_ID", "CSI")]
# 
# #x_train_tbl <- as.data.frame(x_train_tbl)
# x_train_tbl[is.na(x_train_tbl)] <- 0





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






