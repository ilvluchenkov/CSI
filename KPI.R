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

lapply(c("tidyverse", "HDtweedie", 
         "fields", "forcats", "ggalluvial", 
         "ggpubr", "modelr", "recipes", "rminer", 
         "tabplot", "webshot", "ISLR", "e1071", 
         "gamlss", "randomForest", "ROCR", "caret", 
         "rpart.plot", "Hmisc", "ggplot2","scorecard", "data.table", "dplyr", "readr", "bit64"), 
       require, character.only = T)


#-----------------------------BS_CNN_KPI--------------------------------------------------------------------------------------------------------------------
clc()
read_bs_chnn_kpi <- function(bs_chnn_kpi){
  
  bs_chnn_kpi <-read_delim_chunked('dataset/bs_chnn_kpi.csv',
                                   callback = DataFrameCallback$new(function(x, pos) subset(x, select = c(1:8))),
                                   col_types = NULL,
                                   progress = F,
                                   chunk_size = 10000,
                                   delim = ';',
                                   quote = "")
  
  bs_chnn_kpi$MON <- as.integer(substr(bs_chnn_kpi$T_DATE, 4,5))
  bs_chnn_kpi <- bs_chnn_kpi %>% filter(bs_chnn_kpi$MON %in% c(4,5))
  bs_chnn_kpi$MON <- NULL
  #bs_chnn_kpi <- bs_chnn_kpi %>% filter(bs_chnn_kpi$CELL_LAC_ID %in% df_consume_feat$CELL_LAC_ID)
  bs_chnn_kpi[is.na(bs_chnn_kpi)] <- 0
  bs_chnn_kpi[,-c(1,2)] <- apply(bs_chnn_kpi[,-c(1,2)], 2, function(x) as.numeric(gsub("^\\,", "", x)))
  #bs_chnn_kpi <- data.frame(bs_chnn_kpi[!duplicated(bs_chnn_kpi),])
  
  
  #bs_chnn_kpi[3:8] <- apply(bs_chnn_kpi[3:8], 2, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)})
  
  # setDT(bs_chnn_kpi)
  # bs_chnn_kpi <- bs_chnn_kpi[, lapply(.SD, sum), by = c("T_DATE", "CELL_LAC_ID" )]
  
  rownames(bs_chnn_kpi) <- NULL
  
  return(bs_chnn_kpi)
}

bs_chnn_kpi <- read_bs_chnn_kpi(bs_chnn_kpi = bs_chnn_kpi)

#saveRDS(bs_chnn_kpi, file = "~/WORK/scoring/BS_CHNN_KPI.RDS")
#bs_chnn_kpi <- BS_CHNN_KPI <-NULL


#-----------------------------BS_AVG_KPI--------------------------------------------------------------------------------------------------------------------
#READ
read_bs_avg_kpi <- function(bs_avg_kpi){
  
  bs_avg_kpi <-read_delim_chunked('dataset/bs_avg_kpi.csv',
                                  callback = DataFrameCallback$new(function(x, pos) subset(x, select = c(1:5,32:33))),
                                  col_types = NULL,
                                  progress = F,
                                  chunk_size = 10000,
                                  delim = ';',
                                  quote = "")
  
  bs_avg_kpi$MON <- as.integer(substr(bs_avg_kpi$T_DATE, 4,5))
  bs_avg_kpi <- bs_avg_kpi %>% filter(bs_avg_kpi$MON %in% c(4,5))
  bs_avg_kpi$MON <- NULL
  #bs_avg_kpi <- bs_avg_kpi %>% filter(bs_avg_kpi$CELL_LAC_ID %in% df_consume_feat$CELL_LAC_ID)
  bs_avg_kpi[is.na(bs_avg_kpi)] <- 0
  bs_avg_kpi[,-c(1,2)] <- apply(bs_avg_kpi[,-c(1,2)], 2, function(x) as.numeric(gsub("^\\,", "0.", x)))
  #bs_avg_kpi <- data.frame(bs_avg_kpi[!duplicated(bs_avg_kpi),])
  
  # setDT(bs_avg_kpi)
  # bs_avg_kpi <- bs_avg_kpi[, lapply(.SD, sum), by = c("T_DATE", "CELL_LAC_ID" )]
  #bs_avg_kpi[-c(1,2)] <- apply(bs_avg_kpi[-c(1,2)], 2, function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)})
  
  rownames(bs_avg_kpi) <- NULL
  
  return(bs_avg_kpi)
}

bs_avg_kpi <- read_bs_avg_kpi(bs_avg_kpi = bs_avg_kpi)

#saveRDS(bs_avg_kpi, file = "~/WORK/scoring/BS_AVG_KPI.RDS")
#bs_avg_kpi <- BS_AVG_KPI <-NULL

#---------------------------KPI's_JOIN-------------------------------------------------------------------------------------
clc()
df_kpi <- inner_join(BS_AVG_KPI,BS_CHNN_KPI,  by = c("T_DATE" = "T_DATE", "CELL_LAC_ID" = "CELL_LAC_ID"))
BS_AVG_KPI <- NULL
BS_CHNN_KPI <- NULL
clc()
# --------------------------------------------------------------------------------------------------------------------------
#df_kpi[,c(1,3:6,13)] <- NULL
df_kpi[,c(1,5,6)] <- NULL
colnames(df_kpi)
setDT(df_kpi)
df_kpi <- df_kpi[, lapply(.SD, sum), by = c("CELL_LAC_ID")]

preprocessParams <- preProcess(df_kpi, method=c("center", "scale", "pca"))
df_kpi <- predict(preprocessParams, df_kpi)
preprocessParams <- NULL















