#---------------------------------DATA FOR TRAIN-----------------------------------------------------------------------------------
# ORIG <- x_train_tbl
# x_train_tbl<-ORIG
#V-1
#y_train_vec <- as.vector(unlist(x_train_tbl[,2]))
# setDT(x_train_tbl)
# x_train_tbl <- x_train_tbl[, lapply(.SD, mean), by = c("SK_ID","CONTACT_DATE" )]
#x_train_tbl <- as.matrix(x_train_tbl[,-c(1:3)])
#-----------------------------
#V-2
# bins = woebin(x_train_tbl, y = 'CSI')
# x_train_tbl = woebin_ply( x_train_tbl, bins ) %>% as_tibble()
# y_train_vec <- as.matrix(x_train_tbl[,c(1)])
# x_train_tbl <- as.matrix(x_train_tbl[,-c(1)])
#--------------------------------------------------------------------------------------------

x_train_tbl <- as.data.frame(lapply(x_train_tbl, function(x) as.numeric(as.character(x))))

ORIG_train <- x_train_tbl <- ORIG_train
ORIG_test <- x_test_tbl <- ORIG_test


#X-TRAIN--------------------------------------------------------------------
#V-3

colnames(x_train_tbl) <- gsub("\\.", "_", colnames(x_train_tbl))
SK_ID_CSI <- as.data.frame(ORIG[,c(1:3)])
#x_train_tbl[,c(4:50)] <- scale(x_train_tbl[,c(4:50)])


iv = iv(ORIG, y = 'CSI.x') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )
iv %>%
  knitr::kable()

iv <- iv %>% filter(iv$info_value > 0.02)
iv %>% knitr::kable()

x_train_tbl <- subset(ORIG, select = intersect(iv$variable, colnames(ORIG)))
x_train_tbl <- cbind(SK_ID_CSI, x_train_tbl)

y_train_vec <- as.vector(unlist(x_train_tbl[,2]))
x_train_tbl <- as.matrix(x_train_tbl[,-c(1:3)])





#---X-TEST-------------------------------------------------------------------------------------------------------------

x_test_tbl$CELL_LAC_ID.y <- NULL
colnames(x_test_tbl) <- gsub("\\.", "_", colnames(x_test_tbl))
SK_ID <- as.data.frame(ORIG_TEST[,1:2])
#x_test_tbl[,c(3:49)] <- scale(x_test_tbl[,c(3:49)])

x_test_tbl <- subset(ORIG_TEST, select = intersect(iv$variable, colnames(ORIG_TEST)))

#x_test_tbl <- cbind(SK_ID, x_test_tbl)





