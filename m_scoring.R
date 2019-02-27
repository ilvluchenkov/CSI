setwd("/home/rstudio/WORK/scoring")


library(readr)
crx_data_test_x <- read_csv("crx_data_test_x.csv", col_names = FALSE)
crx_data_train_x <- read_csv("crx_data_train_x.csv", col_names = FALSE)

crx_data_train_y <- read_csv("crx_data_train_y.csv", col_names = FALSE)
colnames(crx_data_train_y) <- "X16"



df <- cbind(crx_data_train_x, crx_data_train_y)


df$X17<- as.factor(ifelse(df$X16 == 1, "Yes", "No"))

Amelia::missmap(df)


#------------ calculate the information values
iv = iv(df[,-c(1,2,4:7,9:10,12:14,17)], y = 'X16') %>%
  as_tibble() %>%  
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv %>%
  knitr::kable()  

#-----------corr
  
correlations <- cor(df[,-c(1,2,4:7,9:10,12:14,17)],method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")  


#---------------------------------

