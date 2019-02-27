

#----(1) Predicted Class-----------------------------------------------------------------------------------------------------------

#x_test_tbl <- as.matrix(x_test_tbl)
#x_test_tbl[is.na(x_test_tbl)] <- 0

ORIG_TEST <- x_test_tbl
x_test_tbl<-as.data.frame(x_test_tbl[,-c(1:6)])

#x_test_tbl <- subset(x_test_tbl, select = intersect(x_train_tbl, colnames(x_test_tbl)))

yhat_keras_class_vec <- 
  predict_classes (object = model_keras, 
                   x = as.matrix(x_test_tbl)) %>%
  as.vector()


write.csv(vec, '/home/rstudio/WORK/scoring/vec_1.csv')



#----(2) Predicted Class “Probability”
yhat_keras_prob_vec <-
  predict_proba (object = model_keras,
                 x = as.matrix(x_test_tbl)) %>%
  as.vector()


#Inspect Performance With Yardstick

estimates_keras_tbl <- tibble(
  truth      = as.factor(vec) %>% #!!!!!!!!!!!!!x_test_vec
    fct_recode (Positive = "1", Negative = "0"),
  estimate   = as.factor(vec) %>% 
    fct_recode (Positive = "1", Negative = "0"),
  class_prob = yhat_keras_prob_vec)

options(scipen = 999)

#
options (yardstick.event_first = FALSE)


#---------------------------------------------METRICS
#CONFUSION TABLE
estimates_keras_tbl %>% conf_mat(truth, estimate)

#ACCURACY
estimates_keras_tbl %>% metrics(truth, estimate)

#AUC. ROC Area Under the Curve (AUC) measuremen/used to compare different classifiers, and to compare to randomly guessing (AUC_random = 0.50)
estimates_keras_tbl %>% roc_auc(truth, class_prob)

#PRECISION AND RECALL
str(estimates_keras_tbl)
estimates_keras_tbl <- as.data.frame(estimates_keras_tbl)
estimates_keras_tbl$truth <- as.factor(estimates_keras_tbl$truth)
estimates_keras_tbl$estimate <- as.factor(estimates_keras_tbl$estimate)

estimates_keras_tbl %>%  tibble (
  precision = estimates_keras_tbl %>% precision(truth, estimate),
  recall    = estimates_keras_tbl %>% recall(truth, estimate) )

#F1 Score
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)

#AUC
pos.sample <- sample(estimates_keras_tbl$truth == 'Positive', replace=T)
neg.sample <- sample(estimates_keras_tbl$truth == 'Negative', replace=T)
mean(1.0*(pos.sample > neg.sample) + 0.5*(pos.sample==neg.sample))

#AUC
#mean(sample(estimates_keras_tbl$truth == 'Positive', replace = T) > sample(estimates_keras_tbl$truth == 'Negative', replace = T))
# auc_roc(yhat_keras_class_vec,x_train_tbl,y_train_vec)
#
# score.pairs <- merge(, neg.scores)
# names(score.pairs) <- c("pos.score", "neg.score")
# sum(score.pairs$pos.score > score.pairs$neg.score) / nrow(score.pairs)


explainer <- lime::lime (
  x              = x_train_tbl, 
  model          = model_keras, 
  bin_continuous = FALSE)



system.time (
  explanation <- lime::explain (
    x_test_tbl[1:10, ], # Just to show first 10 cases
    explainer    = explainer, 
    n_labels     = 1, # explaining a `single class`(Polarity)
    n_features   = 4, # returns top four features critical to each case
    kernel_width = 0.5) ) # allows us










