
colnames(x_train_tbl) <- gsub("\\.", "_", colnames(x_train_tbl))

SK_ID_CSI <- as.data.frame(x_train_tbl[,1:2])


#------------ Select variables using IV--------------------------------------------------------------------------------------
# Information Value 	Predictive Power                |
#------------------------------------------------------
#             < 0.02 	useless for prediction          |
#         0.02 - 0.1 	weak predictor                  |  
#         0.1 - 0.3 	medium predictor                |
#         0.3 - 0.5 	strong predictor                |
#             > 0.5 	suspicious too good to be true  |
#-----------------------------------------------------|
lapply(c("tidyverse", "HDtweedie", 
         "fields", "forcats", "ggalluvial", 
         "ggpubr", "modelr", "recipes", "rminer", 
         "tabplot", "webshot", "ISLR", "e1071", 
         "gamlss", "randomForest", "ROCR", "caret", 
         "rpart.plot", "Hmisc", "ggplot2","scorecard"), 
       require, character.only = T)

x_train_tbl <- ORIG

as.data.frame(x_train_tbl)
iv = iv(x_train_tbl, y = 'CSI') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

#---------------------------
iv %>%
  knitr::kable()

iv <- iv %>% filter(iv$info_value > 0.01)
 
iv %>% knitr::kable()

#--------------------------------------------
x_train_tbl <- subset(x_train_tbl, select = intersect(iv$variable, colnames(x_train_tbl)))

x_train_tbl <- cbind(SK_ID_CSI, x_train_tbl)


#-------------------------------------------------------------------------------------------------------------------------------
#-------------Weight of evidence binning----------------------------------------------------------------------------------------
#-------------generates optimal binning for numerical, factor and categorical variables using methods including tree-like segmentation or chi-square merge

bins = woebin(x_train_tbl, y = 'CSI')

#for(l in 1:length(result[,10])){if(!is.na(result[l])) print(result[l])}

# bins$COM_CAT_32 %>%
#   knitr::kable()
# 
# woebin_plot(bins$COM_CAT_32)

#---------------------Apply bins----------------------------------------------------------------------------------------

data_woe = woebin_ply( x_train_tbl, bins ) %>% as_tibble()

#---------------------Feature Selection----IV < 0.02.-Lasso---------------------------------------------------------------------------
#----LASSO отбираются признаки, оказывающие наибольшее влияние на вектор ответов

set.seed(1)

vars = names(data_woe)
vars = vars[ vars != 'CSI']


formula = as.formula( paste( 'CSI ~', paste( vars , collapse = '+')))

lasso = oetteR::f_train_lasso( data = data_woe
                               , p = NULL
                               , formula = formula
                               , k = 50
                               , family = 'binomial') #For classification use 'binomial'. Performance metric MSE will be replaced with AUC.


plotly::ggplotly( lasso$plot_mse )

p = lasso$plot_coef + theme( legend.position = 'none')
plotly::ggplotly( p, tooltip = c('x','y','color'))
#the dashed line marks the lambda with min(MSE) the solid line marks lambda for which MSE is in range of min(MSE)+SEM

#-------------------------FORMULA-----------------------------------------------------------------------------------------------------

lasso$tib_all %>%
  filter(lambda == lambda_1se) %>%
  dplyr::select(lambda_1se, auc, n_coeff_before_lasso, n_coeff_after_lasso) %>% 
  knitr::kable()

#---------------------BUILD and interpret the model

formula = as.formula(lasso$formula_str_lambda_1se)

m = glm(formula, data_woe, family = 'binomial')

model <- as.data.frame(m$model)

weights <- as.data.frame(m$weights)

final <- cbind(SK_ID, weights, model)

colnames(final)[c(1,2)] <- c("SK_ID","weights")

x_train_tbl <- final
final <- NULL