#-------------------INIT model
lapply(c("reticulate","keras", "yardstick"), require, character.only = T)

ORIG <- x_train_tbl

y_train_vec <- as.vector(unlist(x_train_tbl$CSI))
x_train_tbl[,c(1:7,28)] <- NULL

x_train_tbl<-as.data.frame(x_train_tbl[,-c(1:7,28)] )


#------------------------------------------Multi-Layer Perceptron (MLP)------------------------------------------------------------

model_keras <- keras_model_sequential()
model_keras %>%
  #(1) Hidden Layer
  layer_dense (units              = 25, #=> Num Of Nodes
               kernel_initializer = "uniform", #"uniform",
               #kernel_regularizer = regularizer_l1_l2(l1 = .09, l2 = .09),
               activation         = "relu",    
               input_shape        = ncol(x_train_tbl)) %>% 
  
  layer_dropout (rate = 0.1) %>%  #=> Dropout Below 10%: Prevent overfitting
  
  #(2) Hidden Layer
  layer_dense (units              = 25,
               kernel_initializer = "uniform", 
               #kernel_regularizer = regularizer_l1_l2(l1 = .09, l2 = .09),
               activation         = "tanh" ) %>% # "tanh" 
  
  layer_dropout (rate = 0.1) %>%  
  
  # (3) Output Layer-----------------------------------------------------
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               #use_bias = as.vector(CSI$DL_VOLUME_LTE),
               kernel_initializer = "uniform", 
               activation         = "sigmoid") %>% #=> Common for Binary
  
  # (4) Compile Model-----------------------------------------------------
  compile ( optimizer = optimizer_adam(lr = 0.001, decay = 1e-6), #'adam', #=> Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('accuracy') ) #=> Train/Test Evaluation

#optimizer = optimizer_sgd(lr = 0.01, momentum = 0.0, decay = 0.0, nesterov = F),

#------------------------------------------------Check------------------------------------------------------------------------------
model_keras
#------------------------------------------------Fit ‘keras_model’ to the ‘Training’ Data-------------------------------------------
system.time ( 
  history <- fit (
    object           = model_keras,             # => Our Model
    x                = as.matrix(x_train_tbl), #=> Matrix
    y                = as.vector(y_train_vec),             #=> Numeric Vector 
    batch_size       = 30,     #=> #OfSamples/gradient update in each epoch
    epochs           = 50,     #=> Control Training cycles
    validation_split = 30.0) ) #=> Include 30% data for 'Validation' Model

#We want MINIMAL difference between the Validation & Training accuracy.
print (history)

plot(history)

