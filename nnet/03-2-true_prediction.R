# setwd("~/Documents/Projects/OneMoment/bads-ws1718-group07/submission")
#----------------------------------------------------------------------------------
# perform final prediction 
#----------------------------------------------------------------------------------
# training on test set using parameters from tuning
# predicting on train set
# translating predicted probability of return to 1/0 according to tau-value
#----------------------------------------------------------------------------------

library(caret)
library(nnet)
library(pROC)
library(data.table)

#----------------------------------------------------------------------------------
# nnet function that returns predictions and loss as measure
#----------------------------------------------------------------------------------
make_nnet <- function(train, test, par){
  # trains and nnet and makes predictions, then calculates loss as measure
  # train   : dataset for training
  # test    : dataset for prediction 
  # par     : parameter for nnet 
  neunet <- nnet(return~. -order_item_id - tau - tau_v, data = train,
                    trace = FALSE, maxit = 1000,   # general options
                    size = par[1], decay = par[2]) # tuned parameter from par_tuning.R
  yhat   <- predict(neunet, newdata = test, type = "raw")
  
  # transform prob.prediction to 1/0-prediction 
  cv_yhat_dt_zerone      <- 1:length(test$return)
  cv_yhat_dt_zerone[yhat >= test$tau_v] <- 1
  cv_yhat_dt_zerone[yhat <  test$tau_v] <- 0
  
  results <- list(data.table("order_item_id" = test$order_item_id, # unique id
                             "return"        = test$return,           # actual return
                             "yhat.01"       = cv_yhat_dt_zerone), # predicted return
                  neunet)  
  return(results)
}

#----------------------------------------------------------------------------------
# train nnets and make predictions (on the four decomposed subsets)

# load data and decompose dataset in pure subsets
load(file = "./data/known-unknown-data.RData")
source(file="Decompose_Dataset.R")

#----------------------------------------------------------------------------------
# load subsets for different models
# .f   : full model
# .u   : model without user_retrate
# .i   : model without item_retrate
# .iu  : model without user_retrate and item_retrate
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# .f       : full model
#----------------------------------------------------------------------------------
known         <- known.f
unknown       <- unknown.f
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
split.train   <- known.n       # comes from additional data preparation
split.test    <- unknown.n     # 

# tuned parameter from par_tuning.R and tau_tuning.R
par <- c(5, 1) 
tau <- c(0.4353333, 0.5608889, 0.5624444, 0.5902222, 0.6292222, 0.6315556)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
  split.train$tau_v[split.train$tau == tau_c] <- tau[tau_c]
  split.test$tau_v[split.test$tau == tau_c]   <- tau[tau_c]
}

# make prediction 
results.f <- make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# .u       : model without user_retrate
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown <- unknown.u   
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$user_retrate   <- NULL   # remove user_retrate (uncertain categories)
unknown.n$user_retrate <- NULL   #
split.train   <- known.n         # comes from additional data preparation
split.test    <- unknown.n       # 

# tuned parameter from par_tuning.R and tau_tuning.R
par <- c(9, 0.8)
tau <- c(0.4891111, 0.5405556, 0.5768889, 0.6043333, 0.6115556, 0.6057778)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
  split.train$tau_v[split.train$tau == tau_c] <- tau[tau_c]
  split.test$tau_v[split.test$tau == tau_c]   <- tau[tau_c]
}

# make prediction 
results.u <- make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# .i       : model without user_retrate
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.i) # can also use .f for training, variables are pure
unknown <- unknown.i               # 
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   <- NULL   # remove user_retrate (uncertain categories)
unknown.n$item_retrate <- NULL   #
split.test  <- unknown.n         # comes from additional data preparation
split.train <- known.n           #

# tuned parameter from par_tuning.R and tau_tuning.R
par <- c(13, 0.5)
tau <- c(0.4447778, 0.5335556, 0.5724444, 0.5688889, 0.6175556, 0.5998889)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
  split.train$tau_v[split.train$tau == tau_c] <- tau[tau_c]
  split.test$tau_v[split.test$tau == tau_c]   <- tau[tau_c]
}

# make prediction 
results.i <- make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# .iu      : model without user_retrate AND item_retrate
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.i, known.u, known.iu)
unknown <- unknown.iu
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   <- NULL   # remove user_retrate (uncertain categories)
known.n$user_retrate   <- NULL   #
unknown.n$item_retrate <- NULL   #
unknown.n$user_retrate <- NULL   #
                                 #   
split.test  <- unknown.n         # output of additional data preparation 
split.train <- known.n           # 

# tuned parameter from par_tuning.R and tau_tuning.R
par <- c(11, 0.5)
tau <- c(0.5034444, 0.5471111, 0.5734444, 0.6081111, 0.6126667, 0.6323333)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
  split.train$tau_v[split.train$tau == tau_c] <- tau[tau_c]
  split.test$tau_v[split.test$tau == tau_c]   <- tau[tau_c]
}

# make prediction 
results.iu <- make_nnet(split.train, split.test, par)


#----------------------------------------------------------------------------------
# evaluate performance 
#---------------------------------------------------------------------------------- 
# combine predictions of subsets
all <- rbind(results.f[[1]], results.u[[1]], results.i[[1]], results.iu[[1]])
setkey(all, order_item_id)     # order by unique id

sum(all$yhat.01)/50000  # return rate 0.3937

