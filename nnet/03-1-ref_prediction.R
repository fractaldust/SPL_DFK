#----------------------------------------------------------------------------------
# make prediction on training dataset to see how tuned nnet performs
#----------------------------------------------------------------------------------
# splits training set into split.train and split.test
# trains on split.train and predicts on split.test 
# checks performance (loss function) on split.test
#----------------------------------------------------------------------------------

library(caret)
library(nnet)
library(data.table)

source(file = "helperfunctions.R")
load(file   = "./data/known-unknown-data.RData")
real_price  = data.frame(known$order_item_id, known$item_price)  # for loss function
colnames(real_price) = c("order_item_id", "item_price")

# splits train set into split.train and split.test
set.seed(1234) # set.seed for reproducibility
split.idx      = createDataPartition(y = known$return, p = 0.75, list = FALSE)
split.train    = known[split.idx,]    # 75% of known dataset for training
split.test     = known[-split.idx,]   # 25% of known dataset for prediction 
real_price_ts  = real_price$item_price[split.test$order_item_id] # for loss function

#----------------------------------------------------------------------------------
# nnet function that returns predictions and loss as measure
#----------------------------------------------------------------------------------
make_nnet = function(train, test, par){
    # trains and nnet and makes predictions, then calculates loss as measure
    # train   : dataset for training
    # test    : dataset for prediction 
    # par     : parameter for nnet 
    neunet = nnet(return~. -order_item_id - tau - tau_v, data = train,
                  trace = FALSE, maxit = 1000,   # general options
                  size = par[1], decay = par[2]) # tuned parameter from par_tuning.R
    yhat   = predict(neunet, newdata = test, type = "raw")
  
    # transform prob.prediction to 1/0-prediction 
    cv_yhat_dt_zerone      = 1:length(test$return)
    cv_yhat_dt_zerone[yhat >= test$tau_v] = 1
    cv_yhat_dt_zerone[yhat <  test$tau_v] = 0
  
    results = data.table("order_item_id" = test$order_item_id, # unique id
                         "return"        = test$return,        # actual return
                         "yhat.01"       = cv_yhat_dt_zerone)  # predicted return
  return(results)
}

#----------------------------------------------------------------------------------
# train nnets and make predictions (on the four decomposed subsets)
known   = split.train
unknown = split.test

# decompose dataset in pure subsets
source(file = "Decompose_Dataset.R")

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
known         = known.f
unknown       = unknown.f
# additional data preparation for nnet
source(file   = "./nnet/00-3-nnet_DataPrep.R")
split.train   = known.n       # comes from additional data preparation
split.test    = unknown.n     # 

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(5, 0.5)
tau = c(0.466, 0.496, 0.524, 0.554, 0.634, 0.612)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    split.train$tau_v[split.train$tau == tau_c] = tau[tau_c]
    split.test$tau_v[split.test$tau   == tau_c] = tau[tau_c]
}

# make prediction 
results.f = make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# .u       : model without user_retrate
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown = unknown.u   
# additional data preparation for nnet
source(file   = "./nnet/00-3-nnet_DataPrep.R")
known.n$user_retrate   = NULL   # remove user_retrate (uncertain categories)
unknown.n$user_retrate = NULL   #
split.train   = known.n         # comes from additional data preparation
split.test    = unknown.n       # 

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(11, 1)
tau = c(0.516, 0.532, 0.602, 0.624, 0.640, 0.646)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    split.train$tau_v[split.train$tau == tau_c] = tau[tau_c]
    split.test$tau_v[split.test$tau   == tau_c] = tau[tau_c]
}

# make prediction 
results.u = make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# .i       : model without user_retrate
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.i) # can also use .f for training, variables are pure
unknown = unknown.i               # 
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   = NULL   # remove user_retrate (uncertain categories)
unknown.n$item_retrate = NULL   #
split.test  = unknown.n         # comes from additional data preparation
split.train = known.n           #

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(7, 1)
tau = c(0.520, 0.508, 0.492, 0.462, 0.542, 0.616)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    split.train$tau_v[split.train$tau == tau_c] = tau[tau_c]
    split.test$tau_v[split.test$tau   == tau_c] = tau[tau_c]
}

# make prediction 
results.i = make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# .iu      : model without user_retrate AND item_retrate
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.i, known.u, known.iu)
unknown = unknown.iu
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   = NULL   # remove user_retrate (uncertain categories)
known.n$user_retrate   = NULL   #
unknown.n$item_retrate = NULL   #
unknown.n$user_retrate = NULL   #
                                #   
split.test  = unknown.n         # output of additional data preparation 
split.train = known.n           # 

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(13, 0.5)
tau = c(0.534, 0.572, 0.594, 0.604, 0.616, 0.632)
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    split.train$tau_v[split.train$tau == tau_c] = tau[tau_c]
    split.test$tau_v[split.test$tau   == tau_c] = tau[tau_c]
}

# make prediction 
results.iu = make_nnet(split.train, split.test, par)

#----------------------------------------------------------------------------------
# evaluate performance 
#---------------------------------------------------------------------------------- 
# combine predictions of subsets
all = rbind(results.f, results.u, results.i, results.iu)
setkey(all, order_item_id)     # order by unique id

# calculate loss and AUC measure
loss = helper.calcloss(all$return, all$yhat.01, real_price_ts)
loss                    # loss measure: -238717.0
sum(all$yhat.01)/25000  # return rate : 37.96%
