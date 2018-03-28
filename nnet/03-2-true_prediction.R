#----------------------------------------------------------------------------------
# make prediction on training dataset to see how tuned nnet performs
#----------------------------------------------------------------------------------
# splits training set into known and unknown
# trains on known and predicts on unknown 
# checks performance (loss function) on unknown
#----------------------------------------------------------------------------------

library(caret)
library(nnet)
library(data.table)

load(file  = "./data/known-unknown-data.RData")

# load threshold values for tau from 02-tau_tuning
load(file  = "./data/cv_lists-tau-tuning.RData")
source(file = "helperfunctions.R")
measure.f  = helper.cvlist.tau(cv.list.f)
measure.u  = helper.cvlist.tau(cv.list.u)
measure.i  = helper.cvlist.tau(cv.list.i)
measure.iu = helper.cvlist.tau(cv.list.iu)

#----------------------------------------------------------------------------------
# nnet function that returns predictions and loss as measure
#----------------------------------------------------------------------------------
make_nnet = function(train, test, par){
    # trains and nnet and makes predictions, returns predictions
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
                         "yhat.01"       = cv_yhat_dt_zerone)  # predicted return
  return(results)
}

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
known       = known.f
unknown     = unknown.f
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known       = known.n       # comes from additional data preparation
unknown     = unknown.n     # 

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(5, 0.5)
tau = measure.f$tau$mean
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    known$tau_v[known$tau     == tau_c] = tau[tau_c]
    unknown$tau_v[unknown$tau == tau_c] = tau[tau_c]
}

# make prediction 
results.f = make_nnet(known, unknown, par)

#----------------------------------------------------------------------------------
# .u       : model without user_retrate
#----------------------------------------------------------------------------------
known       = rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown     = unknown.u   
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$user_retrate   = NULL   # remove user_retrate (uncertain categories)
unknown.n$user_retrate = NULL   #
known       = known.n           # comes from additional data preparation
unknown     = unknown.n         # 

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(11, 1)
tau = measure.u$tau$mean
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    known$tau_v[known$tau     == tau_c] = tau[tau_c]
    unknown$tau_v[unknown$tau == tau_c] = tau[tau_c]
}

# make prediction 
results.u = make_nnet(known, unknown, par)

#----------------------------------------------------------------------------------
# .i       : model without user_retrate
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.i) # can also use .f for training, variables are pure
unknown = unknown.i               # 
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   = NULL   # remove user_retrate (uncertain categories)
unknown.n$item_retrate = NULL   #
unknown     = unknown.n         # comes from additional data preparation
known       = known.n           #

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(7, 1)
tau = measure.i$tau$mean
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    known$tau_v[known$tau     == tau_c] = tau[tau_c]
    unknown$tau_v[unknown$tau == tau_c] = tau[tau_c]
}

# make prediction 
results.i = make_nnet(known, unknown, par)

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
unknown = unknown.n             # output of additional data preparation 
known   = known.n               # 

# tuned parameter from par_tuning.R and tau_tuning.R
par = c(13, 0.5)
tau = measure.iu$tau$mean
# add tau values for each tau-category (tau_c) to dataset
for (tau_c in 1:6){
    known$tau_v[known$tau     == tau_c] = tau[tau_c]
    unknown$tau_v[unknown$tau == tau_c] = tau[tau_c]
}

# make prediction 
results.iu = make_nnet(known, unknown, par)


#----------------------------------------------------------------------------------
# evaluate performance 
#---------------------------------------------------------------------------------- 
# combine predictions of subsets
all = rbind(results.f, results.u, results.i, results.iu)
setkey(all, order_item_id)     # order by unique id

sum(all$yhat.01)/50000  # return rate : 38.95%
