#----------------------------------------------------------------------------------
# parameter tuning and cross validation
#----------------------------------------------------------------------------------
# runs m repeated (different seed) k-fold cross validations for different 
# tuning parameters
# extract best settings by maximising negative loss function
#----------------------------------------------------------------------------------

load(file  = "./data/known-unknown-data.RData")
real_price = data.frame(known$order_item_id, known$item_price) # for loss function
colnames(real_price) = c("order_item_id", "item_price")

# decompose dataset in pure subsets
source(file = "Decompose_Dataset.R")
source(file = "helperfunctions.R")

# settings for tuning and cross validation 
m = 6 # repeated Cross Validation (same algorithm but different random split)
k = 3 # 3-fold cross validation
parameters = expand.grid("size"  = seq(from = 3, to = 15, by = 2),
                         "decay" = c(0.01, 0.1, 0.5, 0.8, 1))
tau_candidates = 0.002*125:425 # to calculate optimal loss

#----------------------------------------------------------------------------------
# load subsets for different models
# .f   : full model
# .u   : model without user_retrate
# .i   : model without item_retrate
# .iu  : model without user_retrate and item_retrate
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# .f       : full model, n = 25.266
#----------------------------------------------------------------------------------
known   = known.f
unknown = unknown.f
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R") 
known   = known.n       # output of additional data preparation
unknown = unknown.n     # 

# perform repeatet cross validation 
source(file = "./nnet/00-2-rep_cv.R")
cv.list.f   = cv.list   # store result of repeated cross validation 

#----------------------------------------------------------------------------------
# .u       : model without user_retrate, n = 92.409
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown = unknown.u               #
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$user_retrate   = NULL     # remove user_retrate (uncertain categories)
unknown.n$user_retrate = NULL     #
                                  #
known   = known.n                 # output of additional data preparation
unknown = unknown.n               #

# perform repeatet cross validation
source(file = "./nnet/00-2-rep_cv.R")
cv.list.u   = cv.list             # store result of repeated cross validation

#----------------------------------------------------------------------------------
# .i       : model without user_retrate, n = 27.123
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.i) # can also use .f for training, variables are pure
unknown = unknown.i               #
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   = NULL     # remove user_retrate (uncertain categories)
unknown.n$item_retrate = NULL     #
                                  #
known   = known.n                 # output of additional data preparation
unknown = unknown.n               #

# perform repeatet cross validation
source(file = "./nnet/00-2-rep_cv.R")
cv.list.i   = cv.list             # store result of repeated cross validation

#----------------------------------------------------------------------------------
# .iu      : model without user_retrate AND item_retrate, n = 100.000
#----------------------------------------------------------------------------------
known   = rbind(known.f, known.i, known.u, known.iu)
unknown = unknown.i
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   = NULL     # remove user_retrate (uncertain categories)
known.n$user_retrate   = NULL     #
unknown.n$item_retrate = NULL     #
unknown.n$user_retrate = NULL     #
                                  #
known   = known.n                 # output of additional data preparation
unknown = unknown.n               #

# perform repeatet cross validation
source(file = "./nnet/00-2-rep_cv.R")
cv.list.iu  = cv.list             # store result of repeated cross validation

save(cv.list.f, cv.list.u, cv.list.iu, cv.list.i,
     file   = "./data/cv_lists-nnet-tuning.RData")

# to evaluate results
measure.f = helper.cvlist.tune(cv.list.f)
# results for different settings - better evaluation in nnet_tuning_graph.R
