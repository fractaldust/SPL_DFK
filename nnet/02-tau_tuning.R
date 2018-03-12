# setwd("~/Documents/Projects/OneMoment/bads-ws1718-group07/submission")
#----------------------------------------------------------------------------------
# find optimal tau for each tau-category (1:6)
#----------------------------------------------------------------------------------
# runs m repeated (different seed) k-fold cross validations 
# uses tuned nnet settings from par_tuning.R
# determines optimal tau for each tau-category, 
#      by maximising negative loss function (helper.loss)
#----------------------------------------------------------------------------------

library(caret)

load(file = "./data/known-unknown-data.RData")
real_price <- data.frame(known$order_item_id, known$item_price) # for loss function
colnames(real_price) <- c("order_item_id", "item_price")

# decompose dataset in pure subsets
source(file="Decompose_Dataset.R")
source(file = "helperfunctions.R")

# settings for tuning and cross validation 
m <- 6 # repeated Cross Validation (same algorithm but different random split)
k <- 3 # 3-fold cross validation
tau_candidates <- 0.002*125:425

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
known   <- known.f
unknown <- unknown.f
# tuned parameter from par_tuning.R
parameters <- expand.grid("size" = seq(from = 5, to = 5, by = 1),
                          "decay" = c(1))
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known   <- known.n       # output of additional data preparation
unknown <- unknown.n     # 

# perform repeatet cross validation 
source(file="./nnet/00-2-rep_cv.R")
cv.list.f <- cv.list     # store result of repeated cross validation 
save(cv.list, results.par, measure.list, parameters, tau_candidates, known.n, 
     file = "./data/tau-tuning-f.RData")

#----------------------------------------------------------------------------------
# .u       : model without user_retrate, n = 92.409
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown <- unknown.u   
# tuned parameter from par_tuning.R
parameters <- expand.grid("size" = seq(from = 9, to = 9, by = 1),
                          "decay" = c(0.8))
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$user_retrate   <- NULL     # remove user_retrate (uncertain categories)
unknown.n$user_retrate <- NULL     # 
                                   # 
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               # 

# perform repeatet cross validation 
source(file="./nnet/00-2-rep_cv.R")
cv.list.u <- cv.list               # store result of repeated cross validation 
save(cv.list, results.par, measure.list, parameters, tau_candidates, known.n, 
     file = "./data/tau-tuning-u.RData")


#----------------------------------------------------------------------------------
# .i       : model without user_retrate, n = 27.123
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.i) # can also use .f for training, variables are pure
unknown <- unknown.i               # 
# tuned parameter from par_tuning.R
parameters <- expand.grid("size" = seq(from = 13, to = 13, by = 1),
                          "decay" = c(0.5))
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   <- NULL     # remove user_retrate (uncertain categories)
unknown.n$item_retrate <- NULL     #
                                   # 
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               # 

# perform repeatet cross validation 
source(file="./nnet/00-2-rep_cv.R")
cv.list.i <- cv.list               # store result of repeated cross validation
save(cv.list, results.par, measure.list, parameters, tau_candidates, known.n, 
     file = "./data/tau-tuning-i.RData")

#----------------------------------------------------------------------------------
# .iu      : model without user_retrate AND item_retrate, n = 100.000
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.i, known.u, known.iu)
unknown <- unknown.iu
parameters <- expand.grid("size" = seq(from = 11, to = 11, by = 1),
                          "decay" = c(0.5))
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$item_retrate   <- NULL     # remove user_retrate (uncertain categories)
known.n$user_retrate   <- NULL     #
unknown.n$item_retrate <- NULL     #
unknown.n$user_retrate <- NULL     #
                                   # 
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               #

# perform repeatet cross validation 
source(file="./nnet/00-2-rep_cv.R")
cv.list.iu <- cv.list              # store result of repeated cross validation
save(cv.list, results.par, measure.list, parameters, tau_candidates, known.n, 
     file = "./data/tau-tuning-iu.RData")

save(cv.list.f, cv.list.u, cv.list.iu, cv.list.i,
     file = "./data/cv_lists-tau-tuning.RData")
