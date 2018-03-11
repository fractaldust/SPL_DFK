#----------------------------------------------------------------------------------
# parameter tuning and cross validation
#----------------------------------------------------------------------------------
# runs m repeated (different seed) k-fold cross validations for different 
# tuning parameters
# extract best settings by maximising negative loss function
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

# Set tuning parameters
parameters <- expand.grid("nrounds" = c(20, 100, 200),
                         "max_depth" = c(2, 4), 
                         "eta" = c(0.01, 0.05, 0.1, 0.15),
                         "gamma" = 0,
                         "colsample_bytree" = 0.8, 
                         "min_child_weight" = 1)

tau_candidates <- 0.002*125:425 # to calculate optimal loss

#----------------------------------------------------------------------------------
# .f       : full model, n = 25.266
#----------------------------------------------------------------------------------
known   <- known.f
unknown <- unknown.f
# additional data preparation for xgboost
source(file = "./xgboost/00-3-xgboost_DataPrep.R") 
known   <- known.n       # output of additional data preparation
unknown <- unknown.n     # 

known   <- data.frame(known)
unknown <- data.frame(unknown)
# perform repeatet cross validation 
source(file="./xgboost/00-2-rep_cv.R")
cv.list.f <- cv.list     # store result of repeated cross validation 

#----------------------------------------------------------------------------------
# .u       : model without user_retrate, n = 92.409
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown <- unknown.u               #
# additional data preparation for xgboost
source(file = "./xgboost/00-3-xgboost_DataPrep.R")
known.n$user_retrate   <- NULL     # remove user_retrate (uncertain categories)
unknown.n$user_retrate <- NULL     #
#
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               #

known   <- data.frame(known)
unknown <- data.frame(unknown)
# perform repeatet cross validation
source(file="./xgboost/00-2-rep_cv.R")
cv.list.u <- cv.list               # store result of repeated cross validation

#----------------------------------------------------------------------------------
# .i       : model without user_retrate, n = 27.123
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.i) # can also use .f for training, variables are pure
unknown <- unknown.i               #
# additional data preparation for xgboost
source(file = "./xgboost/00-3-xgboost_DataPrep.R")
known.n$item_retrate   <- NULL     # remove user_retrate (uncertain categories)
unknown.n$item_retrate <- NULL     #
#
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               #

known   <- data.frame(known)
unknown <- data.frame(unknown)
# perform repeatet cross validation
source(file="./xgboost/00-2-rep_cv.R")
cv.list.i <- cv.list               # store result of repeated cross validation

#----------------------------------------------------------------------------------
# .iu      : model without user_retrate AND item_retrate, n = 100.000
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.i, known.u, known.iu)
unknown <- unknown.i
# additional data preparation for xgboost
source(file = "./xgboost/00-3-xgboost_DataPrep.R")
known.n$item_retrate   <- NULL     # remove user_retrate (uncertain categories)
known.n$user_retrate   <- NULL     #
unknown.n$item_retrate <- NULL     #
unknown.n$user_retrate <- NULL     #
#
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               #

known   <- data.frame(known)
unknown <- data.frame(unknown)
# perform repeatet cross validation
source(file="./xgboost/00-2-rep_cv.R")
cv.list.iu <- cv.list              # store result of repeated cross validation

save(cv.list.f, cv.list.u, cv.list.iu, cv.list.i,
     file = "./data/cv_lists-xgb-tuning.RData")
