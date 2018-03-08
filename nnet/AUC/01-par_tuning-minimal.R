library(caret)
library(hmeasure)

load(file = "./data/known-unknown-data.RData")

# decompose dataset in pure subsets
source(file="Decompose_Dataset.R")
source(file = "helperfunctions.R")

# settings for tuning and cross validation 
m <- 2 # repeated Cross Validation (same algorithm but different random split)
k <- 3 # 3-fold cross validation
# set of parameters to be used for tuning
# if used for different model, change name in 00-1-kfold_cv.R
parameters <- expand.grid("size" = seq(from = 3, to = 5, by = 2),
                          "decay" = c(0.01, 1))

#----------------------------------------------------------------------------------
# .u       : model without user_retrate, n = 92.409
#----------------------------------------------------------------------------------
known   <- rbind(known.f, known.u) # can also use .f for training, variables are pure
unknown <- unknown.u               # 
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
#save(cv.list.u, file = "./data/nnet-tuning-u.RData")


# and then do the same for the other subsets .f, .i, .u, .iu.... 