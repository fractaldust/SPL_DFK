library(caret)
library(hmeasure)

load(file = "./data/known-unknown-data.RData")

# decompose dataset in pure subsets
source(file="Decompose_Dataset.R")
source(file = "helperfunctions.R")

# settings for tuning and cross validation 
m <- 6 # repeated Cross Validation (same algorithm but different random split)
k <- 3 # 3-fold cross validation
# set of parameters to be used for tuning
# if used for different model, change name in 00-1-kfold_cv.R
parameters <- expand.grid("size" = seq(from = 3, to = 5, by = 2),
                          "decay" = c(0.1, 1))

known   <- known.f
unknown <- unknown.f
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R") 
known   <- known.n       # output of additional data preparation
unknown <- unknown.n     # 

# perform repeatet cross validation 
source(file="./nnet/00-2-rep_cv.R")
cv.list.f <- cv.list     # store result of repeated cross validation 

# and then do the same for the other subsets .i, .u, .iu.... 