library(caret)

load(file = "./data/known-unknown-data.RData")
real_price <- data.frame(known$order_item_id, known$item_price) # for loss function
colnames(real_price) <- c("order_item_id", "item_price")

# decompose dataset in pure subsets
source(file="Decompose_Dataset.R")
source(file = "helperfunctions.R")

# settings for tuning and cross validation 
m <- 2 # repeated Cross Validation (same algorithm but different random split)
k <- 3 # 3-fold cross validation
# set of parameters to be used for tuning
# if used for different model, change name in 00-1-kfold_cv.R

parameters <- expand.grid("size" = seq(from = 3, to = 15, by = 2),
                          "decay" = c(0.01, 0.1, 0.5, 0.8, 1))
tau_candidates <- 0.002*125:425 # to calculate optimal loss

#----------------------------------------------------------------------------------
# .u       : full model, n ~ 25.000
#----------------------------------------------------------------------------------
known   <- known.f 
unknown <- unknown.f               # 
# additional data preparation for nnet
source(file = "./nnet/00-3-nnet_DataPrep.R")
# 
known   <- known.n                 # output of additional data preparation
unknown <- unknown.n               # 

# perform repeatet cross validation 
source(file="./nnet/AUC/00-2-rep_cv.R")
cv.list.f <- cv.list               # store result of repeated cross validation 
#save(cv.list.u, file = "./data/nnet-tuning-u.RData")


# and then do the same for the other subsets .f, .i, .u, .iu.... 