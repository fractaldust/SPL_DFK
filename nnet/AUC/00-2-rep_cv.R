#----------------------------------------------------------------------------------
#  performs t times repeated cross validation (that is k-fold itself)
#----------------------------------------------------------------------------------
# input   : known (=train) and unknown (=test) dataset
# output  : cv.list, measure.list : store measures (AUC, tau (for best split), loss)
#----------------------------------------------------------------------------------
measure.list <- list()
cv.list      <- list()

for (t in 1:m){
  set.seed(1234*t)
  print(t)
  # Splitting the data into a test and a training set 
  idx.train <- caret::createDataPartition(y = known$return, p = 0.8, list = FALSE)
  # Actual data splitting
  tr <- known[idx.train,  ] # training set
  ts <- known[-idx.train, ] # test set
  
  source(file="./nnet/AUC/00-1-kfold_cv.R")
  cv.list[[t]] <- results.par
}
