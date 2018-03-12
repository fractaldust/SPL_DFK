#----------------------------------------------------------------------------------
#  performs t times repeated cross validation (that is k-fold itself)
#----------------------------------------------------------------------------------
# input   : known (=train) and unknown (=test) dataset
# output  : cv.list, measure.list : store measures (AUC, tau (for best split), loss)
#----------------------------------------------------------------------------------
tau_c   <- list()
cv.list <- list()

for (t in 1:m){
  set.seed(1234*t)
  # Splitting the data into a test and a training set 
  idx.train <- caret::createDataPartition(y = known$return, p = 0.8, list = FALSE)
  # Actual data splitting
  tr <- known[idx.train,  ] # training set
  ts <- known[-idx.train, ] # test set
  for (v in 1:6){
    # call script for each tau-class
    tr.v <- tr[tr$tau==v,]
    print(paste("tau =", v, "rep = ", t))
    source(file="./xgboost/00-1-kfold_cv.R")
    tau_c[[paste("tau_c ==", v)]] <- results.par
  }
  cv.list[[t]] <- tau_c
}

rm(tau_c)