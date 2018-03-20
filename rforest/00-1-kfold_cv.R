#----------------------------------------------------------------------------------
# performs k-fold cross validation
#----------------------------------------------------------------------------------
# input   : tr.v (training set where tau-category == v (v in 1:6))
# output  : measure (AUC, loss, which tau) of the combined cross validation 
#----------------------------------------------------------------------------------
library(caret)
library(pROC)
library(doParallel)
library(data.table)
library(randomForest)

# k-fold cross validation
k = k              # is choosen in parent script
sample.idx = sample(nrow(tr.v))
train.rnd  = tr.v[sample.idx,] # randomised tr.v (same rows but random order)
folds = cut(1:nrow(train.rnd), breaks = k, labels = FALSE)

# introduce parallel computing 
nrOfCores = detectCores()
cl        = makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))

results.par = foreach(n = 1:nrow(parameters), .combine = cbind, 
                      .packages = c("caret", "nnet", "pROC", "data.table", "randomForest")) %:%
    # loop for cross validation
    foreach(i = 1:k, 
          .packages = c("caret","nnet", "pROC", "data.table", "randomForest")) %dopar%{
        set.seed(1234)
        # Split data into training and validation
        idx.val  = which(folds == i, arr.ind = TRUE)
        cv.train = train.rnd[-idx.val,]
        cv.train = cv.train[order(cv.train$order_item_id),]
        cv.val   = train.rnd[idx.val,]
        cv.val   = cv.val[order(cv.val$order_item_id),]
        # train forest and make prediction 
        rf       = randomForest(as.factor(return) ~. -order_item_id - tau, data = cv.train, 
                                ntree = parameters$ntree[n], mtry = parameters$mtry[n])
        yhat.val = predict(rf, newdata = cv.val, type = "prob")[,2]
            
        loss = helper.loss(tau_candidates = tau_candidates, 
                           truevals       = cv.val$return, 
                           predictedvals  = yhat.val, 
                           itemprice      = real_price$item_price[cv.val$order_item_id])
        res  = list("loss"        = max(loss), 
                    "parameters"  = data.table("ntree" = parameters$ntree[n],
                                               "mtry"  = parameters$mtry[n]))
          }

# stop parallel computing
stopCluster(cl)

rm(folds, nrOfCores, train.rnd, cl)
