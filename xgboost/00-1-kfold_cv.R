#----------------------------------------------------------------------------------
# performs k-fold cross validation
#----------------------------------------------------------------------------------
# input   : tr.v (training set where tau-category == v (v in 1:6))
# output  : measure (AUC, loss, which tau) of the combined cross validation 
#----------------------------------------------------------------------------------
library(caret)
library(pROC)
#library(doParallel)
#library(microbenchmark)
library(data.table)
library(mlr)

# k-fold cross validation
k <- k              # is choosen in parent script
sample.idx <- sample(nrow(tr.v))
train.rnd  <- tr.v[sample.idx,] # randomised tr.v (same rows but random order)
folds <- cut(1:nrow(train.rnd), breaks = k, labels = FALSE)

vec <- list()
results.par <- list()

for (n in 1:nrow(parameters)){
  vec.1 <- results.par
  for (i in 1:k){
    gc()
    set.seed(1234)
    # Split data into training and validation
    idx.val  <- which(folds == i, arr.ind = TRUE)
    cv.train <- train.rnd[-idx.val,]
    cv.train <- cv.train[order(cv.train$order_item_id),]
    cv.val   <- train.rnd[idx.val,]
    cv.val   <- cv.val[order(cv.val$order_item_id),]
    cv.train$return <- as.factor(cv.train$return)
    task <- makeClassifTask(data = cv.train, target = "return", positive = "1")
    
    
    xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", 
                               par.vals = list("verbose" = 1)) 
    
    #set tuning parameters
    par.vals <- list("nrounds"   = parameters$nrounds[n], 
                     "max_depth" = parameters$max_depth[n], 
                     "eta"       = parameters$eta[n], 
                     "gamma"     = parameters$gamma[n], 
                     "colsample_bytree" = parameters$colsample_bytree[n], 
                     "min_child_weight" = parameters$min_child_weight[n])
    
    xgb.learner <- setHyperPars(xgb.learner, par.vals = par.vals, "verbose" = 0)
    # train xgboost and make prediction 
    xgb <- mlr::train(xgb.learner, task = task)
    yhat <- predict(xgb, newdata = cv.val)
    yhat.val <- yhat[2]$data$prob.1
    
    loss <- helper.loss(tau_candidates = tau_candidates, 
                        truevals       = cv.val$return, 
                        predictedvals  = yhat.val, 
                        itemprice      = real_price$item_price[cv.val$order_item_id])
    res  <- list("loss"        = max(loss), 
                 "parameters"  = parameters[n,])
    res
    vec[[i]] <- res
  }
  results.par <- cbind(vec.1, vec)
}



# combine cross validations
# measure <- helper.evaluate(results.par, tau_candidates)

rm(sample.idx, train.rnd, folds, vec)