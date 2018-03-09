#----------------------------------------------------------------------------------
# performs k-fold cross validation
#----------------------------------------------------------------------------------
# input   : tr.v (training set where tau-category == v (v in 1:6))
# output  : measure (AUC, loss, which tau) of the combined cross validation 
#----------------------------------------------------------------------------------



# THE ONE WITH ONLY LOSS INSTEAD OF AUC

library(caret)
library(nnet)
library(pROC)
library(doParallel)
library(microbenchmark)
library(data.table)

# k-fold cross validation
k <- k              # is choosen in parent script
sample.idx <- sample(nrow(tr.v))
train.rnd  <- tr.v[sample.idx,] # randomised tr.v (same rows but random order)
folds <- cut(1:nrow(train.rnd), breaks = k, labels = FALSE)

# introduce parallel computing 
nrOfCores <- detectCores()
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


timing.par <- system.time(
  # loop for different nnet settings
  results.par <- foreach(n = 1:nrow(parameters), .combine = cbind, 
                         .packages = c("caret", "nnet", "pROC", "data.table")) %:%
    # loop for cross validation
    foreach(i = 1:k, 
            .packages = c("caret","nnet", "pROC", "data.table")) %dopar%{
              gc()
              set.seed(1234)
              # Split data into training and validation
              idx.val  <- which(folds == i, arr.ind = TRUE)
              cv.train <- train.rnd[-idx.val,]
              cv.train <- cv.train[order(cv.train$order_item_id),]
              cv.val   <- train.rnd[idx.val,]
              cv.val   <- cv.val[order(cv.val$order_item_id),]
              # train nnet and make prediction
              neunet <- nnet(return~. -order_item_id - tau, data = cv.train,
                                trace = FALSE, maxit = 1000,
                                size = parameters$size[n], decay = parameters$decay[n])
              yhat.val <- predict(neunet, newdata = cv.val, type = "raw")

              auc  <- auc(cv.val$return, as.vector(yhat.val))[[1]]
              loss <- helper.loss(tau_candidates = tau_candidates, 
                                  truevals       = cv.val$return, 
                                  predictedvals  = yhat.val, 
                                  itemprice      = real_price$item_price[cv.val$order_item_id])
              sse  <- helper.sse(truevals        = cv.val$return, 
                                 predictedvals   = yhat.val)
              res  <- list("loss"        = max(loss), 
                           "auc"         = auc,
                           "sse"         = min(sse), 
                           "parameters"  = data.table("size"  = parameters$size[n],
                                                      "decay" = parameters$decay[n]))
              res
            }
)
# stop parallel computing
stopCluster(cl)

# combine cross validations
# measure <- helper.evaluate(results.par, tau_candidates)

rm(folds, nrOfCores, train.rnd, timing.par, cl)