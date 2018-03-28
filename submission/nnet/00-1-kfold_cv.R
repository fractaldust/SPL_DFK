#----------------------------------------------------------------------------------
# performs k-fold cross validation
#----------------------------------------------------------------------------------
# input   : tr.v (training set where tau-category == v (v in 1:6))
# output  : measure (loss) of the combined cross validation 
#----------------------------------------------------------------------------------
library(caret)
library(nnet)
library(doParallel)
library(data.table)

# k-fold cross validation
k = k              # is choosen in parent script
sample.idx = sample(nrow(tr.v))
train.rnd  = tr.v[sample.idx,] # randomised tr.v (same rows but random order)
folds      = cut(1:nrow(train.rnd), breaks = k, labels = FALSE)

# introduce parallel computing 
nrOfCores = detectCores()
cl        = makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))

results.par = foreach(n = 1:nrow(parameters), .combine = cbind, 
                      .packages = c("caret", "nnet", "data.table")) %:%
    # loop for cross validation
    foreach(i = 1:k, 
            .packages = c("caret","nnet", "data.table")) %dopar%{
        set.seed(1234) # set seed for reproducibility 
        # Split data into training and validation
        idx.val  = which(folds == i, arr.ind = TRUE)
        cv.train = train.rnd[-idx.val,]
        cv.train = cv.train[order(cv.train$order_item_id),]
        cv.val   = train.rnd[idx.val,]
        cv.val   = cv.val[order(cv.val$order_item_id),]
        price    = real_price$item_price[cv.val$order_item_id]
        # train nnet and make prediction
        nnet     = nnet(return~. -order_item_id - tau, 
                        data  = cv.train,
                        trace = FALSE, maxit = 1000,
                        size  = parameters$size[n], 
                        decay = parameters$decay[n])
        yhat.val = predict(nnet, newdata = cv.val, type = "raw")
        loss     = helper.loss(tau_candidates = tau_candidates, 
                               truevals       = cv.val$return, 
                               predictedvals  = yhat.val, 
                               itemprice      = price)
        pars     = data.table("size"  = parameters$size[n],
                              "decay" = parameters$decay[n])
        res      = list("loss"        = max(loss), 
                        "tau"         = tau_candidates[which.max(loss)],
                        "parameters"  = pars)              
            }

# stop parallel computing
stopCluster(cl)

rm(folds, nrOfCores, train.rnd, cl)
