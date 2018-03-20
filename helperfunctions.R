split = function(newcol, concol, c, tag=c[-1]){
    # assignes values to intervals, names the intervals. 
    # newcol : name of new column e.g. train$age-category
    #        : has to be created beforehands 
    # concol : name of controle column e.g. train$age
    # c      : vector of interval borders, e.g. age.s = c(18,25,35,45,55,65) 
    #        : to cover everything set c from minimum to maximum
    # tag    : name tag of intervals 
    #        : default: upper bound of intervals (from c)
    # example: train$user_age = as.factor(split(train$age-category, train$age, c))
  
    c[length(c)] = 1.01*c[length(c)] # change last upper bound to make 
                                    # condition in last round work
  
    for (i in 2:length(c)){
        newcol[concol < c[i] & concol >= c[i-1]] = tag[i-1]
    }
    return(newcol)
}

helper.calcloss = function(truevals, predictedvals, itemprice){
    # function that given a string of true values and one of the predicted ones, gives us the loss value
    # truevals      : string of known values e.g. known$return
    # predictedvals : column of predictions to be evalued e.g. rf.1$predicted (must have same length as truevals)
    # lossone       : loss value associated with predicting 0 and have 1 as true value 
    # losstwo       : loss value associated with predicting 1 and have 0 as true value 
    lossone = 2.5 * ((-3) + (-0.1)*itemprice)
    losstwo = (-0.5) * itemprice 
    temploss = 0
    p = predictedvals - truevals
    for (i in 1:length(truevals)) {
        if      (p[i]==(-1)){temploss = temploss + lossone[i]}
        else if (p[i]==  1 ){temploss = temploss + losstwo[i]}
    }
    return(temploss)
}

helper.loss = function(tau_candidates, truevals, predictedvals, itemprice){
    loss = 1:length(tau_candidates)
    for (s in loss) {
    # translate prob.prediction to 1/0 prediction due to tau_candidate
        cv_yhat_dt_zerone          = 1:length(truevals)
        cv_yhat_dt_zerone[predictedvals >= tau_candidates[s]] = 1
        cv_yhat_dt_zerone[predictedvals <  tau_candidates[s]] = 0
        # calculate loss
        loss[s] = helper.calcloss(truevals      = truevals,
                                  predictedvals = cv_yhat_dt_zerone,
                                  itemprice     = itemprice)
    }
    return(loss)
}

# # for nnet tuning
# helper.evaluate = function(results.par, tau_candidates){
#   auc      = 1:length(results.par) # to store auc for every cv + tuning iteration
#   loss     = 1:length(results.par) # to store max(tau_measure)
#   
#   for (i in 1:length(results.par)) {
#     auc[i]      = results.par[[i]]$auc
#     loss[i]     = results.par[[i]]$loss
#   }
#   # combine the cross validations again (100 = 20 * 5 --> 20)
#   measure = list()
#   for (i in 1:nrow(parameters)){
#     auc.i  = auc[(i*k-k+1):(i*k)]
#     loss.i = loss[(i*k-k+1):(i*k)]
#     measure[["auc"]][[i]]  = mean(auc.i)
#     measure[["loss"]][[i]] = mean(loss.i)
#     measure[["pars"]][[i]] = results.par[[(i*k-k+1)]]$parameters
#   }
#   return(measure)
# }

# for data processing for nnet
# assignes numbers to factors of categorical variables
helper.fac2num = function(){
    load(file = "./data/known-unknown-data.RData")
    colnames(known)[2] = "return"
    
    # full dataset for item_retrate and user_retrate (since target variable was already used anyways)
    # smaller dataset for delivery_time and price_comp (to avoid overfitting)
    known$return  = as.factor(known$return)
    set.seed(1234)
    split.idx.woe = createDataPartition(y = known$return, p = 0.80, list = FALSE)
    split.woe     = known[-split.idx.woe,]
  
    woe.object.full  = woe(return ~ ., data = known, zeroadj = 0.5)
    woe.object.split = woe(return ~ ., data = split.woe, zeroadj = 0.5)
  
    fac2num = list()
    fac2num[["delivery_time"]] = woe.object.split$woe$delivery_time 
    fac2num[["price_comp"]]    = woe.object.split$woe$price_comp
    fac2num[["item_retrate"]]  = woe.object.full$woe$item_retrate
    fac2num[["user_retrate"]]  = woe.object.full$woe$user_retrate
    fac2num[["split.idx"]]     = split.idx.woe
  
    return(fac2num)
}

helper.cvlist <- function(cv.list){
    # extracts mean and standard deviation of cv.list (m*k repetitions)
    # saves results in list tau
    k       = length(cv.list[[1]][[1]][,1])       # dimension of k-fold cross validation
    loss    = matrix(data = NA, nrow = length(cv.list)*k, ncol = nrow(parameters))
    measure = list()
    tau     = list()
    for(m in 1:length(cv.list)){              # m times repeated cv 
        run.m = cv.list[[m]]
        for(v in 1:length(run.m)){            # tau-category 1:6
            tau.v = run.m[[v]]
            for (n in 1:dim(tau.v)[2]){       # change in parameters
                for (k in 1:dim(tau.v)[1]){   # same parameters, k-fold
                      j = k+((m-1)*dim(tau.v)[1])
                      loss[j,n] = tau.v[,n][[k]]$loss
                }
                measure$loss$mean  = apply(loss, 2, mean)
                measure$loss$sd    = apply(loss, 2, sd)
            }
            tau[[v]] = measure
        }
    }
    return(tau)
}
