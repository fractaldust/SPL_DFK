split <- function(newcol, concol, c, tag=c[-1]){
  # assignes values to intervals, names the intervals. 
  # newcol : name of new column e.g. train$age-category
  #        : has to be created beforehands 
  # concol : name of controle column e.g. train$age
  # c      : vector of interval borders, e.g. age.s <- c(18,25,35,45,55,65) 
  #        : to cover everything set c from minimum to maximum
  # tag    : name tag of intervals 
  #        : default: upper bound of intervals (from c)
  # example: train$user_age <- as.factor(split(train$age-category, train$age, c))
  
  c[length(c)] <- 1.01*c[length(c)] # change last upper bound to make 
                                    # condition in last round work
  
  for (i in 2:length(c)){
    newcol[concol < c[i] & concol >= c[i-1]] <- tag[i-1]
  }
  return(newcol)
}


# function calculates loss
helper.calcloss <- function(truevals, predictedvals, itemprice){
  # function that given a string of true values and one of the predicted ones, gives us the loss value
  # truevals      : string of known values e.g. known$return
  # predictedvals : column of predictions to be evalued e.g. rf.1$predicted (must have same length as truevals)
  # lossone       : loss value associated with predicting 0 and have 1 as true value 
  # losstwo       : loss value associated with predicting 1 and have 0 as true value 
  lossone <- 2.5 * ((-3) + (-0.1)*itemprice)
  losstwo <- (-0.5) * itemprice 
  temploss <- 0
  p <- predictedvals - truevals
  for (i in 1:length(truevals)) {
    if      (p[i]==(-1)){temploss <- temploss + lossone[i]}
    else if (p[i]==  1 ){temploss <- temploss + losstwo[i]}
  }
  return(temploss)
}

helper.loss <- function(tau_candidates, truevals, predictedvals, itemprice){
  loss <- 1:length(tau_candidates)
  for (s in loss) {
    # translate prob.prediction to 1/0 prediction due to tau_candidate
    cv_yhat_dt_zerone          <- 1:length(truevals)
    cv_yhat_dt_zerone[predictedvals >= tau_candidates[s]] <- 1
    cv_yhat_dt_zerone[predictedvals <  tau_candidates[s]] <- 0
    # calculate loss
    loss[s] <- helper.calcloss(truevals      = truevals,
                               predictedvals = cv_yhat_dt_zerone,
                               itemprice     = itemprice)
  }
  return(loss)
}

helper.sse  <- function(truevals, predictedvals){
  thresh <- 1:1000*0.001
  sse <- 1:length(thresh)
  for (s in 1:length(thresh)) {
    # translate prob.prediction to 1/0 prediction due to tau_candidate
    cv_yhat_dt_zerone          <- 1:length(truevals)
    cv_yhat_dt_zerone[predictedvals >= thresh[s]] <- 1
    cv_yhat_dt_zerone[predictedvals <  thresh[s]] <- 0
    err <- (truevals - cv_yhat_dt_zerone)**2
    # calculate loss
    sse[s] <- sum(err)
  }
  return(sse)
}

# for nnet tuning
helper.evaluate <- function(results.par, tau_candidates){
  # input:
  # results.par[[k]][[i]] 
  #              k: k-th iteration (1-5 first row (5 cross validation), 20 rows (size*decay))
  #                   i = 1: auc (numeric)
  #                   i = 2: tau_loss (vector of tau_measure according to threshold tau)
  #                   i = 3: yhat.val (vector)
  # tau_candidates : vector of taus that are tried as threshold
  # return:
  # measure        : optimal values 
  #                : "auc"    : maximum AUC
  #                : "tau"    : this tau makes optimal split
  #                : "loss"   : minimal loss 
  auc      <- 1:length(results.par) # to store auc for every cv + tuning iteration
  loss     <- 1:length(results.par) # to store max(tau_measure)
  
  for (i in 1:length(results.par)) {
    auc[i]      <- results.par[[i]]$auc
    loss[i]     <- results.par[[i]]$loss
  }
  # combine the cross validations again (100 = 20 * 5 --> 20)
  measure <- list()
  for (i in 1:nrow(parameters)){
    auc.i  <- auc[(i*k-k+1):(i*k)]
    loss.i <- loss[(i*k-k+1):(i*k)]
    measure[["auc"]][[i]]  <- mean(auc.i)
    measure[["loss"]][[i]] <- mean(loss.i)
    measure[["pars"]][[i]] <- results.par[[(i*k-k+1)]]$parameters
  }
  return(measure)
}

# for data processing for nnet
# assignes numbers to factors of categorical variables
helper.fac2num <- function(){
  load(file = "./data/known-unknown-data.RData")
  
  colnames(known)[2] <- "return"
  
  # full dataset for item_retrate and user_retrate (since target variable was already used anyways)
  # smaller dataset for delivery_time and price_comp (to avoid overfitting)
  
  known$return  <- as.factor(known$return)
  set.seed(1234)
  split.idx.woe <- createDataPartition(y = known$return, p = 0.80, list = FALSE)
  split.woe     <- known[-split.idx.woe,]
  
  woe.object.full  <- woe(return ~ ., data = known, zeroadj = 0.5)
  woe.object.split <- woe(return ~ ., data = split.woe, zeroadj = 0.5)
  
  fac2num <- list()
  fac2num[["delivery_time"]] <- woe.object.split$woe$delivery_time 
  fac2num[["price_comp"]]    <- woe.object.split$woe$price_comp
  fac2num[["item_retrate"]]  <- woe.object.full$woe$item_retrate
  fac2num[["user_retrate"]]  <- woe.object.full$woe$user_retrate
  fac2num[["split.idx"]]     <- split.idx.woe
  
  return(fac2num)
}
