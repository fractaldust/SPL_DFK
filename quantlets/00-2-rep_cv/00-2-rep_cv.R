#----------------------------------------------------------------------------------
#  performs m times repeated cross validation (that is k-fold itself)
#----------------------------------------------------------------------------------
# input   : known (=train) data set
# output  : cv.list stores measure (loss)
#----------------------------------------------------------------------------------
tau_c   = list()
cv.list = list()

for (t in 1:m){
    set.seed(1234*t)
    # randomize rows of data set
    sample.idx = sample(nrow(known))
    tr  = known[sample.idx,] # randomised tr.v (same rows but random order)
    rm(sample.idx)
    for (v in 1:6){
        # call script for each tau-class
        tr.v = tr[tr$tau == v, ]
        print(paste("tau =", v, "rep = ", t))
        source(file = "./nnet/00-1-kfold_cv.R")
        tau_c[[paste("tau_c ==", v)]] = results.par
  }
  cv.list[[t]] = tau_c
}
