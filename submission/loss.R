source(file = "helperfunctions.R")

#-----------------------------
# nnet
load(file  = "./data/cv_lists-nnet-tuning.RData")
parameters = expand.grid("size"  = seq(from = 3, to = 15, by = 2),
                         "decay" = c(0.01, 0.1, 0.5, 0.8, 1))

#-----------------------------
# rf
load(file = "./data/cv_lists-rf-tuning.RData")
parameters <- expand.grid("ntree" = c(200, 300, 400, 500, 600, 700),
                          "mtry"  = c(2, 3, 4, 5))

#------------------------------
# xgb
load(file = "./data/cv_lists-xgb-tuning.RData")
parameters <- expand.grid("nrounds" = c(50, 100, 150),
                          "max_depth" = c(2, 4), 
                          "eta" = c(0.01, 0.1, 0.15),
                          "gamma" = 0,
                          "colsample_bytree" = 0.8, 
                          "min_child_weight" = 1)


part.loss <- function(list){
  tau <- helper.cvlist(list)
  x = 1:length(tau[[1]]$loss$mean)
  df        = data.frame(tau)
  su = (df[,1]*(-1))/max(df[,1]*(-1)) # tau-category 1
  for (i in 2:6){                     # tau-cateogries 2:6
    i  = (i*2)-1
    su = su + (df[,i]*(-1))/max(df[,i]*(-1))
  }
  su = su/6 * (-1)      # normalize and change sign 
  # to compare shape to previous plots
  su2 = data.frame(su)  # change to data.frame for ggplot()
  
  idx <- which.max(su2[[1]])  # best settings for nnet
  
  loss = df[idx,1]
  for (i in 2:6){
    l = loss
    k = (i*2)-1
    loss = l + df[idx,k] 
  }
  return(loss)
}

f <- part.loss(cv.list.f)

u <- part.loss(cv.list.u)

i <- part.loss(cv.list.i)

iu <- part.loss(cv.list.iu)

k <- f+u+i+iu
k
k * 25000/100000

# nnet 100: 1173728
# rf   100: 1244312
# xgb  100: 1178311

# mean of 6 runs - 3fold cv. but with 3 folds full 100.000 observations are calculated