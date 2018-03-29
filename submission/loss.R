#----------------------------------------------------------------------------------
# calculates loss of tuned models (nnet, rf, xgb) for 100.000 observations
#----------------------------------------------------------------------------------
# neural network:  nnet
# random forest :  rf
# xg-boost      :  xgb
# to compare performance of tuned models
#----------------------------------------------------------------------------------


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


part.loss = function(list){
  loss = helper.cvlist.tune(list)
  x    =  1:length(loss[[1]])
  # 3x2 plot for each tau-category
  df        = data.frame(loss[[1]])
  # combine tau-categories
  # when combining the graphs from previous plot, it is important to 
  # normalize the loss of each category to 1 since the loss depends on 
  # the item_price, as well as the tau-category. 
  # higher tau-category leads naturally to a higher loss
  su = (df[,1]*(-1))/max(df[,1]*(-1)) # tau-category 1
  su = loss[[1]]*(-1)/max(loss[[1]]*(-1))
  for (i in 2:6){     
    su = su + (loss[[i]]*(-1))/max(loss[[i]]*(-1))
  }
  su = su/6 * (-1)      # normalize and change sign 
  # to compare shape to previous plots
  su2 = data.frame(su)  # change to data.frame for ggplot()
  idx = which.max(su2[[1]])
  
  val = loss[[1]][idx]
  for (i in 2:6){
    val = val + loss[[i]][idx]
    print(val)
  }
  return(val)
}

# loss of optimal settings (like in 01-tuning script) for decomposed subsets
f  = part.loss(cv.list.f)   
u  = part.loss(cv.list.u)
i  = part.loss(cv.list.i)
iu = part.loss(cv.list.iu)

# sum of all subsets to obtain loss for all 100.000 observations
k  = f + u + i + iu
k

# nnet: -1173728
# rf  : -1244312
# xgb : -1178311

# mean of 6 runs - 3fold cv. but with 3 folds full 100.000 observations are calculated