library(ggplot2)
library(gridExtra)
library(caret)
library(nnet)
library(pROC)
library(data.table)

calc.loss <- function(known, tau_c){
  v <- tau_c
  k = 3
  i = 1
  tr <- known
  par <- c(5, 1) #############################check best setting for subset (.u?)
  
  # tau category v 
  tr.v <- tr[tr$tau==tau_c]
  sample.idx <- sample(nrow(tr.v))
  train.rnd <- tr.v[sample.idx,] # randomised train set (same rows but random order)
  folds <- cut(1:nrow(train.rnd), breaks = k, labels = FALSE) # vector mit 111..222..333..444..555 
  
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
                 size = par[1], decay = par[2])
  yhat.val <- predict(neunet, newdata = cv.val, type = "raw")
  
  auc  <- auc(cv.val$return, as.vector(yhat.val))[[1]]
  loss <- helper.loss(tau_candidates = tau_candidates, 
                      truevals       = cv.val$return, 
                      predictedvals  = yhat.val, 
                      itemprice      = real_price$item_price[cv.val$order_item_id])
  sse  <- helper.sse(truevals        = cv.val$return, 
                     predictedvals   = yhat.val)
  res  <- list("loss.list"        = loss, 
               "auc"              = auc,
               "sse.list"         = sse, 
               "parameters"  = data.table("size"  = par[1],
                                          "decay" = par[2]))
  return(res)
}

#tau_loss graph
load(file = "./data/known-unknown-data.RData")
real_price <- data.frame(known$order_item_id, known$item_price)
colnames(real_price) <- c("order_item_id", "item_price")

source(file="Decompose_Dataset.R") 
library(caret)

#------------------------------

source(file = "helperfunctions.R")

#--------------------
# u
#--------------------
# train model WITHOUT user_retrate n = 92.409
# extension u
known <- rbind(known.f, known.u)
unknown <- unknown.u

source(file = "./nnet/00-3-nnet_DataPrep.R")
known.n$user_retrate <- NULL
unknown.n$user_retrate <- NULL

known <- known.n
tau_candidates <- 0.002*125:425

tau_1 <- calc.loss(known.n, 1)
tau_2 <- calc.loss(known.n, 2)
tau_3 <- calc.loss(known.n, 3)
tau_4 <- calc.loss(known.n, 4)
tau_5 <- calc.loss(known.n, 5)
tau_6 <- calc.loss(known.n, 6)

tau_loss <- data.frame(tau_1$loss.list, tau_2$loss.list, tau_3$loss.list, 
                       tau_4$loss.list, tau_5$loss.list, tau_6$loss.list)

tau_loss <- tau_loss/10000
tau_loss$tau_candidates <- tau_candidates

tau_sse <- data.frame(tau_1$sse.list, tau_2$sse.list, tau_3$sse.list, 
                      tau_4$sse.list, tau_5$sse.list, tau_6$sse.list)
tau_sse$tau_candidates <- 1:1000*0.001


#-------------------------------------------------------------------------
# loss plot
#-------------------------------------------------------------------------
blank.x <- theme(axis.title.x = element_blank())
caption.b <- labs(y= "loss [10.000]")
caption  <- labs(x = "tau", 
                 y = "loss [10.000]")
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                          color = "red", size  = 4,
                          label = "tau_c = 1")
g1 <- ggplot(tau_loss, aes(x=tau_candidates)) + geom_line(aes(y=tau_1.loss.list)) + caption.b + text + theme_bw() + blank.x

text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                          color = "red", size  = 4,
                          label = "tau_c = 2")
g2 <- ggplot(tau_loss, aes(x=tau_candidates)) + geom_line(aes(y=tau_2.loss.list)) + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                          color = "red", size  = 4,
                          label = "tau_c = 3")
g3 <- ggplot(tau_loss, aes(x=tau_candidates)) + geom_line(aes(y=tau_3.loss.list)) + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                          color = "red", size  = 4,
                          label = "tau_c = 4")
g4 <- ggplot(tau_loss, aes(x=tau_candidates)) + geom_line(aes(y=tau_4.loss.list)) + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                          color = "red", size  = 4,
                          label = "tau_c = 5")
g5 <- ggplot(tau_loss, aes(x=tau_candidates)) + geom_line(aes(y=tau_5.loss.list)) + caption + text + theme_bw()
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                          color = "red", size  = 4,
                          label = "tau_c = 6")
g6 <- ggplot(tau_loss, aes(x=tau_candidates)) + geom_line(aes(y=tau_6.loss.list)) + caption + text + theme_bw()

plot <- grid.arrange(g1, g2 , g3, g4, g5, g6, nrow = 3, ncol = 2)

width <- 2
hight <- 1 
b <- 11
ggsave(file="./graphs/plots/find_tau.png",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='png')
plot
dev.off()

#------------------------------------------------------------------------------------------
# sse plot
#-----------------------------------------------------------------------------------------
blank.x <- theme(axis.title.x = element_blank())
caption.b <- labs(y= "sse [10.000]")
caption  <- labs(x = "tau", 
                 y = "sse [10.000]")
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                     color = "red", size  = 4,
                     label = "tau_c = 1")
s1 <- ggplot(tau_sse, aes(x=tau_candidates)) + geom_line(aes(y=tau_1.sse.list)) + caption.b + text + theme_bw() + blank.x

text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                     color = "red", size  = 4,
                     label = "tau_c = 2")
s2 <- ggplot(tau_sse, aes(x=tau_candidates)) + geom_line(aes(y=tau_2.sse.list)) + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                     color = "red", size  = 4,
                     label = "tau_c = 3")
s3 <- ggplot(tau_sse, aes(x=tau_candidates)) + geom_line(aes(y=tau_3.sse.list)) + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                     color = "red", size  = 4,
                     label = "tau_c = 4")
s4 <- ggplot(tau_sse, aes(x=tau_candidates)) + geom_line(aes(y=tau_4.sse.list)) + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                     color = "red", size  = 4,
                     label = "tau_c = 5")
s5 <- ggplot(tau_sse, aes(x=tau_candidates)) + geom_line(aes(y=tau_5.sse.list)) + caption + text + theme_bw()
text     <- annotate(geom = "text", x=-Inf, y=Inf,hjust = 0, vjust = 1, 
                     color = "red", size  = 4,
                     label = "tau_c = 6")
s6 <- ggplot(tau_sse, aes(x=tau_candidates)) + geom_line(aes(y=tau_6.sse.list)) + caption + text + theme_bw()

plot.s <- grid.arrange(s1, s2 , s3, s4, s5, s6, nrow = 3, ncol = 2)

width <- 2
hight <- 1 
b <- 11
ggsave(file="./graphs/plots/find_tau_sse.png",  plot.s,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='png')
plot
dev.off()
