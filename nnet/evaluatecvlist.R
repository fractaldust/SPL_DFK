#cv.list evaluate 2

auc <- matrix(data = NA, nrow = m*k, ncol = nrow(parameters))
loss <- auc
sse <- auc
measure <- list()
tau <- list()
for(m in 1:length(cv.list)){
  run.m <- cv.list[[m]]
  for(v in 1:length(run.m)){        # change in tau category
    tau.v <- run.m[[v]]
    for (n in 1:dim(tau.v)[2]){     # change in parameters
      for (k in 1:dim(tau.v)[1]){   # same parameters, k-fold
        j <- k+((m-1)*dim(tau.v)[1])
        print(paste(k+((m-1)*dim(tau.v)[1]),n))
        auc[j,n]  <- tau.v[,n][[k]]$auc
        loss[j,n] <- tau.v[,n][[k]]$loss
        sse[j,n]  <- tau.v[,n][[k]]$sse
      }
    measure$auc$mean <- apply(auc, 2, mean)
    measure$auc$variance <- apply(auc, 2, sd)
    measure$loss$mean <- apply(loss, 2, mean)
    measure$loss$variance <- apply(loss, 2, sd)
    measure$sse$mean <- apply(sse, 2, mean)
    measure$sse$variance <- apply(sse, 2, sd)
    }
    print(v)
    tau[[v]] <- measure
    }
}

plot(measure$loss$mean)
