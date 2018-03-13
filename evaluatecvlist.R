k       = 3  # dimension of k-fold cross validation
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
        measure$loss$mean     = apply(loss, 2, mean)
        measure$loss$variance = apply(loss, 2, sd)
        }
    tau[[v]] = measure
    }
}
plot(tau[[2]]$loss$mean)