# bring cv.list in better format
# to combine result of the m-times repeated k-fold cross validations
df.cv <- do.call(rbind.data.frame, cv.list)
df <- data.frame()
for (i in 1:dim(df.cv)[1]){
  for (j in 1:dim(df.cv)[2]){
    df[i,j] <- df.cv[,j][[i]]$auc
  }
}
su <- data.table("mean" = as.double(1:dim(df)[2]), 
                 "variance" = as.double(1:dim(df)[2]))
                 
# calculate mean and variance for each setting (parameters)
for(i in 1:dim(df)[2]){
    a  <- unlist(df[,i])
    su[i, "mean"]     <- mean(a)
    su[i, "variance"] <- var(a)
    }
su

y <- su$mean

#plot tuning process
plot(y)
