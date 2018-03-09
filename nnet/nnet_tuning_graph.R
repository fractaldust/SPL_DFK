# plot tuning performance
library(ggplot2)
library(gridExtra)
load(file = "./data/nnet-tuning-u.RData")
k <- dim(nnet.sizes)[1]

# loss
df <- data.frame(measure.list)/10000
blank.x <- theme(axis.title.x = element_blank())
caption.b <- labs(y= "loss [10.000]")
caption  <- labs(x = "parameter index", 
                 y = "loss [10.000]")
text     <- annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 1")
g1 <- ggplot(df, aes(x=1:k, y=tau_c....1.loss)) + geom_point()  + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 2")
g2 <- ggplot(df, aes(x=1:k, y=tau_c....2.loss)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 3")
g3 <- ggplot(df, aes(x=1:k, y=tau_c....3.loss)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 4")
g4 <- ggplot(df, aes(x=1:k, y=tau_c....4.loss)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     <- annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 5")
g5 <- ggplot(df, aes(x=1:k, y=tau_c....5.loss)) + geom_point() + caption + text + theme_bw()
text     <- annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 6")
g6 <- ggplot(df, aes(x=1:k, y=tau_c....6.loss)) + geom_point() + caption + text + theme_bw()

plot <- grid.arrange(g1, g2 , g3, g4, g5, g6, nrow = 3, ncol = 2)
plot

width <- 2
hight <- 1
b <- 11
ggsave(file="./graphs/plots/nnet_tuning.png",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='png')
plot
dev.off()

#-----------------
# 
# # auc
# df <- data.frame(measure.list)
# g1 <- ggplot(df, aes(x=1:k, y=tau_c....1.auc)) + geom_point() 
# g2 <- ggplot(df, aes(x=1:k, y=tau_c....2.auc)) + geom_point() 
# g3 <- ggplot(df, aes(x=1:k, y=tau_c....3.auc)) + geom_point() 
# g4 <- ggplot(df, aes(x=1:k, y=tau_c....4.auc)) + geom_point() 
# g5 <- ggplot(df, aes(x=1:k, y=tau_c....5.auc)) + geom_point() 
# g6 <- ggplot(df, aes(x=1:k, y=tau_c....6.auc)) + geom_point() 
# 
# plot <- grid.arrange(g1, g2 , g3, g4, g5, g6, nrow = 3, ncol = 2)
# plot
# 
# # auc
# su <- df[,1]/max(df[,1])
# su
# for (i in 2:6){
#   i  <- (i*3)-2
#   su <- su + df[,i]/max(df[,i])
#   print(su)
# }
# su1 <- su/6
# plot(su1)

# loss
su
su <- (df[,3]*(-1))/max(df[,3]*(-1))
for (i in 2:6){
  i  <- (i*3)
  su <- su + (df[,i]*(-1))/max(df[,i]*(-1))
}
su2 <- su/6 * (-1)
su2 <- data.frame(su2)
caption  <- labs(x = "parameter index", 
                 y = "normed loss")
plot <- ggplot(su2, aes(x=1:k, y=su2)) + geom_point() + caption + theme_bw()

width <- 1
hight <- 1/2
b <- 11
ggsave(file="./graphs/plots/normed_sum.png",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='png')
plot
dev.off()

plot(su2)
nnet.sizes[which.max(su2),]
# 
# plot(su2 + su1)
# which.max(su2[15:35] + su1[15:35])
# plot(su1,type="o",col="red")
# par(new=TRUE)
# plot(su2,type="o",col="green")

