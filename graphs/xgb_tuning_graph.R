# plot tuning performance
library(ggplot2)
library(gridExtra)

source(file = "helperfunctions.R")
load(file = "./data/cv_lists-xgb-tuning.RData")
parameters = expand.grid("nrounds" = c(50, 100, 150),
                          "max_depth" = c(2, 4), 
                          "eta" = c(0.01, 0.1, 0.15),
                          "gamma" = 0,
                          "colsample_bytree" = 0.8, 
                          "min_child_weight" = 1)
# extract results from tuning with helperfunction
tau = helper.cvlist(cv.list.u)
x   = 1:length(tau[[1]]$loss$mean)

# 3x2 plot for each tau-category
df        = data.frame(tau)/10000
blank.x   = theme(axis.title.x = element_blank())
caption.b = labs(y = "loss [10.000]")    # only y-axis
caption   = labs(x = "parameter index",  # x- and y-axis
                 y = "loss [10.000]")
# text inside plot (for tau-category 1)
text      = annotate(geom  = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 1")
# sub-plot for tau-category 1
g1 = ggplot(df, aes(x=x, y=loss.mean)) + geom_point()  + caption.b + text + theme_bw() + blank.x

# sub-plots for tau-category 2:6
text     = annotate(geom  = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                    color = "red", size  = 4,
                    label = "tau_c = 2")
g2 = ggplot(df, aes(x=x, y=loss.mean.1)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                    color = "red", size  = 4,
                    label = "tau_c = 3")
g3 = ggplot(df, aes(x=x, y=loss.mean.2)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                    color = "red", size  = 4,
                    label = "tau_c = 4")
g4 = ggplot(df, aes(x=x, y=loss.mean.3)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                    color = "red", size  = 4,
                    label = "tau_c = 5")
g5 = ggplot(df, aes(x=x, y=loss.mean.4)) + geom_point() + caption + text + theme_bw()
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                    color = "red", size  = 4,
                    label = "tau_c = 6")
g6 = ggplot(df, aes(x=x, y=loss.mean.5)) + geom_point() + caption + text + theme_bw()

# combine plots
plot = grid.arrange(g1, g2 , g3, g4, g5, g6, nrow = 3, ncol = 2)

# save plot as pdf
width = 2
hight = 1
b = 11
ggsave(file="./graphs/plots/xgb_tuning.pdf",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='pdf')
plot
dev.off()

# loss
su = (df[,1]*(-1))/max(df[,1]*(-1))
for (i in 2:6){
  i  = (i*2)-1
  print(i)
  su = su + (df[,i]*(-1))/max(df[,i]*(-1))
}
su2 = su/6 * (-1)
su2 = data.frame(su2)
caption  = labs(x = "parameter index", 
                 y = "normed loss")
plot = ggplot(su2, aes(x=x, y=su2)) + geom_point() + caption + theme_bw()
plot

width = 1
hight = 1/2
b     = 11
ggsave(file="./graphs/plots/xgb-normed_sum.pdf",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='pdf')
plot
dev.off()

parameters[which.max(su2[[1]]),]
df[which.max(su2[[1]]),]
