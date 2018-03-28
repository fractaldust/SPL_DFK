# plot tuning performance
library(ggplot2)
library(gridExtra)

source(file = "helperfunctions.R")
load(file  = "./data/cv_lists-nnet-tuning.RData")
parameters = expand.grid("size"  = seq(from = 3, to = 15, by = 2),
                         "decay" = c(0.01, 0.1, 0.5, 0.8, 1))

# extract results from tuning with helperfunction
loss = helper.cvlist.tune(cv.list.u)
x    =  1:length(loss[[1]])

# 3x2 plot for each tau-category
df        = data.frame(loss[[1]])/10000
blank.x   = theme(axis.title.x = element_blank())
caption.b = labs(y = "loss [10.000]")    # only y-axis
caption   = labs(x = "parameter index",  # x- and y-axis
                 y = "loss [10.000]")
# text inside plot (for tau-category 1)
text      = annotate(geom  = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 1")
# sub-plot for tau-category 1
g1 = ggplot(df, aes(x=x, y=loss[[1]]/10000)) + geom_point()  + caption.b + text + theme_bw() + blank.x

# sub-plots for tau-category 2:6
text     = annotate(geom  = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                    color = "red", size  = 4,
                    label = "tau_c = 2")
g2 = ggplot(df, aes(x=x, y=loss[[2]]/10000)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 3")
g3 = ggplot(df, aes(x=x, y=loss[[3]]/10000)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 4")
g4 = ggplot(df, aes(x=x, y=loss[[4]]/10000)) + geom_point() + caption.b + text + theme_bw() + blank.x
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 5")
g5 = ggplot(df, aes(x=x, y=loss[[5]]/10000)) + geom_point() + caption + text + theme_bw()
text     = annotate(geom = "text", x=Inf, y=-Inf,hjust = 1, vjust = 0, 
                     color = "red", size  = 4,
                     label = "tau_c = 6")
g6 = ggplot(df, aes(x=x, y=loss[[6]]/10000)) + geom_point() + caption + text + theme_bw()

# combine plots
plot = grid.arrange(g1, g2 , g3, g4, g5, g6, nrow = 3, ncol = 2)

# save plot as pdf
width = 2
hight = 1
b     = 11
ggsave(file="./graphs/plots/nnet_tuning.pdf",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='pdf')
dev.off()

# combine tau-categories
# when combining the graphs from previous plot, it is important to 
# normalize the loss of each category to 1 since the loss depends on 
# the item_price, as well as the tau-category. 
# higher tau-category leads naturally to a higher loss
su = (df[,1]*(-1))/max(df[,1]*(-1)) # tau-category 1
su = tau[[1]]*(-1)/max(tau[[1]]*(-1))
for (i in 2:6){     
    print(i)
    su = su + (tau[[i]]*(-1))/max(tau[[i]]*(-1))
}
su = su/6 * (-1)      # normalize and change sign 
                      # to compare shape to previous plots
su2 = data.frame(su)  # change to data.frame for ggplot()
caption  = labs(x = "parameter index", 
                y = "normed loss")
plot = ggplot(su2, aes(x=x, y=su2)) + geom_point() + caption + theme_bw()

width = 1
hight = 1/2
b     = 11
ggsave(file="./graphs/plots/nnet-normed_sum.pdf",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='pdf')
plot
dev.off()

parameters[which.max(su2[[1]]),]  # best settings for nnet
df[which.max(su2[[1]]),]          # results for best settings
