library(ggplot2)
library(gridExtra)

load(file = "./data/known-unknown-data.RData")

# table with values to create bar-plot
x        = known$item_retrate
double   = data.frame(prop.table(table(x, known$return),1))
double   = double[double$Var2==1,]
double$k = data.frame(table(x))[,2]
colnames(double) = c("category", "return", "retrate", "k")
double = double[1:6,]

# all small parts of the plot get defined
# which data gets used 
# additional lines and labels and so on
p        = ggplot(double, aes(x = category))
retrate  = geom_col(aes(y=retrate, fill = "return rate of category"), alpha=0.3)
histo    = geom_col(aes(y=k/max(double$k), fill = "size of category"), 
                    color = "black", alpha=0.1)
hline    = geom_hline(yintercept=mean(known$return), color="red", size=1)
caption  = labs(title = "variable: item_retrate",
                subtitle = "train set: n = 100.000",
                x = "category", 
                y = "return rate", 
                fill = "legende")
text     = annotate(geom = "text", 
                    x = c(1:6), 
                    y = double$k/max(double$k)-0.08, 
                    label = double$k,    
                    color = "blue", 
                    size  = 3)
text.l   = annotate(geom = "text", 
                    x = 5.2, y = 0.43, 
                    label = "avg. return rate = 0.48174", 
                    color = "red", 
                    size  = 3)
legend   = theme(legend.justification = c(1,1), 
                 legend.position      = c(1,1),
                 legend.title         = element_blank())

# combine and draw plot
plot = p + theme_bw() + retrate  + hline + histo + caption + text + text.l + legend
plot


# save plot as png
width = 1
hight = 1
b = 11 #some factor to make nice proportion of font and everything else
ggsave(filename = "./graphs/plots/item_retrate_r.png", plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='png')
plot
dev.off()

#---------------------------------------------------
# distribution of known and unknown categories
#---------------------------------------------------

# table with values to create bar-plot
x        = known$item_retrate
unk      = unknown$item_retrate
double   = data.frame(prop.table(table(x, known$return),1))
double   = double[double$Var2==1,]
double$k = data.frame(table(x))[,2]
double$u = data.frame(table(unk))[,2]
colnames(double) = c("category", "return", "retrate", "k", "u")

# all small parts of the plot get defined
# which data gets used 
# additional lines and labels and so on
p        = ggplot(double, aes(x = category))
histo    = geom_col(aes(y=k), 
                     fill = "blue", alpha=0.2)
f = c(5000, 5000, 5000, 5000, 5000, 5000, 5000)
text     = annotate(geom = "text", 
                    x = c(1:7), 
                    y = double$k +f , 
                    label = double$k,    
                    color = "blue", 
                    size  = 3)
caption  = labs(subtitle = "train set: n = 100.000",
                x = "category", 
                y = "size of category", 
                fill = "legende")

# now put everything together for first subplot
# (train set)
g1 = p + theme_bw() + histo + text + caption

# define stuff for second supblot
histo.u  = geom_col(aes(y=u), 
                     fill = "blue", alpha=0.2)
text.u   = annotate(geom = "text", 
                   x = c(1:7), 
                   y =  double$u + f/2, 
                   label = double$u,    
                   color = "blue", 
                   size  = 3)
caption.u = labs(subtitle = "test set: n = 50.000",
                x = "category", 
                y = "size of category")
# now put everything together for second subplot
# (test set)
g2 = p + theme_bw() + histo.u + text.u + caption.u

# draw plot
plot = grid.arrange(g1, g2 , nrow = 2)
plot

b = 11
ggsave(file="./graphs/plots/item_retrate_ku.png",  plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='png')
plot
dev.off()

