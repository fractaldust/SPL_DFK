library(ggplot2)
library(gridExtra)

load(file = "./data/known-unknown-data.RData")

# table with values to create bar-plot
x        = known$item_type
double   = data.frame(prop.table(table(x, known$return),1))
double   = double[double$Var2==1,]
double$k = data.frame(table(x))[,2]
colnames(double) = c("category", "return", "retrate", "k")

# all small parts of the plot get defined
# which data gets used 
# additional lines and labels and so on
p        = ggplot(double, aes(x = category))
retrate  = geom_col(aes(y = retrate, fill = "return rate of category"), 
                     alpha=0.3)
histo    = geom_col(aes(y=k/max(double$k), fill = "size of category"), 
                     color = "black", 
                     alpha=0.1)
line     = geom_hline(yintercept=mean(known$return), color="red", size=1)
text.l   = annotate(geom = "text", 
                     x = 3.4, y = 0.43, 
                     label = "avg. return rate = 0.48174", 
                     color = "red", 
                     size  = 3)
caption  = labs(title = "variable: item_type",
                 subtitle = "train set: n = 100.000",
                 x     = "category", 
                 y     = "return rate", 
                 fill  = "legende")
f = c(-5000, 5000, -5000, 5000)
text     = annotate(geom  = "text", 
                     x     = c(1:4), 
                     y     = (double$k + f)/max(double$k), 
                     label = double$k,    
                     color = "blue", 
                     size  = 3)
legend   = theme(legend.justification = c(1,1), 
                  legend.position      = c(1,1),
                  legend.title         = element_blank())

# combine and draw plot
plot = p + theme_bw() + retrate  + line + text.l + histo + caption + text + legend
plot 

width = 1
hight = 1

# save plot as pdf
b=11 #some factor to make nice proportion of font and everything else
ggsave(filename = "./graphs/plots/item_type.pdf", plot,
       width = b*width, height = b*hight, dpi = 150, units = "cm", device='pdf')
plot
dev.off()
