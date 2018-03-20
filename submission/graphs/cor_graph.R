library(corrplot)

load(file = "./data/known-unknown-data.RData")
source(file = "./nnet/00-3-nnet_DataPrep.R")

# combine dummy-variables (binary) 
# to categorical variable "item_type"
known.n$item_type[known$item_type=="clothes"] = 1
known.n$item_type[known$item_type=="shoes"] = 2
known.n$item_type[known$item_type=="rest"] = 3
known.n$item_type[known$item_type=="unsized"] = 4

# delete dummy-variables
known.n$item_typeclothes = NULL
known.n$item_typerest    = NULL
known.n$item_typeshoes   = NULL
known.n$item_typeunsized = NULL

# delete "helper" features that are not used in prediction 
known.n$tau = NULL
known.n$order_item_id = NULL

# create and plot correlation matrix
M = cor(known.n)
plot = corrplot(M, method="circle")
plot 

width = 1
hight = 1 
b = 11
pdf("./graphs/plots/correlation.pdf")
corrplot(M, method="circle")
dev.off()
