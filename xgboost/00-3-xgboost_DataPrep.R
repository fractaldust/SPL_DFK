#----------------------------------------------------------------------------------
# prepare Data for nnet
#----------------------------------------------------------------------------------
# input    : known, unknown
# output   : known.n, unknown.n, split.idx.woe
#----------------------------------------------------------------------------------

library(caret)
library(klaR)
library(data.table)
source(file = "helperfunctions.R")

joint = rbind(known, unknown)


# translate categories of item_type into dummy variables
mm = data.frame(model.matrix(~ item_type+0, data=joint))
joint = cbind(joint, mm$item_typerest, mm$item_typeclothes, mm$item_typeshoes, mm$item_typeunsized)
joint$item_type = NULL

colnames(joint) = c("order_item_id", "return", "user_retrate", "item_retrate", 
                    "item_price", "price_comp", "double_item", "delivery_time", 
                    "tau","item_typerest", "item_typeclothes", "item_typeshoes", 
                    "item_typeunsized")

# use WOE to translate categories from factor to numeric
fac2num = helper.fac2num()

levels(joint$price_comp) = fac2num$price_comp
joint$price_comp         = as.numeric(as.character(joint$price_comp))

levels(joint$delivery_time) = fac2num$delivery_time
joint$delivery_time         = as.numeric(as.character(joint$delivery_time))

levels(joint$user_retrate) = fac2num$user_retrate
joint$user_retrate         = as.numeric(as.character(joint$user_retrate))

levels(joint$item_retrate) = fac2num$item_retrate
joint$item_retrate         = as.numeric(as.character(joint$item_retrate))

setkey(joint, order_item_id)

# normalize data (better for nnet)
normalizer = caret::preProcess(joint, method = c("center", "scale"))
joint.n    = predict(normalizer, newdata = joint)

# make sure that tr$return and ts$return have 0/1 values
joint.n$return        = as.numeric(joint$return)
joint.n$tau           = joint$tau
joint.n$order_item_id = joint$order_item_id

# split in known.n and unknown.n data set
known.n       = joint.n[1:dim(known)[1],]
unknown.n     = joint.n[(dim(known)[1]+1):dim(joint)[1],]
split.idx.woe = fac2num$split.idx

rm(fac2num, mm, joint)
