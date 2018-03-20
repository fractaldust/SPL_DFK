#----------------------------------------------------------------------------------
# Prepare_Dataset.R
#----------------------------------------------------------------------------------
# prepares data sets for model evaluation 
# saves joint and clean as csv, that we don't have to run whole data cleaning 
# routine from the beginning over and over again
# joint:        everything that comes from Data Cleaning (150.000 rows, many variables)
# clean:        only variables that are needed for prediction (150.000 rows)
# known:        part of "clean" where return is known (100.000 rows)
# unknown:      part of "clean" where return is unknown (50.000 rows)
#----------------------------------------------------------------------------------

library(lattice)
library(ggplot2)
library(caret)
set.seed(1234)

setkey(joint, order_item_id)

# final list of variables that are used for prediction 
clean = joint[, list("order_item_id" = order_item_id, 
                     "return"        = return,
                     "user_retrate"  = user_retrate,
                     "item_retrate"  = item_retrate,
                     "item_type"     = item_type,     
                     "item_price"    = item_price,    
                     "price_comp"    = price_comp_c,  
                     "double_item"   = double_item,    
                     "delivery_time" = delivery_time,
                     "tau"           = tau
                     ),]

# seperate data back to KNOWN and UNKNOWN part
known       = clean[1:nrow(train.set),]
unknown     = clean[(nrow(train.set)+1):nrow(clean),]

save(known, unknown, file = "./data/known-unknown-data.RData")
rm(list=ls())
