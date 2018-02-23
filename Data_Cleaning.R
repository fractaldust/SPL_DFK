# setwd("~/Documents/Projects/OneMoment/bads-ws1718-group07/submission")
#----------------------------------------------------------------------------------
# Data Cleaning
#----------------------------------------------------------------------------------
# Cleans training and test set
# creates new variables 
# input: BADS_WS1718_known.csv
#        BADS_WS1718_unknown.csv
# output: joint - data.table of train+test set with clean variables
#----------------------------------------------------------------------------------

library(data.table)
set.seed(1234)

# load helperfunctions
source(file = "helperfunctions.R")

# load data
train.set <- read.csv(file="./data/BADS_WS1718_known.csv", stringsAsFactors = FALSE)
test.set  <- read.csv(file="./data/BADS_WS1718_class.csv", stringsAsFactors = FALSE)
test.set$return <- 200000
joint     <- data.table(rbind(train.set, test.set)) # use fread
joint.k   <- data.table(train.set) #only training data

setkey(joint, user_id, item_id, order_item_id)
setkey(joint.k, user_id, item_id, order_item_id)


#----------------------------------------------------------------------------------
# USER
#----------------------------------------------------------------------------------
# user_retrate  : to what rate did user return in the past
# user_activity : how many items user ordered (bundled in classes 1-2-5-10-25-max)
# user_age      : age of user
#----------------------------------------------------------------------------------

user <- joint[, list("user_nr.obs" = .N,
                     "user_dob"    = user_dob
), 
by = "user_id"]

# # user_activity
user$user_activity <- user$user_nr.obs

# user_age
user$user_dob <- as.Date(user$user_dob)
user$user_age <- as.integer((as.Date("2013-01-22") - user$user_dob)/365.25)
user <- unique(user)

c   <- c(0,18,30,40,50,60,70,80,150)
tag <- c("0-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
user$user_age_c <- split(user$user_age_c, 
                         user$user_age, 
                         c, tag)
user$user_age_c[which(user$user_dob=="1900-11-19")] <- "NA.1"
user$user_age_c[which(user$user_dob=="1949-11-19")] <- "NA.2"
user$user_age_c[which(is.na(user$user_dob))]        <- "NA.3"

# user_retrate
# only consider training data set, where return value is known
user.k <- joint.k[, list("k_nr.obs"      = .N,
                         "user_retrate" = mean(return)), 
                  by = "user_id"]
c   <- c(0, 0.17, 0.38, 0.58, 0.79, 1.1)
tag <- c("very low", "low","normal", "high", "very high")

user.k$user_retrate_c <- split(user.k$user_retrate_c, 
                               user.k$user_retrate, 
                               c, tag)
user.k$user_retrate_c[user.k$k_nr.obs <= 6] <- "unknown"

# user_id of users that ordered ONLY in test set
new.user <- unique(user$user_id[!(user$user_id %in% user.k$user_id)])
length(new.user) # 6785 new users in test set


# combine known and general user-matrix
user <- user.k[user]
user$user_retrate_c[is.na(user$user_retrate_c)] <- "new"
user$user_retrate_c <- factor(user$user_retrate_c, 
                              levels = c(tag, "unknown", "new"))

# variables that are going to be used in prediction
user.final <- user[, list("user_id"       = user_id, 
                          "user_retrate"  = as.factor(user_retrate_c),
                          "user_age"      = as.factor(user_age_c),
                          "user_activity" = as.numeric(user_activity)
)]

rm(c, tag, user.k, new.user)

#----------------------------------------------------------------------------------
# ITEM
#----------------------------------------------------------------------------------
## item_retrate    : to what rate was item returned in the past
## item_avg.price  : average price of item
## item_type       : type of item
## item_nr.obs     : how often item was bought
#----------------------------------------------------------------------------------

# item_avg.price
item <- joint[, list("item_nr.obs"    = .N,
                     "item_avg.price" = mean(item_price), 
                     "item_size"      = item_size
), 
by = "item_id"]


# item type depending on size system
# shoes    : 36:42, 5,5+,...,9,
# clothes  : s,m,l,xl,xxl, S, M,...,XXL
# unsized  : "unsized", "other"

shoes <- c(30:48, paste(35:46, "+", sep = ""),
           1:9, paste(1:12, "+", sep = ""))
clothes <- c("xs", "s", "m", "l", "xl", "xxl", "xxxl",
           "XS", "S", "M", "L", "XL", "XXL", "XXXL",
           100:176)
unsized <- c("unsized")

item$type                               <- "rest"
item$type[item$item_size %in% shoes]    <- "shoes"
item$type[item$item_size %in% clothes]  <- "clothes"
item$type[item$item_size %in% unsized]  <- "unsized"

item <- unique(item)

type <- item[,list("type"=type), by= "item_id"]
type <- type[, .N, by=.(item_id, type)]

# matrix which shows how often item got categorised as type (shoes/clothes/unsized/rare)
# if one item_id gets categorised in more than one categorie (e.g. shoes AND clothes)
# then "clothes" wins, because it is more likely that clothes have the sizes like shoes than
# vice versa
type.matrix <- dcast(type, item_id ~ type, value.var="N", fill = 0)

id.clothes <- type.matrix$item_id[as.logical(type.matrix$clothes)]
id.shoes <- type.matrix$item_id[as.logical(type.matrix$shoes)]

# here: overwrite item_type, if it got categorised two times
item$type[item$item_id %in% id.shoes] <- "shoes"
item$type[item$item_id %in% id.clothes] <- "clothes"
#-------------------------------------------------------------------------

# item_retrate
item.k <- joint.k[, list("item_nr.obs.k" = .N,
                         "item_retrate"  = mean(return)), 
                  by = "item_id"]
c   <- c(0, 0.26, 0.40, 0.56, 0.71, 1.1)
tag <- c("very low", "low", "normal", "high", "very high")

item.k$item_retrate_c <- split(item.k$item_retrate_c, 
                               item.k$item_retrate, 
                               c, tag)
item.k$item_retrate_c[item.k$item_nr.obs.k <= 15] <- "unknown"

# item_id of items that ordered ONLY in test set
new.item <- unique(item$item_id[!(item$item_id %in% item.k$item_id)])
length(new.item) # 103 new items in test set


# combine known and general item-matrix
setkey(item, item_id)
setkey(item.k, item_id)
item <- item.k[item]
item$item_retrate_c[is.na(item$item_retrate_c)] <- "new"
item$item_retrate_c <- factor(item$item_retrate_c, 
                              levels = c(tag, "unknown", "new"))

# variables that are going to be used in prediction
item.final <- item[, list("item_id"         = item_id, 
                          "item_retrate"    = as.factor(item_retrate_c),
                          "item_avg.price" = item_avg.price, 
                          #"item_nr.obs"     = item_nr.obs, 
                          "item_type"       = as.factor(type)
)]
item.final <- unique(item.final)
dim(item.final) # should be 2759 - correct

rm(shoes, clothes, unsized, type, type.matrix, new.item,id.clothes, id.shoes, c, tag)

#----------------------------------------------------------------------------------
# Insert new information in data set
#----------------------------------------------------------------------------------
joint <- joint[user.final]
setkey(joint, item_id)
joint <- joint[item.final]
dim(joint) #150000 - correct


#----------------------------------------------------------------------------------
# ORDER SPECIFIC
#----------------------------------------------------------------------------------
# order_week    : during which week of the year item was ordered
# price_comp    : if item was bought cheaper or more expensive 
#               : than average (in percent)
# item_price    : actual price of item
# delivery_time : how many days between order and delivery
# double_item   : if item was bought by same customer twice (or more)
# tau           : group items according to item_price 
#               : only need the tau for loss function not for prediction 
#----------------------------------------------------------------------------------

# order week
joint$order_date <- as.Date(joint$order_date)
joint$order_week <- as.numeric(lubridate::week(joint$order_date))

# price_comp
joint$price_comp <- NA
joint$price_comp <- ((joint$item_price - joint$item_avg.price)
                     /joint$item_avg.price)
# 207 NA's because devided by 0 (item_avg.price==0)
joint$price_comp[is.na(joint$price_comp)] <- 0

c   <- c(-1, -0.5, -0.1, 0.1, 0.5, 1, 9)
tag <- c[-1]

for (i in 2:length(c)){
  tag[i-1]  <- paste(c[i-1], "to", c[i])
}

joint$price_comp_c <- NA
joint$price_comp_c <- split(joint$price_comp_c, joint$price_comp, c, tag)
joint$price_comp_c[is.na(joint$price_comp_c)] <- "NA"
joint$price_comp_c <- factor(joint$price_comp_c, levels = c(tag, "NA"))

# delivery_time
joint$order_date    <- as.Date(joint$order_date)
joint$delivery_date <- as.Date(joint$delivery_date)
joint$delitime      <- joint$delivery_date - joint$order_date

c   <- as.numeric(c(-9000,0,2,5,20,50,200))
tag <- c("neg", "0-1", "2-4", "5-19", "20-50", "50+")

joint$delivery_time <- split(joint$delivery_time,
                             joint$delitime,
                             c, tag)
joint$delivery_time[is.na(joint$delivery_time)] <- "?"

joint$delivery_time <- factor(joint$delivery_time, levels = c(tag[-1],"neg", "?"))

# double_item
setkey(joint, user_id, item_id)
double <- joint[, list(count=.N), by = c("user_id","item_id")]

colnames(double) <- c("user_id", "item_id", "double_item")
double$double_item[double$double_item >= 4] <- 4

joint <- joint[double]

# tau 
c   <- c(0,30,59,79,90,120,1000) 
tag <- c(1,2,3,4,5,6)

joint$tau <- split(joint$tau,
                   joint$item_price, 
                   c, tag)

rm(c, tag, double)

#----------------------------------------------------------------------------------
# write file of complete data set
#----------------------------------------------------------------------------------
#write.csv(joint, file="./data/joint.csv")

