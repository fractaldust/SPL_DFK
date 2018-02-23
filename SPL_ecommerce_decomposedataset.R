#----------------------------------------------------------------------------------
# decomposes data set in pure subsets
#----------------------------------------------------------------------------------
# creates known and unknown data sets for different models .f : full model can be trained (item_retrate and
# user_ratrate are both clean) .i : model without item_retrate (only user_retrate is clean) .u : model without
# user_retrate (only item_retrate is clean) .iu : model without item_retrate AND without user_ratrate : BOTH
# item_retrate and user_retrate contain unreasonable categories
#----------------------------------------------------------------------------------

library(data.table)

#----------------------------------------------------------------------------------
# .f : full model
#----------------------------------------------------------------------------------
# remove : 'unknown' and 'new' of 'item_retrate' AND 'user_retrate' model : item_retrate AND user_retrate are clean
# now, can be used for model n.k = 25266 n.u = 10082
known.f <- known[item_retrate != "unknown", ]
known.f <- known.f[user_retrate != "unknown", ]
unknown.f <- unknown[item_retrate != "unknown" & user_retrate != "unknown", ]
unknown.f <- unknown.f[item_retrate != "new" & user_retrate != "new", ]

#----------------------------------------------------------------------------------
# .i : model without item_retrate
#----------------------------------------------------------------------------------
# remove : 'unknown' and 'new' of 'user_retrate' model : user_retrate is clean now, can be used for model n.k =
# 1857 n.u = 813
known.i <- known[user_retrate != "unknown" & item_retrate == "unknown", ]
unknown.i <- unknown[user_retrate != "unknown" & user_retrate != "new", ]
unknown.i <- unknown.i[item_retrate == "unknown" | item_retrate == "new", ]

#----------------------------------------------------------------------------------
# .u : model without user_retrate
#----------------------------------------------------------------------------------
# remove : 'unknown' and 'new' of 'item_retrate' model : item_retrate is clean now, can be used for model n.k =
# 67143 n.u = 35703
known.u <- known[item_retrate != "unknown" & user_retrate == "unknown", ]
unknown.u <- unknown[item_retrate != "unknown" & item_retrate != "new", ]
unknown.u <- unknown.u[user_retrate == "unknown" | user_retrate == "new", ]

#----------------------------------------------------------------------------------
# .iu : model without item_retrate AND user_retrate
#----------------------------------------------------------------------------------
# leftover : leftover rows where BOTH 'item_retrate' AND 'user_retrate' : have uncertain categories model :
# user_retrate and item_retrate are both not clean : model without 'item_retrate' AND 'user_retrate' --> extension
# .iu n.k = 5734 n.u = 3402
known.iu <- known[user_retrate == "unknown" & item_retrate == "unknown", ]
unknown.iu <- unknown[user_retrate == "unknown" | user_retrate == "new", ]
unknown.iu <- unknown.iu[item_retrate == "unknown" | item_retrate == "new", ]

