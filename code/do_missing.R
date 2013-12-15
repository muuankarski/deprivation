


library(VIM)

spineMiss(merge_mod[, c("RB020", "HX090")])

# työköyhyys
aggr(merge_mod[, c("PL070","PL072")], numbers=TRUE, prop = c(TRUE, FALSE))

spineMiss(merge_mod[, c("RB020", "PL070")])
spineMiss(merge_mod[, c("RB020", "PL072")])

spineMiss(merge_mod2[, c("RB020", "PL070")])
spineMiss(merge_mod2[, c("RB020", "PL072")])


#parcoordMiss(merge_mod[, c("RB020", "PL070", "PL072",
                           "HX090")], plotvars = 1:3, 
             highlight = 4, alpha = 0.2)

# materiaalinen köyhyys

aggr(merge_mod[, c("HS010","HS011","HS020","HS021","HS030","HS031","HS040",
                   "HS050","HH050","HS060","HS070",
                   "HS080","HS100","HS110")], numbers=TRUE, prop = c(TRUE, FALSE))

par(mfrow=c(1,1))
aggr(merge_mod[, c("HS011","HS021","HS031","HS040",
                   "HS050")], numbers=TRUE, prop = c(TRUE, FALSE))
aggr(merge_mod[, c("HH050","HS060","HS070",
                   "HS080","HS100","HS110")], numbers=TRUE, prop = c(TRUE, FALSE))

#################################
#################################

merge_mod$RB020 <- factor(merge_mod$RB020)

par(mfrow=c(2,2))
spineMiss(merge_mod[, c("RB020", "HS010")])
spineMiss(merge_mod[, c("RB020", "HS011")])
spineMiss(merge_mod[, c("RB020", "HS020")])
spineMiss(merge_mod[, c("RB020", "HS021")])
spineMiss(merge_mod[, c("RB020", "HS030")])
spineMiss(merge_mod[, c("RB020", "HS031")])


par(mfrow=c(2,2))
spineMiss(merge_mod2[, c("RB020", "HS010")])
spineMiss(merge_mod2[, c("RB020", "HS011")])
spineMiss(merge_mod2[, c("RB020", "HS020")])
spineMiss(merge_mod2[, c("RB020", "HS021")])
spineMiss(merge_mod2[, c("RB020", "HS030")])
spineMiss(merge_mod2[, c("RB020", "HS031")])







spineMiss(merge_mod[, c("RB020", "HS040")])

par(mfrow=c(4,2))
spineMiss(merge_mod[, c("RB020", "HS050")])
spineMiss(merge_mod[, c("RB020", "HH050")])
spineMiss(merge_mod[, c("RB020", "HS060")])
spineMiss(merge_mod[, c("RB020", "HS070")])
spineMiss(merge_mod[, c("RB020", "HS080")])
spineMiss(merge_mod[, c("RB020", "HS100")])
spineMiss(merge_mod[, c("RB020", "HS110")])

#################################
#################################