#############################################
# MATERIAALINEN PUUTE
#############################################
df <- subset(merge_mod, select=c(RB020,HH_ID,PER_ID,RB050,DB090,HS010,
                                 HS011,HS020,HS021,HS030,HS031,HH050,HS060,HS050,
                                          HS040,HS110,HS080,HS100,HS070,
                                 HS010_F,HS011_F,HS020_F,HS021_F,HS030_F,HS031_F,
                                 HH050_F,HS050_F,HS040_F,HS060_F,
                                 HS110_F,HS080_F,HS100_F,HS070_F)) # 2008
#df <- subset(merge_mod, select=c(RB020,HH_ID,PER_ID,RB050,DB090,HS010,
#                                 HS020,HS030,HH050,HS060,HS050,
#                                 HS040,HS110,HS080,HS100,HS070)) # 2004
head(df)
df <- subset(df, RB020 %in% c("DK","RO",
                              "PL","DE",
                              "IT","UK"))

#####



####



##############
# eurostatin sivuilla: 

#Severely materially deprived persons have living conditions
#greatly constrained by a lack of resources and cannot afford at least four of
#the following: 
# 1 to pay rent or (HS011 - 1 yes, once; 2 yes, twice or more; 3 no)
# 2 utility bills; (HS021 - 1 yes, once; 2 yes, twice or more; 3 no)
# 3 to keep their home adequately warm; (HH050 1 yes, 2 no)
# 4 to pay unexpected expenses; (HS060 1 yes, 2 no)
# 5 to eat meat, fish or a protein equivalent every second day; (HS050 1 yes, 2 no)
# 6 a week holiday away from home (HS040 1 yes, 2 no); 
# 7 a car; (HS110 1 yes, 2 cannot afford, 3 no other reason)
# 8 a washing machine; (HS100 1 yes, 2 cannot afford, 3 no other reason)
# 9 a colour TV; (HS080 1 yes, 2 cannot afford, 3 no other reason)
# 10 or a telephone. (HS070 1 yes, 2 cannot afford, 3 no other reason)
## -tsekataan eka missän on NA ja kui paljon

#df$HS010 <- factor(df$HS010)
#df$HS020 <- factor(df$HS020)
#df$HS030 <- factor(df$HS030)

#df$HS011 <- factor(df$HS011)
#df$HS021 <- factor(df$HS021)
#df$HS031 <- factor(df$HS031)

df$HS040 <- factor(df$HS040)
df$HS050 <- factor(df$HS050)
df$HH050 <- factor(df$HH050)
df$HS060 <- factor(df$HS060)
df$HS070 <- factor(df$HS070)
df$HS080 <- factor(df$HS080)
df$HS100 <- factor(df$HS100)
df$HS110 <- factor(df$HS110)

df$RB020 <- factor(df$RB020)

###############
# missing data
df$RB020 <- factor(df$RB020)
par(mfrow=c(2,2))
spineMiss(df[, c("RB020", "HS010")])
spineMiss(df[, c("RB020", "HS011")])
spineMiss(df[, c("RB020", "HS020")])
spineMiss(df[, c("RB020", "HS021")])
spineMiss(df[, c("RB020", "HS030")])
spineMiss(df[, c("RB020", "HS031")])


##############
df.1 <- subset(df, RB020 %in% c('RO','PL','IT','DK')) # because missing data in different variables for diff cntry
df.2 <- subset(df, RB020 %in% c('DE','UK'))

# pay rent HS010
qplot(df.2$HS030_F)
df.2$md_rent[df.2$HS010_F == -2] <- 0
df.2$md_rent[df.2$HS010 == 1] <- 1
df.2$md_rent[df.2$HS010 != 1] <- 0
# utility bill HS020
df.2$md_utility[df.2$HS020_F == -2] <- 0
df.2$md_utility[df.2$HS020 == 1] <- 1
df.2$md_utility[df.2$HS020 != 1] <- 0
# HS030: Arrears on hire purchase instalments or other loan payments
df.2$md_hire[df.2$HS030_F == -2] <- 0
df.2$md_hire[df.2$HS030 == 1] <- 1
df.2$md_hire[df.2$HS030 != 1] <- 0
#
df.2$md_hcost_sum = df.2$md_rent + df.2$md_utility + df.2$md_hire
df.2$md_hcost[df.2$md_hcost_sum >= 1] <- 1
df.2$md_hcost[df.2$md_hcost_sum == 0] <- 0
summary(df.2$md_hcost)

# pay rent HS011
df.1$md_rent[df.1$HS011_F == -2] <- 0
df.1$md_rent[df.1$HS011 == 1] <- 1
df.1$md_rent[df.1$HS011 != 1] <- 0
# utility bill HS021
df.1$md_utility[df.1$HS021_F == -2] <- 0
df.1$md_utility[df.1$HS021 == 1] <- 1
df.1$md_utility[df.1$HS021 != 1] <- 0
# HS031: Arrears on hire purchase instalments or other loan payments
df.1$md_hire[df.1$HS031_F == -2] <- 0
df.1$md_hire[df.1$HS031 == 1] <- 1
df.1$md_hire[df.1$HS031 != 1] <- 0
#
df.1$md_hcost_sum = df.1$md_rent + df.1$md_utility + df.1$md_hire
df.1$md_hcost[df.1$md_hcost_sum >= 1] <- 1
df.1$md_hcost[df.1$md_hcost_sum == 0] <- 0

df <- rbind(df.1,df.2)

summary(df$md_hcost)
# holiday HS040
df$md_holiday[df$HS040 == 2] <- 1
df$md_holiday[df$HS040 != 2] <- 0
summary(df$md_holiday)
# food HS050
df$md_food[df$HS050 == 2] <- 1
df$md_food[df$HS050 != 2] <- 0
summary(df$md_food)
# energy HH050
df$md_energy[df$HH050 == 2] <- 1
df$md_energy[df$HH050 != 2] <- 0
summary(df$md_energy)
# unexp HS060
df$md_unexp[df$HS060 == 2] <- 1
df$md_unexp[df$HS060 != 2] <- 0
summary(df$md_unexp)
# telep HS070
df$md_telep[df$HS070 == 2] <- 1
df$md_telep[df$HS070 != 2] <- 0
summary(df$md_telep)
# tv HS080
df$md_tv[df$HS080 == 2] <- 1
df$md_tv[df$HS080 != 2] <- 0
summary(df$md_tv)
# wash HS100
df$md_wash[df$HS100 == 2] <- 1
df$md_wash[df$HS100 != 2] <- 0
summary(df$md_wash)
# car HS110
df$md_car[df$HS110 == 2] <- 1
df$md_car[df$HS110 != 2] <- 0
summary(df$md_car)
#########################
# uudelleenkoodataan puute = 1, muu = nolla ja muutos numeerisiksi yhteenlaskua varten
#df$md_rent <- as.numeric(df$md_rent)
#df$md_utility <- as.numeric(df$md_utility)
#df$md_hire <- as.numeric(df$md_hire)
df$md_hcost <- as.numeric(df$md_hcost)
df$md_holiday <- as.numeric(df$md_holiday)
df$md_food <- as.numeric(df$md_food)
df$md_energy <- as.numeric(df$md_energy)
df$md_unexp <- as.numeric(df$md_unexp)
df$md_telep <- as.numeric(df$md_telep)
df$md_tv <- as.numeric(df$md_tv)
df$md_wash <- as.numeric(df$md_wash)
df$md_car <- as.numeric(df$md_car)

#df$md_rent <- recode(df$md_rent, "1=1; 2=0")
#df$md_utility <- recode(df$md_utility, "1=1; 2=0")
#df$md_hire <- recode(df$md_hire, "1=1; 2=0")
df$md_hcost <- recode(df$md_hcost, "1=1; 2=0")
df$md_holiday <- recode(df$md_holiday, "1=1; 2=0")
df$md_food <- recode(df$md_food, "1=1; 2=0")
df$md_energy <- recode(df$md_energy, "1=1; 2=0")
df$md_unexp <- recode(df$md_unexp, "1=1; 2=0")
df$md_telep <- recode(df$md_telep, "1=1; 2=0")
df$md_tv <- recode(df$md_tv, "1=1; 2=0")
df$md_wash <- recode(df$md_wash, "1=1; 2=0")
df$md_car <- recode(df$md_car, "1=1; 2=0")


#########################
# summamuuttuja
df$md_total <- df$md_hcost +
    df$md_holiday +
  df$md_food +
  df$md_energy +
  df$md_unexp +
  df$md_telep +
  df$md_tv +
  df$md_wash +
  df$md_car
summary(df$md_total)
qplot(df$md_total)

# materiaalisesti köyhä on
df$material_poor[df$md_total >= 4] <- "mt.poor"
df$material_poor[df$md_total < 4] <- "mt.non.poor"
df$material_poor <- factor(df$material_poor)

df.mp <- df
summary(df.mp$material_poor)


### testing
df.md <- join(df.ip,df.wp,by="PER_ID")
df.md <- join(df.md,df.mp,by="PER_ID")

summary(df.md)

df.venn <- subset(df.md, select=c("PER_ID","HH_ID","RB020",
                                  "age","work_poor",
                                  "income_poor",
                                  "material_poor",
                                  "RB050"))


df.venn$RB020 <- factor(df.venn$RB020)
d.df <- svydesign(id=~PER_ID, weights=~RB050, data=df.venn)
mp <- data.frame(prop.table(svytable(~RB020+material_poor, d.df),1)*100)
mp


