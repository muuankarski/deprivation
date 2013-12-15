##############################################################
## Targeting differential, Coady-Grosh-Hoddinott indicator, median program transfer
##############################################################

#### Coverage & Leakage - Coverage is the portion of poor households that receives the transfer
# The idea -> 
# 1. identify people who are poor (done!) 
# 2. identify people who are reveiving benefit
# 3. count the shares within different dimensions and totally
######################
library(survey)
library(ggplot2)
library(plyr)
library(reshape2)
library(car)

# 1. identify the poor - get data from 
head(df.venn)

# 2. identify people who are reveiving benefit - 
# WHICH IS a COMBINATION OF 
# a) "social exclusion not elsewhere classified - HY060G
# b) "HY070G/HY070N: HOUSING ALLOWANCES "


head(merge_mod)
df.sb <- subset(merge_mod, select=c("RB020","HH_ID","PER_ID", # identification
                                    "HY060G","HY070G"))

df.sb$socass <- (df.sb$HY060G + df.sb$HY070G) # add up the two variables

## testing ##
#df.sb.long <- melt(df.sb, id.vars=c("RB020","HH_ID","PER_ID"))
#ggplot(data=subset(df.sb.long, value > 0) , aes(x=value, color=variable)) + geom_density() + 
 # xlim(0,10000) + facet_wrap(~RB020)

#ggplot(data=df.sb.long , aes(x=value, color=variable)) + geom_density() + 
#  xlim(0,1000) + facet_wrap(~RB020)
################


df.sb$benefit[df.sb$socass > 0] <- "recipient"
df.sb$benefit[df.sb$socass == 0] <- "non.recipient"

#df.sb$benefit[df.sb$HY060G > 0] <- "recipient"
#df.sb$benefit[df.sb$HY060G == 0] <- "non.recipient"

#df.sb$benefit[df.sb$HY070G > 0] <- "recipient"
#df.sb$benefit[df.sb$HY070G == 0] <- "non.recipient"

df.coverage <- join(df.venn,df.sb,by="PER_ID")
write.csv(df.coverage, file="df.coverage.csv")

#########################################
# Coverage!
#########################################
# suhteelliset osuudet maittain

#df.recipient <- subset(df.coverage, benefit %in% 'recipient')
#d.df <- svydesign(id=~PER_ID, weights=~RB050, data=df.recipient)
d.df <- svydesign(id=~PER_ID, weights=~RB050, data=df.coverage)
#
ip <- data.frame(prop.table(svytable(~RB020+income_poor+benefit, d.df),1)*100)
wp <- data.frame(prop.table(svytable(~RB020+work_poor+benefit, d.df),1)*100)
mp <- data.frame(prop.table(svytable(~RB020+material_poor+benefit, d.df),1)*100)
md <- data.frame(prop.table(svytable(~RB020+multidim2+benefit, d.df),1)*100)

##################################
# Coverage - multidimensional
##################################
md$Freq <- round(md$Freq, 1)

md.w <- dcast(md, RB020 + multidim2 ~ benefit , value.var="Freq")
md.w$cov <- (md.w$recipient/(md.w$non.recipient+md.w$recipient))*100
md.w$share <- md.w$non.recipient + md.w$recipient

md.w$cov <- round(md.w$cov, 1) 
md.w$share <- round(md.w$share, 1)

head(md.w)
ggplot(data=subset(md.w, multidim2 == 'a.non.poor'), aes(x=RB020)) + facet_wrap(~multidim2) +
  geom_bar(aes(y=share), stat="identity", fill="Grey") +
  geom_bar(aes(y=recipient), stat="identity", fill="Black") + coord_flip() +
  geom_text(aes(y=recipient, label=cov), hjust=-0.1, size=3) +
  geom_text(aes(y=share, label=share), hjust=0.5, size=3, color="Dim Grey")
  
ggplot(data=subset(md.w, multidim2 != 'a.non.poor'), aes(x=RB020)) + facet_wrap(~multidim2) +
  geom_bar(aes(y=share), stat="identity", fill="Grey") +
  geom_bar(aes(y=recipient), stat="identity", fill="Black") + coord_flip() +
  geom_text(aes(y=recipient, label=cov), hjust=-0.1, size=3) +
  geom_text(aes(y=recipient, label=share), hjust=-3, size=4, color="Dim Grey")

ggplot(data=subset(md.w, multidim2 != 'a.non.poor'), aes(x=multidim2)) + facet_wrap(~RB020) +
  geom_bar(aes(y=share), stat="identity", fill="Grey") +
  geom_bar(aes(y=recipient), stat="identity", fill="Black") + coord_flip() +
  geom_text(aes(y=recipient, label=cov), hjust=-0.1, size=3) +
  geom_text(aes(y=recipient, label=share), hjust=-2.2, size=4, color="Dim Grey")
##################################
##################################

##################################
# Coverage - Income poverty
##################################

ip.w <- dcast(ip, RB020 + income_poor ~ benefit , value.var="Freq")
ip.w$cov <- (ip.w$recipient/(ip.w$non.recipient+ip.w$recipient))*100
ip.w$share <- ip.w$non.recipient + ip.w$recipient

ip.w$cov <- round(ip.w$cov, 1) 
ip.w$share <- round(ip.w$share, 1)

ggplot(data=ip.w, aes(x=RB020)) + facet_wrap(~income_poor) +
  geom_bar(aes(y=share), stat="identity", fill="Grey") +
  geom_bar(aes(y=recipient), stat="identity", fill="Black") + coord_flip() +
  geom_text(aes(y=recipient, label=cov), hjust=-0.1, vjust=1, size=3) +
  geom_text(aes(y=share, label=share), hjust=0.5, vjust=-1, size=3, color="Dim Grey")

##################################
# Coverage - Material deprivation
##################################

mp.w <- dcast(mp, RB020 + material_poor ~ benefit , value.var="Freq")
mp.w$cov <- (mp.w$recipient/(mp.w$non.recipient+mp.w$recipient))*100
mp.w$share <- mp.w$non.recipient + mp.w$recipient

mp.w$cov <- round(mp.w$cov, 1) 
mp.w$share <- round(mp.w$share, 1)

ggplot(data=mp.w, aes(x=RB020)) + facet_wrap(~material_poor) +
  geom_bar(aes(y=share), stat="identity", fill="Grey") +
  geom_bar(aes(y=recipient), stat="identity", fill="Black") + coord_flip() +
  geom_text(aes(y=recipient, label=cov), hjust=-0.1, vjust=1, size=3) +
  geom_text(aes(y=share, label=share), hjust=0.5, vjust=-1, size=3, color="Dim Grey")

##################################
# Coverage - Work poverty
##################################

wp.w <- dcast(wp, RB020 + work_poor ~ benefit , value.var="Freq")
wp.w$cov <- (wp.w$recipient/(wp.w$non.recipient+wp.w$recipient))*100
wp.w$share <- wp.w$non.recipient + wp.w$recipient

wp.w$cov <- round(wp.w$cov, 1) 
wp.w$share <- round(wp.w$share, 1)

ggplot(data=wp.w, aes(x=RB020)) + facet_wrap(~work_poor) +
  geom_bar(aes(y=share), stat="identity", fill="Grey") +
  geom_bar(aes(y=recipient), stat="identity", fill="Black") + coord_flip() +
  geom_text(aes(y=recipient, label=cov), hjust=-0.1, vjust=1, size=3) +
  geom_text(aes(y=share, label=share), hjust=0.5, vjust=-1, size=3, color="Dim Grey")

#####################
#####################

#####################
# testing - missing data
head(df.coverage)
spineMiss(df.coverage[, c("RB020","benefit")])
spineMiss(df.coverage[, c("RB020","work_poor")])
spineMiss(df.coverage[, c("RB020","income_poor")])
spineMiss(df.coverage[, c("RB020","material_poor")])
spineMiss(df.coverage[, c("RB020","multidim")])
spineMiss(df.coverage[, c("RB020","age")])

head(df.venn)
spineMiss(df.venn[, c("RB020","work_poor")])
spineMiss(df.venn[, c("RB020","income_poor")])
spineMiss(df.venn[, c("RB020","material_poor")])
spineMiss(df.venn[, c("RB020","multidim")])
spineMiss(df.venn[, c("RB020","age")])

#####################
#####################

#####################
#### testing ####
ggplot(ip, aes(x=RB020, y=Freq, fill=income_poor)) + geom_bar(stat="identity")
ggplot(wp, aes(x=RB020, y=Freq, fill=work_poor)) + geom_bar(stat="identity")
ggplot(mp, aes(x=RB020, y=Freq, fill=material_poor)) + geom_bar(stat="identity")
ggplot(md, aes(x=RB020, y=Freq, fill=multidim)) + geom_bar(stat="identity") + 
  scale_fill_manual(values=cbPalette)
#####################
#####################

#
ip.2 <- subset(ip, income_poor %in% 'inc.poor')
wp.2 <- subset(wp, work_poor %in% 'work.poor')
mp.2 <- subset(mp, material_poor %in% 'mt.poor')
md.2 <- md
#md.2 <- subset(md, multidim %in% 'work.poor|inc.poor|mt.poor') # hox!
#
names(ip.2) <- c("geo.time","var1","var2","value")
names(wp.2) <- c("geo.time","var1","var2","value")
names(mp.2) <- c("geo.time","var1","var2","value")
names(md.2) <- c("geo.time","var1","var2","value")
#
cov <- rbind(ip.2,wp.2,mp.2,md.2)
cov.w <- dcast(cov, geo.time + var2 ~var1, value.var="value")
write.csv2(cov.w, file="socass_venn.csv")

ggplot(cov, aes(x=geo.time,y=value,fill=var1)) +
  geom_bar(position="dodge", stat="identity") + 
  coord_cartesian(ylim=c(0, 75))

#
svg("socass_venn_DE.svg")
plot(DE.venn <- venneuler(c(IncomePoor=41.32, WorkPoor=51.45, MaterialPoor=29.63,"IncomePoor&MaterialPoor"=3.25,"MaterialPoor&WorkPoor"=9.68,"IncomePoor&WorkPoor"=14.54,"IncomePoor&MaterialPoor&WorkPoor"=13.47)))
dev.off()
svg("socass_venn_IT.svg")
plot(IT.venn <- venneuler(c(IncomePoor=26.99, WorkPoor=25.83, MaterialPoor=18.82,"IncomePoor&MaterialPoor"=1.66,"MaterialPoor&WorkPoor"=4.00,"IncomePoor&WorkPoor"=9.41,"IncomePoor&MaterialPoor&WorkPoor"=6.24)))
dev.off()
svg("socass_venn_PL.svg")
plot(PL.venn <- venneuler(c(IncomePoor=51.02, WorkPoor=39.94, MaterialPoor=55.21,"IncomePoor&MaterialPoor"=11.11,"MaterialPoor&WorkPoor"=5.01,"IncomePoor&WorkPoor"=6.24,"IncomePoor&MaterialPoor&WorkPoor"=23.94)))
dev.off()
svg("socass_venn_RO.svg")
plot(RO.venn <- venneuler(c(IncomePoor=39.83, WorkPoor=23.31, MaterialPoor=49.12,"IncomePoor&MaterialPoor"=17.76,"MaterialPoor&WorkPoor"=3.21,"IncomePoor&WorkPoor"=4.32,"IncomePoor&MaterialPoor&WorkPoor"=9.47)))
dev.off()
svg("socass_venn_SE.svg")
plot(SE.venn <- venneuler(c(IncomePoor=42.96, WorkPoor=46.10, MaterialPoor=6.85,"IncomePoor&MaterialPoor"=0.83,"MaterialPoor&WorkPoor"=1.31,"IncomePoor&WorkPoor"=24.21,"IncomePoor&MaterialPoor&WorkPoor"=2.71)))
dev.off()
svg("socass_venn_UK.svg")
plot(UK.venn <- venneuler(c(IncomePoor=46.48, WorkPoor=60.12, MaterialPoor=13.33,"IncomePoor&MaterialPoor"=1.65,"MaterialPoor&WorkPoor"=4.26,"IncomePoor&WorkPoor"=27.30,"IncomePoor&MaterialPoor&WorkPoor"=6.09)))
dev.off()
#





#########################################
# Leakage
#########################################
# same survey design is fine

# lasketaan HY060G etuuden ryhmittäiset (maa+köyhä/ei-köyhä) SUMMAT

# We need both 1) the share of ratio of non-poor recipients of 
# total recipients and 2) ratio of total transfers received by 
# non-poor recipients of total sum of transfers

df.bnf.r <- data.frame(svyby(~HY060G, ~RB020+income_poor, d.df, svytotal)) # benefit-ratio
ggplot(df.bnf.r, aes(x=RB020, y=HY060G, fill=income_poor)) +
  geom_bar(stat="identity", position="dodge") +
  opts(legend.position="top")

df.bnf.r.w <- dcast(df.bnf.r, RB020 ~ income_poor, value.var="HY060G") # to wide
names(df.bnf.r.w) <- c("RB020","nonpoor","poor")

#
df.pop.r <- svyby(~income_poor, ~RB020, d.df, svytotal) # population-ratio
names(df.pop.r) <- c("RB020","nonpoor","poor","se.nonpoor","se.poor")

df.pop.r.l <- melt(df.pop.r, id.vars="RB020", 
                   measure.vars=c("nonpoor",
                                  "poor"))

ggplot(df.pop.r.l, aes(x=RB020, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  opts(legend.position="top")


# ratios
df.bnf.r.w$leakage.benefit <- (df.bnf.r.w$nonpoor/df.bnf.r.w$poor)*100
df.pop.r$leakage.pop <- (df.pop.r$nonpoor/df.pop.r$poor)*100


# Targeting differential
## income poor
ip.cov <- subset(cov, var1 %in% 'inc.poor', select=c("geo.time","value"))
names(ip.cov) <- c("RB020","coverage")
leak.bnf <- subset(df.bnf.r.w, select=c("RB020","leakage.benefit"))
leak.pop <- subset(df.pop.r, select=c("RB020","leakage.pop"))
##
ip.trg.diff <- join(ip.cov,leak.bnf,by="RB020")
ip.trg.diff <- join(ip.trg.diff,leak.pop,by="RB020")
ip.trg.diff$trg.diff.bnf <- ip.trg.diff$coverage-ip.trg.diff$leakage.benefit
ip.trg.diff$trg.diff.pop <- ip.trg.diff$coverage-ip.trg.diff$leakage.pop

head(ip.trg.diff)

########################################################
########################################################
##          Coady-Grosh-Hoddinott indicator           ##
########################################################
########################################################
# 1) find the total benefit sum
df.bnf.r <- data.frame(svyby(~HY060G, ~RB020+income_poor, d.df, svytotal)) # benefit-ratio
cgh.bnf <- dcast(df.bnf.r, RB020 ~ income_poor, value.var="HY060G") # to wide
names(cgh.bnf) <- c("RB020","bnf.nonpoor","bnf.poor")
# 2) find the total n of both groups
cgh.nr <- svyby(~income_poor, ~RB020, d.df, svytotal) # population-ratio
names(cgh.nr) <- c("RB020","nr.nonpoor","nr.poor","se.nonpoor","se.poor")
cgh.nr <- subset(cgh.nr, select=c("RB020","nr.nonpoor","nr.poor"))
# 3) join the datas
df.cgh <- join(cgh.bnf,cgh.nr,by="RB020")
# 3) count the within group ratios of bnf and r
df.cgh$meanbnf.nonpoor <- df.cgh$bnf.nonpoor/df.cgh$nr.nonpoor
df.cgh$meanbnf.poor <- df.cgh$bnf.poor/df.cgh$nr.poor
df.cgh$cgh <- df.cgh$meanbnf.poor/df.cgh$meanbnf.nonpoor

ggplot(df.cgh, aes(x=RB020,y=cgh)) + 
  geom_bar(stat="identity",  fill="#E69F00") +
  geom_hline(aes(yintercept=1), colour="Dim Grey", linetype="dashed")


# Jess!!