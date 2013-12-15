# testataan venn-diagrammia

# paketit
## perussetti
library(car)
library(reshape)
library(ggplot2)
library(plyr)
## spessut
library(venneuler)
#library(VennDiagram)
library(laeken)
library(survey)



#############################################
# Yhdistetään köyhyystiedot income, work intensity and material deprivation -muuttujista
# -> ->
summary(df.venn)

head(df.ip)
head(df.wp)
head(df.mp)

df.md <- join(df.ip,df.wp,by="PER_ID")
df.md <- join(df.md,df.mp,by="PER_ID")

summary(df.md)

df.venn <- subset(df.md, select=c("PER_ID","HH_ID","RB020",
                                "age","work_poor",
                                  "income_poor",
                                  "material_poor",
                                  "RB050"))


##########################################################
# VENN-diagrammit
##########################################################
##########################################################
summary(df.venn)
#  poistetaan NA:t
df.venn <- df.venn[!is.na(df.venn$material_poor),]
df.venn <- df.venn[!is.na(df.venn$work_poor),]
df.venn <- df.venn[!is.na(df.venn$income_poor),]
# lasketaan päällekkäiset
df.venn$multidim <- paste(df.venn$work_poor,df.venn$income_poor,
                        df.venn$material_poor, sep = "|")
df.venn$multidim <- factor(df.venn$multidim)
df.venn$multidim2 <- recode(df.venn$multidim,
                                "'work.non.poor|inc.non.poor|mt.non.poor'='a.non.poor';
                                'work.non.poor|inc.non.poor|mt.poor'='c.MaterialPoor';
                                'work.non.poor|inc.poor|mt.non.poor'='b.IncomePoor';
                                'work.non.poor|inc.poor|mt.poor'='e.IncomePoor&MaterialPoor';
                                'work.poor|inc.non.poor|mt.non.poor'='d.WorkPoor';
                                'work.poor|inc.non.poor|mt.poor'='g.MaterialPoor&WorkPoor';
                                'work.poor|inc.poor|mt.non.poor'='f.IncomePoor&WorkPoor';
                                'work.poor|inc.poor|mt.poor'='h.IncomePoor&MaterialPoor&WorkPoor'")
summary(df.venn)
# lasketaan materiaalisesti deprivoituneet
df.venn$multi[df.venn$income_poor == 'inc.poor'] <- "poor"
df.venn$multi[df.venn$work_poor == 'work.poor'] <- "poor"
df.venn$multi[df.venn$material_poor == 'mt.poor'] <- "poor"

df.venn$multi <- recode(df.venn$multi, "'poor'='poor';
                        else='non.poor'")

df.venn$multi <- factor(df.venn$multi)
summary(df.venn$multi)

df.venn$RB020 <- factor(df.venn$RB020)
summary(df.venn$RB020)

table(df.ip$RB020,df.ip$income_poor)
table(df.wp$RB020,df.wp$work_poor)
table(df.mp$RB020,df.mp$material_poor)

head(df.ip)



##########################################################
# Survey-design
##########################################################
d.df <- svydesign(id=~PER_ID, weights=~RB050, data=df.venn)


#########################################
# datat venn-diagrammeihin #
#########################################
# suhteelliset osuudet maittain
ip <- data.frame(prop.table(svytable(~RB020+income_poor, d.df),1)*100)
wp <- data.frame(prop.table(svytable(~RB020+work_poor, d.df),1)*100)
mp <- data.frame(prop.table(svytable(~RB020+material_poor, d.df),1)*100)
md <- data.frame(prop.table(svytable(~RB020+multidim2, d.df),1)*100)

names(ip) <- c("geo.time","variable","value")
names(wp) <- c("geo.time","variable","value")
names(mp) <- c("geo.time","variable","value")
names(md) <- c("geo.time","variable","value")

df <- rbind(ip,wp,mp,md)

df$value <- round(df$value, 1)
#df$value2 <- lapply(df$value, gsub, patt=",", replace=".")
# printataan csv:ksi venn-datojen calc-manipulaatiota varten
df.w <- dcast(df, geo.time ~ variable, value.var="value")
df.w$inc.non.poor <- NULL
df.w$work.non.poor  <- NULL
df.w$mt.non.poor <- NULL
df.w$a.non.poor <- NULL
df.w$b.IncomePoor <- NULL
df.w$c.MaterialPoor <- NULL
df.w$d.WorkPoor <- NULL
write.csv2(df.w, file="venn.csv")


md.w <- dcast(md, geo.time ~ variable, value.var="value")
#write.csv2(md.w, file="mdw.csv")


#########################################
## tehdään tolppakuvio eri luokista
df.z <- subset(df, 
             variable %in% c("inc.poor","work.poor",
              "mt.poor",
              #"work.non-poor|inc.non-poor|mt.non-poor",
              "work.non-poor|inc.non-poor|mt.poor",
              "work.non-poor|inc.poor|mt.non-poor",
              "work.non-poor|inc.poor|mt.poor",
              "work.poor|inc.non-poor|mt.non-poor",
              "work.poor|inc.non-poor|mt.poor",
              "work.poor|inc.poor|mt.non-poor",
              "work.poor|inc.poor|mt.poor"))

ggplot(df.z, aes(x=geo.time, y=value, fill=variable, label=value)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(size=4, position = position_dodge(width=1))
#########################################

##########################
## Printing the svg's ##
##########################

svg("venn_DE.svg")
plot(DE.venn <- venneuler(c(IncomePoor=14.6, WorkPoor=11.1, MaterialPoor=5.0,"IncomePoor&MaterialPoor"=1.0,"MaterialPoor&WorkPoor"=0.4,"IncomePoor&WorkPoor"=5.0,"IncomePoor&MaterialPoor&WorkPoor"=2.1)))
dev.off()
svg("venn_IT.svg")
plot(IT.venn <- venneuler(c(IncomePoor=18.8, WorkPoor=10.9, MaterialPoor=4.9,"IncomePoor&MaterialPoor"=1.4,"MaterialPoor&WorkPoor"=0.5,"IncomePoor&WorkPoor"=4.4,"IncomePoor&MaterialPoor&WorkPoor"=1.5)))
dev.off()
svg("venn_PL.svg")
plot(PL.venn <- venneuler(c(IncomePoor=18.3, WorkPoor=6.8, MaterialPoor=11.2,"IncomePoor&MaterialPoor"=3.4,"MaterialPoor&WorkPoor"=0.4,"IncomePoor&WorkPoor"=1.7,"IncomePoor&MaterialPoor&WorkPoor"=2.0)))
dev.off()
svg("venn_RO.svg")
plot(RO.venn <- venneuler(c(IncomePoor=21.7, WorkPoor=7.1, MaterialPoor=25.9,"IncomePoor&MaterialPoor"=9.5,"MaterialPoor&WorkPoor"=1.2,"IncomePoor&WorkPoor"=1.2,"IncomePoor&MaterialPoor&WorkPoor"=2.2)))
dev.off()
svg("venn_DK.svg")
plot(DK.venn <- venneuler(c(IncomePoor=11.9, WorkPoor=8.1, MaterialPoor=2.2,"IncomePoor&MaterialPoor"=0.5,"MaterialPoor&WorkPoor"=0.6,"IncomePoor&WorkPoor"=3.4,"IncomePoor&MaterialPoor&WorkPoor"=0.5)))
dev.off()
svg("venn_UK.svg")
plot(UK.venn <- venneuler(c(IncomePoor=16.1, WorkPoor=9.8, MaterialPoor=5.5,"IncomePoor&MaterialPoor"=1.0,"MaterialPoor&WorkPoor"=0.9,"IncomePoor&WorkPoor"=3.9,"IncomePoor&MaterialPoor&WorkPoor"=1.8)))
dev.off()


###############################################
### ABsoluuttiset arvot
head(df)

df.p <- df

#  Populations
geo.time <- c("RO","PL","DE","UK","IT","DK")
pop <- c(21.5,38.1,82.2,61.2,59.6,5.6)
df.pop <- data.frame(geo.time,pop)
df.pop$pop <- df.pop$pop * 1000000

df.p$value <- df.p$value / 100
df.popu <- join(df.p,df.pop,by="geo.time")
df.popu$n <- df.popu$value * df.popu$pop

df.popu.w <- dcast(df.popu, geo.time ~ variable, value.var="n")
df.popu.w$inc.non.poor <- NULL
df.popu.w$work.non.poor  <- NULL
df.popu.w$mt.non.poor <- NULL
df.popu.w$a.non.poor <- NULL
df.popu.w$b.IncomePoor <- NULL
df.popu.w$c.MaterialPoor <- NULL
df.popu.w$d.WorkPoor <- NULL
write.csv(df.popu.w, file="venn.popu.csv")


plot(DE.venn <- venneuler(c(IncomePoor=12001200, WorkPoor=9124200, MaterialPoor=4110000,"IncomePoor&MaterialPoor"=822000,"MaterialPoor&WorkPoor"=328800,"IncomePoor&WorkPoor"=4110000,"IncomePoor&MaterialPoor&WorkPoor"=1726200)))
plot(DK.venn <- venneuler(c(IncomePoor=666400, WorkPoor=453600, MaterialPoor=123200,"IncomePoor&MaterialPoor"=28000,"MaterialPoor&WorkPoor"=33600,"IncomePoor&WorkPoor"=190400,"IncomePoor&MaterialPoor&WorkPoor"=28000)))
plot(IT.venn <- venneuler(c(IncomePoor=11204800, WorkPoor=6496400, MaterialPoor=2920400,"IncomePoor&MaterialPoor"=834400,"MaterialPoor&WorkPoor"=298000,"IncomePoor&WorkPoor"=2622400,"IncomePoor&MaterialPoor&WorkPoor"=894000)))
plot(PL.venn <- venneuler(c(IncomePoor=6972300, WorkPoor=2590800, MaterialPoor=4267200,"IncomePoor&MaterialPoor"=1295400,"MaterialPoor&WorkPoor"=152400,"IncomePoor&WorkPoor"=647700,"IncomePoor&MaterialPoor&WorkPoor"=762000)))
plot(RO.venn <- venneuler(c(IncomePoor=4665500, WorkPoor=1526500, MaterialPoor=5568500,"IncomePoor&MaterialPoor"=2042500,"MaterialPoor&WorkPoor"=258000,"IncomePoor&WorkPoor"=258000,"IncomePoor&MaterialPoor&WorkPoor"=473000)))
plot(UK.venn <- venneuler(c(IncomePoor=9853200, WorkPoor=5997600, MaterialPoor=3366000,"IncomePoor&MaterialPoor"=612000,"MaterialPoor&WorkPoor"=550800,"IncomePoor&WorkPoor"=2386800,"IncomePoor&MaterialPoor&WorkPoor"=1101600)))





# datat venn-diagrammeihin #
#########################################
# suhteelliset osuudet maittain
data.frame(svytable(~RB020+income_poor, d.df))
data.frame(svytable(~RB020+work_poor, d.df))
data.frame(svytable(~RB020+material_poor, d.df))
data.frame(svytable(~RB020+multidim2, d.df))

wp <- data.frame(prop.table(svytable(~RB020+work_poor, d.df),1)*100)
mp <- data.frame(prop.table(svytable(~RB020+material_poor, d.df),1)*100)
md <- data.frame(prop.table(svytable(~RB020+multidim2, d.df),1)*100)

names(ip) <- c("geo.time","variable","value")
names(wp) <- c("geo.time","variable","value")
names(mp) <- c("geo.time","variable","value")
names(md) <- c("geo.time","variable","value")






#########################################################################

# frekvenssit maittain
## weighted
prop.table(svytable(~income_poor, d.df))*100
prop.table(svytable(~work_poor, d.df))*100
prop.table(svytable(~material_poor, d.df))*100
prop.table(svytable(~multidim, d.df))*100
## non-weighted
prop.table(table(df.venn$income_poor))*100
prop.table(table(df.venn$work_poor))*100
prop.table(table(df.venn$material_poor))*100
prop.table(table(df.venn$multidim))*100
# frekvenssit koko maittain
## weighted
prop.table(svytable(~RB020+income_poor, d.df),1)*100
prop.table(svytable(~RB020+work_poor, d.df),1)*100
prop.table(svytable(~RB020+material_poor, d.df),1)*100
prop.table(svytable(~RB020+multidim, d.df),1)*100
## non-weighted
prop.table(table(df.venn$RB020,df.venn$income_poor),1)*100
prop.table(table(df.venn$RB020,df.venn$work_poor),1)*100
prop.table(table(df.venn$RB020,df.venn$material_poor),1)*100
prop.table(table(df.venn$RB020,df.venn$multidim),1)*100

# Venn-diagrammin isoihin palloihin tarvitaan kunkin dimension riippumattomat osuudet
d.df <- svydesign(id=~PER_ID, weights=~RB050, 
                  data=subset(df.venn, RB020 %in% 'RO'))
# Income poverty
t <- data.frame(prop.table(svytable(~ageclass+income_poor, d.df),1)*100)
subset(t, Freq > 0.01)
# Work poverty
t <- data.frame(prop.table(svytable(~ageclass+work_poor, d.df),1)*100)
subset(t, Freq > 0.01)
# Material poverty
t <- data.frame(prop.table(svytable(~ageclass+material_poor, d.df),1)*100)
subset(t, Freq > 0.01)
# Multidimensional
t <- data.frame(prop.table(svytable(~ageclass+multidim2, d.df),1)*100)
subset(t, Freq > 0.01)


# ikäluokittain
plot(vd.0.17 <- venneuler(c(IncomePoor=11.55, MaterialPoor=6.26, WorkPoor=13.22, 
                            "IncomePoor&MaterialPoor"=1.23, 
                            "IncomePoor&WorkPoor"=3.9, 
                            "MaterialPoor&WorkPoor"=0.55,
                            "IncomePoor&MaterialPoor&WorkPoor"=2.56)))


plot(vd.0.17)
plot(vd.18.24)
plot(vd.25.54)
plot(vd.55.64)

# Venn-diagrammin isoihin palloihin tarvitaan kunkin dimension riippumattomat osuudet
d.df <- svydesign(id=~PER_ID, weights=~RB050, data=subset(df.venn, RB020 %in% 'FI'))
# Income poverty
data.frame(svytable(~RB020+income_poor, d.df))
# Work poverty
data.frame(svytable(~ageclass+work_poor, d.df))
# Material poverty
data.frame(svytable(~ageclass+material_poor, d.df))
# Multidimensional
data.frame(svytable(~ageclass+multidim2, d.df))

# ikäluokittain
vd.0.17 <- venneuler(c(IncomePoor=7685.645, MaterialPoor=4164.068, WorkPoor=8799.099, 
                       "IncomePoor&MaterialPoor"=818.7023, 
                       "IncomePoor&WorkPoor"=2609.9461, 
                       "MaterialPoor&WorkPoor"=369.0661,
                       "IncomePoor&MaterialPoor&WorkPoor"=1702.7241))

vd.18.24 <- venneuler(c(IncomePoor=111288.123, MaterialPoor=37680.257, WorkPoor=97213.403, 
                        "IncomePoor&MaterialPoor"=5662.0519, 
                        "IncomePoor&WorkPoor"=55576.8394, 
                        "MaterialPoor&WorkPoor"=4332.5899,
                        "IncomePoor&MaterialPoor&WorkPoor"=15366.3741))

vd.25.54 <- venneuler(c(IncomePoor=199558.997, MaterialPoor=117797.814, WorkPoor=197316.503, 
                        "IncomePoor&MaterialPoor"=18414.3580, 
                        "IncomePoor&WorkPoor"=75017.6260, 
                        "MaterialPoor&WorkPoor"=22703.1381,
                        "IncomePoor&MaterialPoor&WorkPoor"=30743.3087))

vd.55.64 <- venneuler(c(IncomePoor=45249.634, MaterialPoor=15915.208, WorkPoor=73595.183, 
                        "IncomePoor&MaterialPoor"=2704.9640, 
                        "IncomePoor&WorkPoor"=23943.4609, 
                        "MaterialPoor&WorkPoor"=1879.5682,
                        "IncomePoor&MaterialPoor&WorkPoor"=6533.0358))




### Päällekkäisyyksistä tolpat
t <- data.frame(prop.table(svytable(~ageclass+multidim2, d.df),1)*100)
df2 <- subset(t, Freq > 0.01)
ggplot(data=df2, aes(x=ageclass, y=Freq)) + 
  geom_bar(stat="identity") + coord_flip() +
  facet_wrap(~multidim2)


# maittain
vd.fi <- venneuler(c(IncomePoor=12.24, MaterialPoor=5.91, WorkPoor=12.69, 
                     "IncomePoor&MaterialPoor"=0.93, 
                     "IncomePoor&WorkPoor"=5.23, 
                     "MaterialPoor&WorkPoor"=0.99,
                     "IncomePoor&MaterialPoor&WorkPoor"=1.83))
plot(vd.fi)
vd.ro <- venneuler(c(IncomePoor=22.44, MaterialPoor=28.36, WorkPoor=16.29, 
                     "IncomePoor&MaterialPoor"=7.44, 
                     "IncomePoor&WorkPoor"=3.83, 
                     "MaterialPoor&WorkPoor"=2.06,
                     "IncomePoor&MaterialPoor&WorkPoor"=5.01))
plot(vd.ro)
vd.fi <- venneuler(c(IncomePoor=10.2, MaterialPoor=3.7, WorkPoor=9.5, 
                     "IncomePoor&MaterialPoor"=0.7, 
                     "IncomePoor&WorkPoor"=3.4, 
                     "MaterialPoor&WorkPoor"=0.5,
                     "IncomePoor&MaterialPoor&WorkPoor"=1.1))


### Tolpat
ggplot(data=df2, aes(x=multidim2, y=Freq)) + 
  geom_bar(stat="identity") + coord_flip()
############################################################################






