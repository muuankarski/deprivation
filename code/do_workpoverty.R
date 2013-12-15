# Tuloköyhyys ja deprivaatio-indikaattorit löytyy jo valmiina
# -> tarvitaan matalan työintensiteetin mittari
#############################################################
# muotoilu on seuraava 
## Persons are defined as living in households with very low work intensity
## if they are aged 0-59 and the working age members in the household
## worked less than 20 % of their potential during the past year.

#df <- subset(merge_mod, select=c("HH_ID","PER_ID","PL070","PL072","RB020","RB080"))

df <- subset(merge_mod, select=c("HH_ID","PER_ID","RB050",
                                 "PL210A","PL210B","PL210C","PL210D","PL210E","PL210F",
                                 "PL210G","PL210H","PL210I","PL210J","PL210K","PL210L",
                                 "PL211A","PL211B","PL211C","PL211D","PL211E","PL211F",
                                 "PL211G","PL211H","PL211I","PL211J","PL211K","PL211L",
                                 "PL211A_F","PL211B_F","PL211C_F","PL211D_F","PL211E_F","PL211F_F",
                                 "PL211G_F","PL211H_F","PL211I_F","PL211J_F","PL211K_F","PL211L_F",
                                 "pl070","pl072","PL073","PL074","PL075","PL076",
                                 "RB020","RB080"))


df <- subset(df, RB020 %in% c("DK","RO",
                              "PL","DE",
                              "IT","UK"))

df$age <- 2010 - df$RB080 # 2008
#df$age <- 2004 - df$RB080 # 2004
df <- subset(df, age < 60)
df <- subset(df, age > 17)

########################################
library(VIM)
df$RB020 <- factor(df$RB020)
par(mfrow=c(2,2))
spineMiss(df[, c("RB020", "PL210C")])
spineMiss(df[, c("RB020", "PL210D")])
spineMiss(df[, c("RB020", "PL211C")])
spineMiss(df[, c("RB020", "PL211D")])
########################################
# Excluded households
## students 18- 24
df$student[df$age < 25 & df$PL211A == 6] <- "student"
df$student[df$age < 25 & df$PL211B == 6] <- "student"
df$student[df$age < 25 & df$PL211C == 6] <- "student"
df$student[df$age < 25 & df$PL211D == 6] <- "student"
df$student[df$age < 25 & df$PL211E == 6] <- "student"
df$student[df$age < 25 & df$PL211F == 6] <- "student"
df$student[df$age < 25 & df$PL211G == 6] <- "student"
df$student[df$age < 25 & df$PL211H == 6] <- "student"
df$student[df$age < 25 & df$PL211I == 6] <- "student"
df$student[df$age < 25 & df$PL211J == 6] <- "student"
df$student[df$age < 25 & df$PL211K == 6] <- "student"
df$student[df$age < 25 & df$PL211L == 6] <- "student"
df$student <- recode(df$student, "'student'='student';
                     else='non-student'")
summary(df$student)
df <- subset(df, student %in% 'non-student')
########################################
########################################
########################################
df$PL211A2[df$PL211A == 1] <- 1
df$PL211A2[df$PL211A == 3] <- 1
df$PL211A2[df$PL211A == 2] <- 0.5
df$PL211A2[df$PL211A == 4] <- 0.5
df$PL211A2[df$PL211A >= 5] <- 0
df$PL211A2[df$PL211A < 1] <- 0
df$PL211A2[df$PL211A == NA] <- 1

#
df$PL211B2[df$PL211B == 1] <- 1
df$PL211B2[df$PL211B == 3] <- 1
df$PL211B2[df$PL211B == 2] <- 0.5
df$PL211B2[df$PL211B == 4] <- 0.5
df$PL211B2[df$PL211B >= 5] <- 0
df$PL211B2[df$PL211B < 1] <- 0
df$PL211B2[df$PL211B == NA] <- 1
#
df$PL211C2[df$PL211C == 1] <- 1
df$PL211C2[df$PL211C == 3] <- 1
df$PL211C2[df$PL211C == 2] <- 0.5
df$PL211C2[df$PL211C == 4] <- 0.5
df$PL211C2[df$PL211C >= 5] <- 0
df$PL211C2[df$PL211C < 1] <- 0
df$PL211C2[df$PL211C == NA] <- 1
#
df$PL211D2[df$PL211D == 1] <- 1
df$PL211D2[df$PL211D == 3] <- 1
df$PL211D2[df$PL211D == 2] <- 0.5
df$PL211D2[df$PL211D == 4] <- 0.5
df$PL211D2[df$PL211D >= 5] <- 0
df$PL211D2[df$PL211D < 1] <- 0
df$PL211D2[df$PL211D == NA] <- 1
#
df$PL211E2[df$PL211E == 1] <- 1
df$PL211E2[df$PL211E == 3] <- 1
df$PL211E2[df$PL211E == 2] <- 0.5
df$PL211E2[df$PL211E == 4] <- 0.5
df$PL211E2[df$PL211E >= 5] <- 0
df$PL211E2[df$PL211E < 1] <- 0
df$PL211E2[df$PL211E == NA] <- 1
#
df$PL211F2[df$PL211F == 1] <- 1
df$PL211F2[df$PL211F == 3] <- 1
df$PL211F2[df$PL211F == 2] <- 0.5
df$PL211F2[df$PL211F == 4] <- 0.5
df$PL211F2[df$PL211F >= 5] <- 0
df$PL211F2[df$PL211F < 1] <- 0
df$PL211F2[df$PL211F == NA] <- 1
#
df$PL211G2[df$PL211G == 1] <- 1
df$PL211G2[df$PL211G == 3] <- 1
df$PL211G2[df$PL211G == 2] <- 0.5
df$PL211G2[df$PL211G == 4] <- 0.5
df$PL211G2[df$PL211G >= 5] <- 0
df$PL211G2[df$PL211G < 1] <- 0
df$PL211LG[df$PL211G == NA] <- 1
#
df$PL211H2[df$PL211H == 1] <- 1
df$PL211H2[df$PL211H == 3] <- 1
df$PL211H2[df$PL211H == 2] <- 0.5
df$PL211H2[df$PL211H == 4] <- 0.5
df$PL211H2[df$PL211H >= 5] <- 0
df$PL211H2[df$PL211H < 1] <- 0
df$PL211H2[df$PL211H == NA] <- 1
#
df$PL211I2[df$PL211I == 1] <- 1
df$PL211I2[df$PL211I == 3] <- 1
df$PL211I2[df$PL211I == 2] <- 0.5
df$PL211I2[df$PL211I == 4] <- 0.5
df$PL211I2[df$PL211I >= 5] <- 0
df$PL211I2[df$PL211I < 1] <- 0
df$PL211I2[df$PL211I == NA] <- 1
#
df$PL211J2[df$PL211J == 1] <- 1
df$PL211J2[df$PL211J == 3] <- 1
df$PL211J2[df$PL211J == 2] <- 0.5
df$PL211J2[df$PL211J == 4] <- 0.5
df$PL211J2[df$PL211J >= 5] <- 0
df$PL211J2[df$PL211J < 1] <- 0
df$PL211J2[df$PL211J == NA] <- 1
#
df$PL211K2[df$PL211K == 1] <- 1
df$PL211K2[df$PL211K == 3] <- 1
df$PL211K2[df$PL211K == 2] <- 0.5
df$PL211K2[df$PL211K == 4] <- 0.5
df$PL211K2[df$PL211K >= 5] <- 0
df$PL211K2[df$PL211K < 1] <- 0
df$PL211K2[df$PL211K == NA] <- 1
#
df$PL211L2[df$PL211L == 1] <- 1
df$PL211L2[df$PL211L == 3] <- 1
df$PL211L2[df$PL211L == 2] <- 0.5
df$PL211L2[df$PL211L == 4] <- 0.5
df$PL211L2[df$PL211L >= 5] <- 0
df$PL211L2[df$PL211L < 1] <- 0
df$PL211L2[df$PL211L == NA] <- 1
########################################
df$PL211_sum = df$PL211A2+df$PL211B2+df$PL211C2+df$PL211D2+
  df$PL211E2+df$PL211F2+df$PL211G2+df$PL211H2+
  df$PL211I2+df$PL211J2+df$PL211K2+df$PL211L2
summary(df$PL211_sum)

df$PL211_sum <- recode(df$PL211_sum, "NA=12")

########################################
table(df$RB020,df$PL211_sum)
# kumulatiiviset kuukaudet

df$cum_months <- ave(df$PL211_sum, df$HH_ID, FUN = cumsum) #PL211

## lasketaan working age -tyyppien määrä kotitalouksittain "köyhyysrajaa"
## varten
df$count <- recode(df$RB020, "else=1")
df$count <- as.numeric(df$count)
df$cum_counts <- ave(df$count, df$HH_ID, FUN = cumsum)
head(df)
# valitaan suurimman "countin" tapaukset edustamaan HH:ta
sel <- ave(df$cum_counts, 
           df$HH_ID, FUN = max) == df$cum_counts
df.uniq <- df[sel,]
# tehdään avainmuuttujista nyt hh-tason data
df.hh <- subset(df.uniq, select=c("HH_ID","PER_ID","cum_months","cum_counts","RB050"))
# lasketaan work intensity köyhyysraja hh:lle
head(df.hh)
# raja on siis 20% ja se on yhden henkilön osalta 3.6 kk
## eka siis pitää laskea keskimääräinen työssäkäynti eli kumulatiiviset
# kuukauden jaettuna työikäisten määrällä.
df.hh$avg_months <- df.hh$cum_months/df.hh$cum_counts
qplot(df.hh$avg_months)
# tehdään köyhyysmuuttuja 
df.hh$work_poor[df.hh$avg_months < 2.4] <- "work.poor"
df.hh$work_poor[df.hh$avg_months >= 2.4] <- "work.non.poor"
df.hh$work_poor <- factor(df.hh$work_poor)
# alternative
df.hh$theomax <- df.hh$cum_counts * 12
df.hh$ratio <- df.hh$cum_months / df.hh$theomax
df.hh$work_poor2[df.hh$ratio < 0.2] <- "work.poor"
df.hh$work_poor2[df.hh$ratio >= 0.2] <- "work.non.poor"
df.hh$work_poor2 <- factor(df.hh$work_poor2)

df.wp <- join(dfx,df.hh,by="HH_ID")



#df.wp$work_poor <- recode(df.wp$work_poor, "NA='work.non.poor'")


### tyesting
df.md <- join(df.ip,df.wp,by="PER_ID")
df.md <- join(df.md,df.mp,by="PER_ID")

head(df.md)


summary(df.md)

df.venn <- subset(df.md, select=c("PER_ID","HH_ID","RB020",
                                  "age","work_poor",
                                  "income_poor",
                                  "material_poor",
                                  "RB050"))


df.venn$RB020 <- factor(df.venn$RB020)
d.df <- svydesign(id=~PER_ID, weights=~RB050, data=df.venn)
wp <- data.frame(prop.table(svytable(~RB020+work_poor, d.df),1)*100)
wp
