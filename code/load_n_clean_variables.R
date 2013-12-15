# PAKETIT
library(foreign)
library(reshape)
library(car)
library(ggplot2)
library(survey)
#############################################################
# DATOJEN LATAUS JA YHDISTÄMINEN
#############################################################
df2004 <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2004/cross_rev0/df2004.csv", header=TRUE)
#df2005 <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2005/cross_rev0/df2005.csv", header=TRUE)
df2008 <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2008/cross_rev4/df2008.csv", header=TRUE)
df2009 <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2009/cross_rev2/df2009.csv", header=TRUE)
df2010 <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2010/cross_rev0/df2010.csv", header=TRUE)

names(df2004)

## tehdään uniikki hh-id joka tyypille
# faktori
df2004$HH_ID_Y <- paste(df2004$RB010,df2004$HH_ID, sep="_")
df2004$PER_ID_Y <- paste(df2004$RB010,df2004$PER_ID, sep="_")

df2008$HH_ID_Y <- paste(df2008$RB010,df2008$HH_ID, sep="_")
df2008$PER_ID_Y <- paste(df2008$RB010,df2008$PER_ID, sep="_")

df2009$HH_ID_Y <- paste(df2009$RB010,df2009$HH_ID, sep="_")
df2009$PER_ID_Y <- paste(df2009$RB010,df2009$PER_ID, sep="_")

df2010$HH_ID_Y <- paste(df2010$RB010,df2010$HH_ID, sep="_")
df2010$PER_ID_Y <- paste(df2010$RB010,df2010$PER_ID, sep="_")



# AGE 2004
df2004$age <- 2004 - df2004$RB080
df2004$age_class[df2004$age < 25] <- '1) alle 24'
df2004$age_class[df2004$age >= 25 & df2004$age < 45] <- '2) 25-44'
df2004$age_class[df2004$age >= 45 & df2004$age < 63] <- '3) 45-62'
df2004$age_class[df2004$age >= 63] <- '4) yli 62'

# AGE 2005
df2005$age <- 2005 - df2005$RB080
df2005$age_class[df2005$age < 25] <- '1) alle 24'
df2005$age_class[df2005$age >= 25 & df2005$age < 45] <- '2) 25-44'
df2005$age_class[df2005$age >= 45 & df2005$age < 63] <- '3) 45-62'
df2005$age_class[df2005$age >= 63] <- '4) yli 62'

# AGE 2006
df2006$age <- 2006 - df2006$RB080
df2006$age_class[df2006$age < 25] <- '1) alle 24'
df2006$age_class[df2006$age >= 25 & df2006$age < 45] <- '2) 25-44'
df2006$age_class[df2006$age >= 45 & df2006$age < 63] <- '3) 45-62'
df2006$age_class[df2006$age >= 63] <- '4) yli 62'

# AGE 2007
df2007$age <- 2007 - df2007$RB080
df2007$age_class[df2007$age < 25] <- '1) alle 24'
df2007$age_class[df2007$age >= 25 & df2007$age < 45] <- '2) 25-44'
df2007$age_class[df2007$age >= 45 & df2007$age < 63] <- '3) 45-62'
df2007$age_class[df2007$age >= 63] <- '4) yli 62'

# AGE 2008
df2008$age <- 2008 - df2008$RB080
df2008$age_class[df2008$age < 25] <- '1) alle 24'
df2008$age_class[df2008$age >= 25 & df2008$age < 45] <- '2) 25-44'
df2008$age_class[df2008$age >= 45 & df2008$age < 63] <- '3) 45-62'
df2008$age_class[df2008$age >= 63] <- '4) yli 62'

# AGE 2009
df2009$age <- 2009 - df2009$RB080
df2009$age_class[df2009$age < 25] <- '1) alle 24'
df2009$age_class[df2009$age >= 25 & df2009$age < 45] <- '2) 25-44'
df2009$age_class[df2009$age >= 45 & df2009$age < 63] <- '3) 45-62'
df2009$age_class[df2009$age >= 63] <- '4) yli 62'


# AGE 2010
df2010$age <- 2010 - df2010$RB080
df2010$age_class[df2010$age < 25] <- '1) alle 24'
df2010$age_class[df2010$age >= 25 & df2010$age < 45] <- '2) 25-44'
df2010$age_class[df2010$age >= 45 & df2010$age < 63] <- '3) 45-62'
df2010$age_class[df2010$age >= 63] <- '4) yli 62'

