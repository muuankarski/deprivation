# PAKETIT
library(foreign)
library(reshape)
library(car)
library(ggplot2)
library(survey)
#############################################################
# DATOJEN LATAUS JA YHDISTÄMINEN
#############################################################
# HOUSEHOLD
#############################
## household_reg
#hh_reg <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2008/rev1/d_file.csv", header=TRUE)
#hh_reg <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2008/cross_rev4/d_file.csv", header=TRUE)
#hh_reg <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2009/cross_rev2/d_file.csv", header=TRUE)
hh_reg <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2010/cross_rev0/d_file.csv", header=TRUE)
#hh_reg <- subset(hh_reg, DB020 %in% c("RO","PL","DE","UK","IT","DK"))
## household_data
#hh_data <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2008/rev1/h_file.csv", header=TRUE)
#hh_data <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2008/cross_rev4/h_file.csv", header=TRUE)
#hh_data <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2009/cross_rev2/h_file.csv", header=TRUE)
hh_data <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2010/cross_rev0/h_file.csv", header=TRUE)
#hh_data <- subset(hh_data, HB020 %in% c("RO","PL","DE","UK","IT","DK"))
## yhdistetään datat maan (DB020) ja hh-id:n (DB020) perusteella 
## nimetään uudelleen hh_damuutentasta ko muuttujat
hh_data <- rename(hh_data, c(HB020="DB020", HB030="DB030"))
## merge
hh_merge <- merge(hh_reg,hh_data,by=c("DB020","DB030"))
#############################
# PERSONAL
#############################
## personal_reg
#per_reg <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2008/rev1/r_file.csv", header=TRUE)
#per_reg <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2008/cross_rev4/r_file.csv", header=TRUE)
#per_reg <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2009/cross_rev2/r_file.csv", header=TRUE)
per_reg <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2010/cross_rev0/r_file.csv", header=TRUE)
#per_reg <- subset(per_reg, RB020 %in% c("RO","PL","DE","UK","IT","DK"))
## personal_data
#per_data <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2008/rev1/p_file.csv", header=TRUE)
#per_data <- read.csv("/media/EOS_DIGITAL/Asiakirjat/data/eu_silc/2008/cross_rev4/p_file.csv", header=TRUE)
#per_data <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2009/cross_rev2/p_file.csv", header=TRUE)
per_data <- read.csv("/media/baikal/Asiakirjat/data/eu_silc/2010/cross_rev0/p_file.csv", header=TRUE)
#per_data <- subset(per_data, PB020 %in% c("RO","PL","DE","UK","IT","DK"))
# yhdistetään datat maan (DB020) ja per-id:n (DB020) perusteella 
## nimetään uudelleen per_datasta ko muuttujat
per_data <- rename(per_data, c(PB020="RB020", PB030="RB030"))
## merge ???????
per_merge <- merge(per_reg,per_data,by=c("RB020","RB030"))
#############################################################
# VALITAAN KÄYTETTÄVÄT MUUTTUJAT
#############################################################
# HOUSEHOLD MERGE
#############################
vars_hh_merge <- c("DB020","DB030", # Year, Country
                   "DB090","HX050", # weight, equivalent hh size
                   "HS010","HS011","HS020","HS021","HS030","HS031", # material deprivation
                   "HH050","HS050","HS040","HS060", # material deprivation
                   "HS110","HS080","HS100","HS070", # material deprivation
                   "HS010_F","HS011_F","HS020_F","HS021_F","HS030_F","HS031_F",
                   "HH050_F","HS050_F","HS040_F","HS060_F",
                   "HS110_F","HS080_F","HS100_F","HS070_F",
                   "HY020","HY022","HY023","HY040N", # INCOME
                   "HY050N","HY070N", # INCOME
                   "HY080N","HY090N", # INCOME
                   "HY110N","HY130N", # INCOME
                   "HY145N", # INCOME
                   "HX090", # equivalent hh income
                   "HY050G","HY060G","HY070G","HY080G") # social benefits
hh_merge_mod <- hh_merge[vars_hh_merge]                    
# PERSONAL MERGE
#############################
vars_per_merge <- c("RB010","RB020", # YEAR, COUNTRY
                    "RB030","PX030", # Personal ID , CURRENT HOUSEHOLD ID
                    "RB050","PB040","PB060", # weights 
                    "RB080","RB090", # year of birth, gender
                    "PH010", # health
                    "PY010N","PY050N","PY090N","PY100N", # incomes
                    "PY110N","PY120N","PY130N","PY140N", # incomes
                    "PL070","PL072","RB080",
                    "PY080G","PY090G","PY100G","PY110G") # social benefits
per_merge_mod <- per_merge[vars_per_merge]
#############################################################
# 2009 ->
#############################################################
vars_per_merge <- c("RB010","RB020", # YEAR, COUNTRY
                    "RB030","PX030", # Personal ID , CURRENT HOUSEHOLD ID
                    "RB050","PB040","PB060", # weights 
                    "RB080","RB090", # year of birth, gender
                    "PH010", # health
                    "PY010N","PY050N","PY090N","PY100N", # incomes
                    "PY110N","PY120N","PY130N","PY140N", # incomes
                    "PL210A","PL210B","PL210C","PL210D","PL210E","PL210F",
                    "PL210G","PL210H","PL210I","PL210J","PL210K","PL210L",
                    "PL211A","PL211B","PL211C","PL211D","PL211E","PL211F",
                    "PL211G","PL211H","PL211I","PL211J","PL211K","PL211L",
                    "PL211A_F","PL211B_F","PL211C_F","PL211D_F","PL211E_F","PL211F_F",
                    "PL211G_F","PL211H_F","PL211I_F","PL211J_F","PL211K_F","PL211L_F",
                    "pl070","pl072","PL073","PL074","PL075","PL076",
                    "PL080","PL085","PL086","PL087","PL088","PL089","PL090",
                    "RB080",
                    "PY080G","PY090G","PY100G","PY110G") # social benefits
per_merge_mod <- per_merge[vars_per_merge]

#############################################################
# YHDISTETÄÄN PER JA HH DATA
#############################################################
hh_merge_mod <- rename(hh_merge_mod, c(DB020="RB020", DB030="PX030"))

merge_mod <- merge(per_merge_mod,hh_merge_mod,by=c("RB020","PX030"), all.x=TRUE)

# DISPOSABLE INCOME
# AGE
merge_mod$age <- 2010 - merge_mod$RB080
## eqSS ## ei toimi, siis otetaan HX050 muuttuja hh datasta ko. muuttujaksi
## tehdään uniikki hh-id joka tyypille
# faktori
merge_mod$HH_ID <- paste(merge_mod$RB020,merge_mod$PX030, sep="_")
merge_mod$HH_ID <- factor(merge_mod$HH_ID)

merge_mod$PER_ID <- paste(merge_mod$RB020,merge_mod$RB030, sep="_")
merge_mod$PER_ID <- factor(merge_mod$PER_ID)

write.csv(merge_mod, file="data/merge_mod.csv")


#merge_mod10_rev0 <- merge_mod
#merge_mod09_rev2 <- merge_mod
#merge_mod08_rev1 <- merge_mod
#merge_mod08_rev4 <- merge_mod

rm(hh_reg)
rm(hh_data)
rm(hh_merge)
rm(hh_merge_mod)
rm(per_reg)
rm(per_data)
rm(per_merge)
rm(per_merge_mod)
