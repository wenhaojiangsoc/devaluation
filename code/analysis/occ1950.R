library(dplyr)
library(lfe)
library(dpm)
library(plm)
library(ggplot2)
library(haven)
library(labelled)
library(expss)

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main')
ma <- read_dta("occ1950.dta")

## keep working age population 18-65
ma <- ma[which(ma$age >= 18 & ma$age <= 65), ]

## New variable: observations counts
ma <- ma %>% dplyr::group_by(year, occ1950) %>% dplyr::mutate(count = sum(perwt, na.rm=T))

## New variable: percent female
ma <- ma %>% group_by(year, occ1950) %>% mutate(percent_f = weighted.mean(sex - 1, w=dplyr::coalesce(perwt,0), na.rm=T))

## new variable: race
ma <- ma %>% rename(race_2 = race)
ma[which(ma$race_2 == 1 & ma$hispan == 0), "race"] <- 0
ma[which(ma$race_2 == 2 & ma$hispan == 0), "race"] <- 1
ma[which(ma$race_2 == 3 & ma$hispan == 0), "race"] <- 2
ma[which((ma$race_2 == 4 | ma$race_2 == 5 | ma$race_2 == 6) & ma$hispan == 0), "race"] <- 3
ma[which(ma$hispan == 1 | ma$hispan == 2 | ma$hispan == 3 | ma$hispan == 4), "race"] <- 4
ma[which(is.na(ma$race)), "race"] <- 5
ma <- ma %>% dplyr::select(-c(race_2, hispan))

## top-truncate incwage to be less than 800000
ma[which(ma$incwage>=8e+5),"incwage"] <- NA

## new variable: continuous work hours per week
ma[which(ma$hrswork2 == 1 & ma$year < 1980), "hours"] <- 7.5
ma[which(ma$hrswork2 == 2 & ma$year < 1980), "hours"] <- 22
ma[which(ma$hrswork2 == 3 & ma$year < 1980), "hours"] <- 32
ma[which(ma$hrswork2 == 4 & ma$year < 1980), "hours"] <- 37
ma[which(ma$hrswork2 == 5 & ma$year < 1980), "hours"] <- 40
ma[which(ma$hrswork2 == 6 & ma$year < 1980), "hours"] <- 44.5
ma[which(ma$hrswork2 == 7 & ma$year < 1980), "hours"] <- 54
ma[which(ma$hrswork2 == 8 & ma$year < 1980), "hours"] <- 60
ma[which(ma$year >= 1980), "hours"] <- ma[which(ma$year >= 1980), "uhrswork"]

## new variable: continuous weeks
ma[which(!is.na(ma$wkswork1)), "weeks"] <- ma[which(!is.na(ma$wkswork1)), "wkswork1"]
ma[which(ma$wkswork2 == 1 & is.na(ma$weeks)), "weeks"] <- 7
ma[which(ma$wkswork2 == 2 & is.na(ma$weeks)), "weeks"] <- 20
ma[which(ma$wkswork2 == 3 & is.na(ma$weeks)), "weeks"] <- 33
ma[which(ma$wkswork2 == 4 & is.na(ma$weeks)), "weeks"] <- 43.5
ma[which(ma$wkswork2 == 5 & is.na(ma$weeks)), "weeks"] <- 48.5
ma[which(ma$wkswork2 == 6 & is.na(ma$weeks)), "weeks"] <- 51

## new variable: real wage (income per hour)
ma$hours_ann <- ma$weeks*ma$hours
ma$wage_h <- ma$incwage/ma$hours_ann  
ma$rwage_h <- ma$wage_h*ma$cpi99

## new variable: continuous years in education
ma[which(ma$educd == 0), "eduyear"] <- 0
ma[which(ma$educd == 1), "eduyear"] <- 0
ma[which(ma$educd == 2), "eduyear"] <- 0
ma[which(ma$educd == 10), "eduyear"] <- 4 
ma[which(ma$educd == 11), "eduyear"] <- 0
ma[which(ma$educd == 12), "eduyear"] <- 0
ma[which(ma$educd == 13), "eduyear"] <- 2.5  
ma[which(ma$educd == 14), "eduyear"] <- 1
ma[which(ma$educd == 15), "eduyear"] <- 2
ma[which(ma$educd == 16), "eduyear"] <- 3
ma[which(ma$educd == 17), "eduyear"] <- 4
ma[which(ma$educd == 20), "eduyear"] <- 6.5
ma[which(ma$educd == 21), "eduyear"] <- 5.5
ma[which(ma$educd == 22), "eduyear"] <- 5
ma[which(ma$educd == 23), "eduyear"] <- 6
ma[which(ma$educd == 24), "eduyear"] <- 7.5
ma[which(ma$educd == 25), "eduyear"] <- 7
ma[which(ma$educd == 26), "eduyear"] <- 8
ma[which(ma$educd == 30), "eduyear"] <- 9
ma[which(ma$educd == 40), "eduyear"] <- 10
ma[which(ma$educd == 50), "eduyear"] <- 11
ma[which(ma$educd <= 65 & ma$educd >= 60), "eduyear"] <- 12
ma[which(ma$educd <= 71 & ma$educd >= 70), "eduyear"] <- 13
ma[which(ma$educd <= 83 & ma$educd >= 80), "eduyear"] <- 14
ma[which(ma$educd == 90), "eduyear"] <- 15
ma[which(ma$educd == 100), "eduyear"] <- 16
ma[which(ma$educd == 101), "eduyear"] <- 16
ma[which(ma$educd == 110), "eduyear"] <- 17
ma[which(ma$educd == 111), "eduyear"] <- 18
ma[which(ma$educd == 112), "eduyear"] <- 19
ma[which(ma$educd == 113), "eduyear"] <- 20
ma[which(ma$educd == 114), "eduyear"] <- 17
ma[which(ma$educd == 115), "eduyear"] <- 19
ma[which(ma$educd == 116), "eduyear"] <- 20

## new variable: potential labor market experience
ma$lmexp <- ma$age - ma$eduyear - 6  

## wages
ma <- ma %>% group_by(year, occ1950) %>% mutate(wage_male = median(rwage_h[sex == 1],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(wage_fem = median(rwage_h[sex == 2],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(wage = median(rwage_h,na.rm=T)) %>% ungroup()

## education mean
ma <- ma %>% group_by(year, occ1950) %>% mutate(edu_male = mean(eduyear[sex == 1],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(edu_fem = mean(eduyear[sex == 2],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(edu = mean(eduyear,na.rm=T)) %>% ungroup()

## years of experience
ma <- ma %>% group_by(year, occ1950) %>% mutate(lmexp_male = mean(lmexp[sex == 1],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(lmexp_fem = mean(lmexp[sex == 2],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(lmexp = mean(lmexp,na.rm=T)) %>% ungroup()

## hours of work
ma <- ma %>% group_by(year, occ1950) %>% mutate(hours_male = mean(hours[sex == 1],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(hours_fem = mean(hours[sex == 2],na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(hours = mean(hours,na.rm=T)) %>% ungroup()

## race
library(fastDummies)
ma$race <- unclass(ma$race)
ma <- fastDummies::dummy_cols(ma, select_columns = "race")

## group by and summarize
occ1950 <-
  ma %>%
  group_by(year, occ1950) %>%
  summarize(count = max(count,na.rm=T),
            percent_f = max(percent_f,na.rm=T),
            race_0 = mean(race_0,na.rm=T),
            race_1 = mean(race_1,na.rm=T), 
            race_2 = mean(race_2,na.rm=T), 
            race_3 = mean(race_3,na.rm=T),
            race_4 = mean(race_4,na.rm=T), 
            race_5 = mean(race_5,na.rm=T),
            incscore = max(occscore,na.rm=T),
            duncansei = max(sei,na.rm=T),
            hwsei90 = max(hwsei,na.rm=T),
            presgl = max(presgl,na.rm=T),
            prent90 = max(prent,na.rm=T),
            earnscore = max(erscor50,na.rm=T),
            eduscore = max(edscor50,na.rm=T),
            npboss = max(npboss50,na.rm=T),
            wage_male = max(wage_male,na.rm=T), 
            wage_fem = max(wage_fem,na.rm=T),
            wage = max(wage,na.rm=T),
            edu_male = max(edu_male,na.rm=T),
            edu_fem = max(edu_fem,na.rm=T), 
            edu = max(edu,na.rm=T), 
            lmexp_male = max(lmexp_male,na.rm=T), 
            lmexp_fem = max(lmexp_fem,na.rm=T),  
            lmexp = max(lmexp,na.rm=T), 
            hours_male = max(hours_male,na.rm=T),
            hours_fem = max(hours_fem,na.rm=T),
            hours = max(hours,na.rm=T)
            )

## replace infinite
occ1950[sapply(occ1950, is.infinite)] <- NA
occ1950[which(occ1950$edu_fem==0),"edu_fem"] <- NA

write.csv(occ1950,"/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main/occ1950.csv",row.names = FALSE)
