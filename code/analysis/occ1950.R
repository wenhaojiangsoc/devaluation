library(dplyr)
library(lfe)
library(dpm)
library(plm)
library(ggplot2)
library(haven)
library(labelled)
library(expss)

## read data
setwd('devaluation-main/data/census and merged')
ma <- read_dta("occ1950.dta") ## this is the IPUMS data you should download following the xml file descriptions

## keep working age population 25-64
ma <- ma[which(ma$age >= 25 & ma$age <= 64), ]

## keep those who are economically active (incwage!=0)
ma[which(ma$incwage>=999998),"incwage"] <- NA
ma <- ma %>%
  filter((incwage>0&!is.na(incwage)&year>=1940) |
           (year<1940))

## New variable: observations counts
ma <- ma %>% dplyr::group_by(year, occ1950) %>% dplyr::mutate(count = sum(perwt, na.rm=T))

## New Variable: mean age
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_age = weighted.mean(age, w=perwt, na.rm=T))

## New variable: percent female
ma <- ma %>% group_by(year, occ1950) %>% mutate(percent_f = weighted.mean(sex-1, w=perwt, na.rm=T))

## new variable: race
ma <- ma %>% rename(race_2 = race)
ma[which(ma$race_2 == 1 & ma$hispan == 0), "race"] <- 0
ma[which(ma$race_2 == 2 & ma$hispan == 0), "race"] <- 1
ma[which(ma$race_2 == 3 & ma$hispan == 0), "race"] <- 2
ma[which((ma$race_2 == 4 | ma$race_2 == 5 | ma$race_2 == 6) & ma$hispan == 0), "race"] <- 3
ma[which(ma$hispan == 1 | ma$hispan == 2 | ma$hispan == 3 | ma$hispan == 4), "race"] <- 4
ma[which(is.na(ma$race)), "race"] <- 5
ma <- ma %>% dplyr::select(-c(race_2, hispan))

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
ma[sapply(ma, is.infinite)] <- NA

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

#### group-wise variables ####

## wages
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_wage = weighted.mean(rwage_h, w=perwt, na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_wage_men = weighted.mean(rwage_h[sex==1], w=perwt[sex==1], na.rm=T)) %>% ungroup()
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_wage_women = weighted.mean(rwage_h[sex==2], w=perwt[sex==2], na.rm=T)) %>% ungroup()

## education mean
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_edu = weighted.mean(eduyear, w=perwt, na.rm=T)) %>% ungroup()

## years of experience
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_lmexp = weighted.mean(lmexp, w=perwt, na.rm=T)) %>% ungroup()

## hours of work
ma <- ma %>% group_by(year, occ1950) %>% mutate(mean_hours = weighted.mean(hours, w=perwt, na.rm=T)) %>% ungroup()

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
            presgl = max(presgl,na.rm=T),
            prent90 = max(prent,na.rm=T),
            earnscore = max(erscor50,na.rm=T),
            eduscore = max(edscor50,na.rm=T),
            npboss = max(npboss50,na.rm=T),
            mean_wage = max(mean_wage,na.rm=T),
            mean_wage_men = max(mean_wage_men,na.rm=T),
            mean_wage_women = max(mean_wage_women,na.rm=T),
            mean_edu = max(mean_edu,na.rm=T), 
            mean_lmexp = max(mean_lmexp,na.rm=T), 
            mean_hours = max(mean_hours,na.rm=T)
            )

## replace infinite
occ1950[sapply(occ1950, is.infinite)] <- NA

## returns to years of education
library(broom)
returns_to_education <- ma %>%
  group_by(occ1950, year) %>%
  filter(sum(!is.na(eduyear) & !is.na(lmexp) & !is.na(hours) &
               !is.na(race_1) & !is.na(race_2) & !is.na(race_3) &
               !is.na(race_4) & !is.na(race_5) & !is.na(sex) & !is.na(wage_h))>2) %>%
  do(tidy(lm(wage_h ~ eduyear + lmexp + I(lmexp^2) + sex + hours + race_1 + race_2 + race_3 + race_4 + race_5, data = .))) %>%
  filter(term == "eduyear") %>%
  select(occ1950, year, estimate)
occ1950 <- merge(occ1950, returns_to_education,by=c("occ1950","year"),all.x=T)

## create growth/decline of the job
occ1950 <-
  occ1950 %>%
  filter(occ1950<979) %>%
  group_by(year) %>%
  mutate(total = sum(count)) %>%
  mutate(proportion = count/total) %>%
  group_by(occ1950) %>%
  mutate(proportion_lag = dplyr::lag(proportion,1),
         change_prop = (proportion - proportion_lag)/proportion_lag)

## read skill data
dot1965 <- read_dta("~/Library/CloudStorage/Dropbox/Devaluation Word Embeddings/Data/Misc./DoT1965/DoT1965_OCC1960.dta") %>% mutate(year=1960)
dot1977 <- read_dta("~/Library/CloudStorage/Dropbox/Devaluation Word Embeddings/Data/Misc./DoT1977/DS0001/DoT1977_OCC1960.dta") %>% 
  mutate(year=1970) %>% rename(motor=moto)
dot1991 <- read_dta("~/Library/CloudStorage/Dropbox/Devaluation Word Embeddings/Data/Misc./DoT1991/DoT1991_OCC1960.dta") %>% mutate(year=1990)
dotskill <- rbind(dot1965,dot1977,dot1991)

## read occ1960 to occ1950 crosswalk
cross <- read_dta("~/Library/CloudStorage/Dropbox/Devaluation Word Embeddings/Data/Misc./occ1960_1950.dta")
cross <-
  cross %>%
  group_by(occ, occ1950) %>%
  summarize(count = sum(perwt))
cross <- 
  cross %>%
  group_by(occ) %>%
  arrange(desc(count)) %>%
  filter(row_number()==1)
cross <- cross %>%
  select(-count) %>%
  rename(OCC1960=occ)
dotskill <- merge(dotskill,cross,by="OCC1960",all.x=T)
dotskill <- dotskill %>% filter(!is.na(occ1950))
occ1950 <- merge(occ1950,dotskill %>% select(-OCC1960),by=c("occ1950","year"),all.x=T)

## save output
write.csv(occ1950,"devaluation-main/data/census and merged/occ1950.csv",row.names = FALSE)
