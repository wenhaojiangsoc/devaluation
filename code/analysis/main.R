library(dplyr)
library(lfe)
library(ggplot2)
library(zoo)
library(ggpubr)
library(stargazer)
library(ggrepel)
library(stringr)
library(tidyverse)
library(lavaan)

#~#~#~#~#~#~#~#~#~#~#
##### Read Data #####
#~#~#~#~#~#~#~#~#~#~#

## read data
## set your directory in the following analyses
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main')
full_data <- data.frame()

## COCHA
data <- read.csv("COCHA.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data$corpus <- "COCHA"
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
full_data <- rbind(full_data,data)

## Ngram
data <- read.csv("ngram.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
data$corpus <- "ngram"
full_data <- rbind(full_data,data)

## merge with 1950 occupation official code
code <- read.csv("1950occcode.csv")
code$occ <- gsub('n.e.c.', 'nec', code$occ)
full_data$occupation_origin <- gsub('n.e.c.', 'nec', full_data$occupation_origin)
full_data <- merge(full_data, code, by.y="occ", by.x="occupation_origin", all.x = T)
rm(code)

## merge with 1950 occupation actual measures
occ1950 <- read.csv("occ1950.csv")
full_data <- merge(full_data,occ1950,
                   by.x=c("code","year"),
                   by.y=c("occ1950","year"),
                   all.x=T)
full_data[which(full_data$hwsei90 < -10000 | full_data$hwsei90 > 10000),"hwsei90"] <- NA
full_data[which(full_data$prent90 < -10000 | full_data$prent90 > 10000),"prent90"] <- NA

## create identifier
full_data <- full_data %>%
  filter(occupation_single!="retired") %>%
  group_by(occupation_origin) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  as.data.frame()

## drop duplicate
full_data <- full_data %>%
  distinct(year,code,corpus,.keep_all = T)

full_data[full_data$code<=99,"category"] <- "Professional, Technical"
full_data[full_data$code>=200 & full_data$code<=290,"category"] <- "Managers, Officials, and Proprietors"
full_data[full_data$code>=300 & full_data$code<=390,"category"] <- "Clerical and Kindred"
full_data[full_data$code>=400 & full_data$code<=490,"category"] <- "Sales Workers"
full_data[full_data$code>=500 & full_data$code<=595,"category"] <- "Craftsmen"
full_data[full_data$code>=600 & full_data$code<=690,"category"] <- "Operatives"
full_data[full_data$code>=700 & full_data$code<=790,"category"] <- "Service Workers"
full_data[full_data$code>=800 & full_data$code<=890,"category"] <- "Farm Laborers"
full_data[full_data$code>=900 & full_data$code<=970,"category"] <- "Laborers"

## drop non-civilian jobs
full_data <-
  full_data %>%
  filter(code!=595&code!=987&code!=984&code!=982)

## count
full_data <-
  full_data %>%
  group_by(id) %>%
  mutate(count=mean(count,na.rm=T))

## summarize the frequency
full_data %>%
  group_by(corpus,id) %>%
  summarize(times = sum(!is.na(gender))) %>%
  summarize(times = mean(times))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
##### Cultural Feminization - Figure 5 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## create wage quartiles
full_data <- full_data %>%
  group_by(corpus,code) %>%
  fill(mean_wage, .direction = "updown") %>%
  mutate(wage = mean(
    mean_wage[year>=1990],na.rm=T)) %>%
  ungroup() %>%
  group_by(corpus) %>%
  mutate(wage_quartile = case_when(wage < quantile(wage, seq(0,1,0.2), na.rm=T)[2] ~ 1,
                                   (wage < quantile(wage, seq(0,1,0.2), na.rm=T)[3]) &
                                     (wage >= quantile(wage, seq(0,1,0.2), na.rm=T)[2]) ~ 2,
                                   (wage < quantile(wage, seq(0,1,0.2), na.rm=T)[4]) &
                                     (wage >= quantile(wage, seq(0,1,0.2), na.rm=T)[3]) ~ 3,
                                   (wage < quantile(wage, seq(0,1,0.2), na.rm=T)[5]) &
                                     (wage >= quantile(wage, seq(0,1,0.2), na.rm=T)[4]) ~ 4,
                                   (wage <= quantile(wage, seq(0,1,0.2), na.rm=T)[6]) &
                                     (wage >= quantile(wage, seq(0,1,0.2), na.rm=T)[5]) ~ 5)) %>%
  group_by(code) %>%
  arrange(year)

## plot
plot <- list()

for (l in 0:5){
  
  ## indicate panel index
  panel <- c("A","B","C","D","E","F")[l+1]
    
  ## combine the two datasets
  combine_fem <- data.frame(coef = rep(NA,5),
                            se = rep(NA,5),
                            quantile = seq(1,5))
  ## lag
  full_data <- full_data %>%
    group_by(corpus,code) %>%
    arrange(year) %>%
    mutate(lag_percent_f = dplyr::lag(percent_f,l))
  
  ## smooth the cultural measure
  smooth <- function(x) c(x[1], rollmean(x, 3), x[length(x)])
  full_data <-
    full_data %>%
    group_by(corpus,code) %>%
    arrange(year) %>%
    mutate(gender_smooth=smooth(gender))
  
  for (i in 1:max(full_data$wage_quartile,na.rm=T)) {
    
    if (i != 3) {
      ## save coefficient
      combine_fem[combine_fem$quantile==i,"coef"] <- 
        summary(
          felm(gender_smooth ~ lag_percent_f + factor(corpus)
               | id + year | 0 | id, full_data[which(full_data$wage_quartile==i), ],
               weights = full_data[which(full_data$wage_quartile==i), ]$count)
        )$coef[1,1]
      
      ## save SE
      combine_fem[combine_fem$quantile==i,"se"] <- 
        summary(
          felm(gender_smooth ~ lag_percent_f + factor(corpus)
               | id + year | 0 | id, full_data[which(full_data$wage_quartile==i), ],
               weights = full_data[which(full_data$wage_quartile==i), ]$count)
        )$coef[1,2]
    }
    
    if (i == 3) {
      ## save coefficient
      combine_fem[combine_fem$quantile==i,"coef"] <- 
        summary(
          felm(gender_smooth ~ lag_percent_f + factor(corpus)
               | id + year | 0 | 0, full_data[which(full_data$wage_quartile==i), ],
               weights = full_data[which(full_data$wage_quartile==i), ]$count)
        )$coef[1,1]
      
      ## save SE
      combine_fem[combine_fem$quantile==i,"se"] <- 
        summary(
          felm(gender_smooth ~ lag_percent_f + factor(corpus)
               | id + year | 0 | 0, full_data[which(full_data$wage_quartile==i), ],
               weights = full_data[which(full_data$wage_quartile==i), ]$count)
        )$coef[1,2]
    }
    
  }
  
  ## plot
  combine_fem <- combine_fem %>%
    mutate(change = case_when(coef-1.96*se>0 ~ "1",
                              .default = "0"))
  
  plot[[l+1]] <-
    ggplot(combine_fem,aes(x=quantile,y=coef,group=change)) +
    geom_point(aes(color=change),position=position_dodge(0.15),size=2.7)+
    geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=change), width=0,
                  position=position_dodge(0.2)) +
    geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
    scale_color_manual(values=c("blue3","red4")) +
    theme_minimal() +
    xlab("wage quintile") +
    ylab("coefficient") +
    ggtitle(paste0(panel, ": lag = ", as.character(l))) +
    theme(legend.position = "none",
          text=element_text(family="Times",size=12),
          plot.title=element_text(hjust=0.5,face="bold",size=12))
}

## drop residual var and data
rm(i,combine_fem,data,occ1950,l)

###### Visualize individual changes ######

change <- full_data %>%
  mutate(wage_quartile=case_when(wage_quartile<=2~1,
                                 wage_quartile<=4&wage_quartile>=3~2,
                                 wage_quartile==5~3)) %>%
  group_by(code,occupation_origin,occupation_single,category,wage_quartile,count) %>%
  summarize(gender_3=mean(gender[year==2010],na.rm=T), ## mean cultural measure of the two corpora
            gender_2=mean(gender[year==1970],na.rm=T),
            gender_1=mean(gender[year<=1950],na.rm=T),
            pfemale_3=mean(percent_f[year==2010],na.rm=T),
            pfemale_2=mean(percent_f[year==1960],na.rm=T), ## lag for 1 decade for better visualization; does not change the interpretation
            pfemale_1=mean(percent_f[year<=1950],na.rm=T)) %>%
  group_by(wage_quartile) %>%
  summarize(gender_1=weighted.mean(gender_1,count,na.rm=T),
            gender_2=weighted.mean(gender_2,count,na.rm=T),
            gender_3=weighted.mean(gender_3,count,na.rm=T),
            pfemale_1=weighted.mean(pfemale_1,count,na.rm=T),
            pfemale_2=weighted.mean(pfemale_2,count,na.rm=T),
            pfemale_3=weighted.mean(pfemale_3,count,na.rm=T),
            count=sum(count,na.rm=T)) %>%
  pivot_longer(
    cols = -c("wage_quartile","count"),
    names_to = c("category", "time"),
    names_sep = "_", 
    values_to = "value"
  ) %>%
  mutate(wage_quartile=case_when(
    wage_quartile==1 ~ "1&2",
    wage_quartile==2 ~ "3&4",
    wage_quartile==3 ~ "5"
  ))

## change in percent female
plot[[7]] <- change %>%
  group_by(wage_quartile,category) %>%
  filter(category=="pfemale") %>%
  mutate(value_end = dplyr::lead(value)) %>%
  ggplot(aes(x = value, xend = value_end, y = wage_quartile, yend = wage_quartile, 
             group = wage_quartile)) +
  geom_segment(arrow = arrow(type = "closed", length = unit(0.2, "cm"), ends = "last"),
               size = 0.5, color="blue3") +
  geom_vline(aes(xintercept=0.5),lty="dashed",color="grey40") +
  ggtitle("E: Actual Feminization") +
  labs(x = "share of women",
       y = "wage quintile") +
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Times",size=11.5),
        axis.text.x=element_text(hjust=0.5),
        axis.text.y=element_text(hjust=0.5),
        plot.title=element_text(hjust=0.5,face="bold",size=11.5))

## change in gender typing
plot[[8]] <- change %>%
  group_by(wage_quartile,category) %>%
  mutate(value_end = dplyr::lead(value)) %>%
  filter(category=="gender") %>%
  ggplot(aes(x = value, xend = value_end, y = wage_quartile, yend = wage_quartile, 
             group = wage_quartile)) +
  geom_segment(arrow = arrow(type = "closed", length = unit(0.2, "cm"), ends = "last"),
               size = 0.5, color="red4") +
  geom_vline(aes(xintercept=0),lty="dashed",color="grey40") +
  scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
  ggtitle("F: Cultural Feminization") +
  labs(x = "female typing",
       y = "wage quintile") +
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Times",size=12),
        axis.text.x=element_text(hjust=0.5),
        axis.text.y=element_text(hjust=0.5),
        plot.title=element_text(hjust=0.5,face="bold",size=12))

ggarrange(plot[[1]],plot[[2]],plot[[3]],
          plot[[4]],plot[[7]],plot[[8]],
          ncol = 3, nrow = 2)

## save figure 5 in the main paper
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/main/cultural_fem_arrow.tiff", 
       width = 17.5, height = 8.5, units = "cm", dpi = 300)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
##### Measures of Prestige - Table 1 and 2 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## verify prestige measure 
## I pooled year and survey prestige measures for a more compact presentation

prestige_data <- rbind(
  full_data %>% filter((year==1960)& 
                       (presgl>=0&prent90>=0)) %>%
  dplyr::group_by(corpus) %>%
  dplyr::select(corpus,presgl,evaluation,
                potency,activity,prestige,count,code,wage_quartile,
                earnscore,eduscore,npboss) %>%
  pivot_longer(
    cols = c(presgl),
    names_to = "year",
    values_to = "survey_prestige"
  ),
  full_data %>% filter((year==1990)& 
                         (presgl>0&prent90>0)) %>%
    dplyr::group_by(corpus) %>%
    dplyr::select(corpus,prent90,evaluation,
                  potency,activity,prestige,count,code,wage_quartile,
                  earnscore,eduscore,npboss) %>%
    pivot_longer(
      cols = c(prent90),
      names_to = "year",
      values_to = "survey_prestige"
    )
)

## respective model
model1 <-
  lm(survey_prestige/100 ~ prestige + factor(corpus) + factor(year), prestige_data,
     weight = count)
model2 <-
  lm(survey_prestige/100 ~ evaluation + factor(corpus) + factor(year), prestige_data,
     weight = count)
model3 <-
  lm(survey_prestige/100 ~ potency + factor(corpus) + factor(year), prestige_data,
     weight = count)
model4 <-
  lm(survey_prestige/100 ~ activity + factor(corpus) + factor(year), prestige_data,
     weight = count)
model5 <-
  lm(survey_prestige/100 ~ evaluation + potency + activity + factor(corpus) + factor(year), prestige_data,
     weight = count)
model6 <-
  lm(survey_prestige/100 ~ prestige + evaluation + potency + activity +
       factor(corpus) + factor(year), prestige_data,
     weight = count)

## Table 1: validation against survey prestige
stargazer(model1, model2, model3, model4, model5, model6,
          type = "text", 
          omit.stat = c("adj.rsq","ser","f"),
          omit = c("corpus","year","Constant"),
          dep.var.labels = "Survey Prestige")
rm(model1, model2, model3, model4, model5, model6)

## Table 2: respective R^2 over survey
## 1960
rsq_1960 <- c(
  summary(lm(survey_prestige/100 ~ prestige + evaluation + potency + activity,
           prestige_data %>% filter(year=="presgl" & corpus=="COCHA"),
           weight = count))$r.squared,
summary(lm(survey_prestige/100 ~ prestige + evaluation + potency + activity,
           prestige_data %>% filter(year=="presgl" & corpus=="ngram")))$r.squared,
summary(lm(survey_prestige/100 ~ earnscore,
           prestige_data %>% filter(year=="presgl" & corpus=="ngram"),
           weight = count))$r.squared,
summary(lm(survey_prestige/100 ~ eduscore,
           prestige_data %>% filter(year=="presgl" & corpus=="ngram"),
           weight = count))$r.squared,
summary(lm(survey_prestige/100 ~ npboss,
           prestige_data %>% filter(year=="presgl" & corpus=="ngram"),
           weight = count))$r.squared
)
library(weights)
corr_1960 <- rbind(
  t(c(NA,NA)),
  t(c(NA,NA)),
  t(wtd.cor(
  prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl","survey_prestige"]/100,
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl",]$earnscore,
  weight=prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl",]$count)[1:2]),
  t(wtd.cor(
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl","survey_prestige"]/100,
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl",]$eduscore,
    weight=prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl",]$count)[1:2]),
  t(wtd.cor(
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl","survey_prestige"]/100,
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl",]$npboss,
    weight=prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="presgl",]$count)[1:2])
)
## print results
cbind(corr_1960,rsq_1960)

## 1990
rsq_1990 <- c(
  summary(lm(survey_prestige/100 ~ prestige + evaluation + potency + activity,
             prestige_data %>% filter(year=="prent90" & corpus=="COCHA"),
             weight = count))$r.squared,
  summary(lm(survey_prestige/100 ~ prestige + evaluation + potency + activity,
             prestige_data %>% filter(year=="prent90" & corpus=="ngram"),
             weight = count))$r.squared,
  summary(lm(survey_prestige/100 ~ earnscore,
             prestige_data %>% filter(year=="prent90" & corpus=="ngram"),
             weight = count))$r.squared,
  summary(lm(survey_prestige/100 ~ eduscore,
             prestige_data %>% filter(year=="prent90" & corpus=="ngram"),
             weight = count))$r.squared,
  summary(lm(survey_prestige/100 ~ npboss,
             prestige_data %>% filter(year=="prent90" & corpus=="ngram"),
             weight = count))$r.squared
)
library(weights)
corr_1990 <- rbind(
  t(c(NA,NA)),
  t(c(NA,NA)),
  t(wtd.cor(
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90","survey_prestige"]/100,
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90",]$earnscore,
    weight=prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90",]$count)[1:2]),
  t(wtd.cor(
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90","survey_prestige"]/100,
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90",]$eduscore,
    weight=prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90",]$count)[1:2]),
  t(wtd.cor(
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90","survey_prestige"]/100,
    prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90",]$npboss,
    weight=prestige_data[prestige_data$corpus=="ngram"&prestige_data$year=="prent90",]$count)[1:2])
)
## print results
cbind(corr_1990,rsq_1990)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Main Robust TWFE - Figure 6 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## import function
source("/Users/wenhao/Dropbox/Devaluation Word Embeddings/code2/function.R")

###### with controls ######

## save results - ngram
results_ngram <- data.frame(
  outcome = rep(c("prestige","potency","evaluation","activity"),6),
  window = c(rep(0,4),
             rep(1,4),
             rep(2,4),
             rep(3,4),
             rep(4,4),
             rep(5,4)),
  coef = rep(NA,24),
  se = rep(NA,24))

for (out in c("prestige","potency","evaluation","activity")){
  for (t in 1:5){
    results_ngram[results_ngram$outcome==out&results_ngram$window==t,"coef"] <- 
      FErobust(full_data[full_data$corpus=="ngram", ], count=11, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[1]
    results_ngram[results_ngram$outcome==out&results_ngram$window==t,"se"] <- 
      FErobust(full_data[full_data$corpus=="ngram", ], count=11, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[2]
  } 
}

## append the vanilla TWFE estimates
for (out in c("prestige","potency","evaluation","activity")){
  results_ngram[results_ngram$outcome==out&results_ngram$window==0,"coef"] <- 
    summary(felm(as.formula(paste(out,"~ gender + income + education | code + year | 0 | code")),
          full_data[full_data$corpus=="ngram", ]))$coef[1,1]
  results_ngram[results_ngram$outcome==out&results_ngram$window==0,"se"] <- 
    summary(felm(as.formula(paste(out,"~ gender + income + education | code + year | 0 | code")),
                 full_data[full_data$corpus=="ngram", ]))$coef[1,2]
}

results_ngram %>%
  filter(window<=4) %>%
  mutate(window=as.character(window)) %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       .default = "0")) %>%
  mutate(outcome=case_when(outcome=="prestige"~"general prestige",
                           .default = outcome)) %>%
  mutate(outcome=factor(outcome,levels=c("general prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=window,y=coef)) +
  geom_point(aes(color=sig),size=2.6) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ylim(-0.25,0.05) +
  scale_x_discrete(labels= c("full","1","2","3","4")) +
  facet_wrap(.~outcome, ncol=4) +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  ggtitle("Ngram, 1900-2009") +
  scale_y_continuous(labels=function(x) sprintf("%.2f", x)) +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/main/fig6A_main_ngram.tiff", 
       width = 17.5, height = 5.2, units = "cm", dpi = 300)

## save results - COCHA
results_COCHA <- data.frame(
  outcome = rep(c("prestige","potency","evaluation","activity"),6),
  window = c(rep(0,4),
             rep(1,4),
             rep(2,4),
             rep(3,4),
             rep(4,4),
             rep(5,4)),
  coef = rep(NA,24),
  se = rep(NA,24))

for (out in c("prestige","potency","evaluation","activity")){
  for (t in 1:5){
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"coef"] <- 
      FErobust(full_data[full_data$corpus=="COCHA", ], count=12, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[1]
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"se"] <- 
      FErobust(full_data[full_data$corpus=="COCHA", ], count=12, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[2]
  } 
}

## append the vanilla TWFE estimates
for (out in c("prestige","potency","evaluation","activity")){
  results_COCHA[results_COCHA$outcome==out&results_COCHA$window==0,"coef"] <- 
    summary(felm(as.formula(paste(out,"~ gender + income + education | code + year | 0 | code")),
                 full_data[full_data$corpus=="COCHA", ]))$coef[1,1]
  results_COCHA[results_COCHA$outcome==out&results_COCHA$window==0,"se"] <- 
    summary(felm(as.formula(paste(out,"~ gender + income + education | code + year | 0 | code")),
                 full_data[full_data$corpus=="COCHA", ]))$coef[1,2]
}

results_COCHA %>%
  filter(window<=4) %>%
  mutate(window=as.character(window)) %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       coef-1.96*se>0~"1",
                       .default = "0")) %>%
  mutate(outcome=case_when(outcome=="prestige"~"general prestige",
                           .default = outcome)) %>%
  mutate(outcome=factor(outcome,levels=c("general prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=window,y=coef)) +
  geom_point(aes(color=sig),size=2.6) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ylim(-0.28,0.07) +
  scale_x_discrete(labels= c("full","1","2","3","4")) +
  facet_grid(.~outcome) +
  ggtitle("COCHA, 1900-2019") +
  scale_y_continuous(labels=function(x) sprintf("%.2f", x)) +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/main/fig6B_main_COCHA.tiff", 
       width = 17.5, height = 5.2, units = "cm", dpi = 300)

## interpret the magnitude of the results
full_data %>%
  filter(corpus=="COCHA") %>%
  group_by(code) %>%
  summarize(sd_gender=sd(gender,na.rm=T),
            sd_potency=sd(potency,na.rm=T),
            sd_prestige=sd(prestige,na.rm=T)) %>%
  summarize(sd_gender=mean(sd_gender,na.rm=T),
            sd_potency=mean(sd_potency,na.rm=T),
            sd_prestige=mean(sd_prestige,na.rm=T))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Mediation Analysis - Figure 7 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

library(mediation)

## PCA measure of prestige in each year

## Ngram
ngram <- full_data[full_data$corpus=="ngram"&!is.na(full_data$gender),]
for (i in unique(ngram$year)) {
  ngram[which(ngram$year==i),"PCA"] <-
    princomp(ngram[which(ngram$year==i),
                         c("prestige","evaluation",
                            "potency","activity")])$scores[,1]
}
ngram <- ngram %>%
  dplyr::select(code,year,corpus,PCA)

## COCHA
COCHA <- full_data[full_data$corpus=="COCHA"&!is.na(full_data$gender),]
for (i in unique(COCHA$year)) {
  COCHA[which(COCHA$year==i),"PCA"] <-
    princomp(COCHA[which(COCHA$year==i),
                   c("prestige","evaluation",
                     "potency","activity")])$scores[,1]
}
COCHA <- COCHA %>%
  dplyr::select(code,year,corpus,PCA)

## merge with PCA
full_data_pca <-
  merge(full_data,rbind(ngram,COCHA),
      by=c("code","year","corpus"),
      all.x=T)

full_data_corpusFE <- full_data_pca %>%
  mutate(corpus_ngram=case_when(
    corpus=="ngram"~1,
    .default = 0
  ))

## mediation
model <- '
  PCA ~ a1 * gender + p1 * corpus_ngram
  mean_wage ~ c_prime * gender + b2 * PCA + k1 * corpus_ngram

  ## controls of human capital
  mean_wage ~~ d1 * mean_edu + d2 * mean_lmexp +
  d3 * mean_hours + d4 * change_prop + d11 * estimate +
  d5 * routine_cog +
  d6 * person_int + d7 * numerical + d8 * eye_hand_foot +
  d9 * intelligence + d10 * verbal +
  d12 * spatial + d13 * form + d14 * clerical + 
  d15 * motor + d16 * finger + d17 * manual +
  d18 * color
         
  PCA ~~ e1 * mean_edu + e2 * mean_lmexp +
  e3 * mean_hours + e4 * change_prop + e11 * estimate +
  e5 * routine_cog +
  e6 * person_int + e7 * numerical + e8 * eye_hand_foot +
  e9 * intelligence + e10 * verbal +
  e12 * spatial + e13 * form + e14 * clerical + 
  e15 * motor + e16 * finger + e17 * manual +
  e18 * color

  # Total effect
  total_effect := c_prime + (a1 * b2)
  indirect_effect := (a1 * b2)
'

## three lag strategies
plot <- list()
for (i in 0:2) {
 
  ## bootstrap the 95% CI 
  results <- data.frame(total = as.numeric(),
                        indirect = as.numeric(),
                        direct = as.numeric())
  
  for (b in 1:100){
    
    ## resample with replacement
    boot_sample <- full_data_corpusFE[sample(1:dim(full_data_corpusFE)[1],dim(full_data_corpusFE)[1],replace=TRUE),] 
    fit <- sem(model, data = boot_sample %>% 
                 group_by(code) %>%
                 arrange(year) %>%
                 mutate(percent_f = lag(percent_f, i*2),
                        gender = lag(gender, i*1),
                        PCA = lag(PCA, i*1)) %>%
                 mutate(mean_wage = log(mean_wage))) ## log wage
    
    ## get estimates
    total <- summary(fit)$pe[65,6]
    indirect <- summary(fit)$pe[66,6]
    direct <- total - indirect
    
    ## save results
    results[b,"total"] <- total
    results[b,"indirect"] <- indirect
    results[b,"direct"] <- direct
  }
  
  plot[[i+1]] <-
    results %>%
    summarize(est_total = mean(total),
              se_total = sd(total),
              est_direct = mean(direct),
              se_direct = sd(direct),
              est_mediate = mean(indirect),
              se_mediate = sd(indirect)) %>%
    reshape(varying = 1:6,
            sep= "_",
            timevar= "category",
            times = c("total","direct","mediate"),
            new.row.names= 1:3,
            direction = "long") %>%
  mutate(color=case_when(category=="mediate"~"red3",
                         .default = "blue3")) %>%
    ggplot(aes(x=category, y=est)) +
    geom_point(aes(color=color),size=5) +
    geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se,color=color),size=0.75, width=0) +
    xlab("") +
    scale_y_continuous(labels=function(x) sprintf("%.1f", x)) +
    ylab("estimation") +
    geom_hline(yintercept = 0, lty="dashed") +
    scale_color_manual(values=c("blue3","red4")) +
    xlab("") +
    ylab("estimation") +
    geom_hline(yintercept = 0, lty="dashed") +
    coord_flip() +
    theme_minimal() +
    ggtitle(paste0("gender lag = ", as.character(i))) +
    theme(
      legend.position = "none",
      plot.title = element_text(size=21,hjust=0.5,face="bold"),
      text=element_text(family="Times",size=16.5),
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      axis.title = element_text(size=20))
  
}

## plot results
ggarrange(plot[[1]],plot[[2]], plot[[3]],
          ncol=3,nrow=1)

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/main/fig7_mediation.tiff", 
       width = 28, height = 6.5, units = "cm", dpi = 300)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Robustness Check - Figure 8 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

###### full controls ######

## impute 1980 measure by linear intrapolation
df_interpolated <- 
  full_data %>%
  group_by(corpus,code) %>%
  filter(year %in% c(1960, 1970, 1990, 1980, 2000)) %>%
  select(code,year,corpus,intelligence,verbal,numerical,spatial,form,
         clerical,motor,finger,manual,eye_hand_foot,color,routine_cog,
         person_int) %>%
  mutate(across(c(intelligence, verbal,numerical,spatial,form,
                  clerical,motor,finger,manual,eye_hand_foot,color,routine_cog,
                  person_int), 
                ~ na.approx(., x = year, na.rm = FALSE, rule = 2), .names = "{.col}")) %>%
  ungroup()

## merge
full_data_impute <- full_data %>%
  select(-c(intelligence, verbal,numerical,spatial,form,
            clerical,motor,finger,manual,eye_hand_foot,color,routine_cog,
            person_int)) %>%
  left_join(df_interpolated,
            by = c("code", "year", "corpus"))

## save result
result <- data.frame(
  prestige = rep(c("prestige","potency","evaluation","activity"),4),
  model = c(rep("text",4),rep("basic_lm",4),rep("basic_skill_lm",4),rep("full",4)),
  estimate = rep(NA,16),
  SE = rep(NA,16),
  corpus = rep("ngram",16)
)
result <- rbind(result,result %>% mutate(corpus="COCHA"))

## for each corpus measure
for (cor in c("ngram","COCHA")){
  
  ## for each prestige measure
  for (p in c("prestige","potency","evaluation","activity")){
    
    ## original text control - estimate
    result[result$prestige==p & result$corpus==cor & result$model=="text", "estimate"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,1] 
    
    ## original text control - SE
    result[result$prestige==p & result$corpus==cor & result$model=="text", "SE"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,2] 
    
    ## basic labor market - estimate
    result[result$prestige==p & result$corpus==cor & result$model=="basic_lm", "estimate"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education + 
         race_1 + race_2 + race_3 + race_4 + mean_wage + mean_edu +
         mean_lmexp + I(mean_lmexp^2) + mean_hours + change_prop + 
                            estimate | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,1] 
    
    ## basic labor market - SE
    result[result$prestige==p & result$corpus==cor & result$model=="basic_lm", "SE"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education + 
         race_1 + race_2 + race_3 + race_4 + mean_wage + mean_edu +
         mean_lmexp + I(mean_lmexp^2) + mean_hours + change_prop + 
                            estimate | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,2] 
    
    ## basic skill labor market - estimate
    result[result$prestige==p & result$corpus==cor & result$model=="basic_skill_lm", "estimate"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education + 
         race_1 + race_2 + race_3 + race_4 + mean_wage + mean_edu +
         mean_lmexp + I(mean_lmexp^2) + mean_hours + change_prop + 
                            estimate + numerical + routine_cog + 
                            person_int | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,1] 
    
    ## basic skill labor market - SE
    result[result$prestige==p & result$corpus==cor & result$model=="basic_skill_lm", "SE"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education + 
         race_1 + race_2 + race_3 + race_4 + mean_wage + mean_edu +
         mean_lmexp + I(mean_lmexp^2) + mean_hours + change_prop + 
                            estimate + numerical + routine_cog + 
                            person_int | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,2] 
    
    ## full control - estimate
    result[result$prestige==p & result$corpus==cor & result$model=="full", "estimate"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education + 
         race_1 + race_2 + race_3 + race_4 + mean_wage + mean_edu +
         mean_lmexp + I(mean_lmexp^2) + mean_hours + change_prop + 
                            estimate + intelligence + verbal + numerical +
                            spatial + form + clerical + motor + finger + 
                            manual + eye_hand_foot + color + routine_cog + 
                            person_int | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,1] 
    
    ## full control - SE
    result[result$prestige==p & result$corpus==cor & result$model=="full", "SE"] <- 
      summary(
        felm(as.formula(paste(p,"~ gender + income + education + 
         race_1 + race_2 + race_3 + race_4 + mean_wage + mean_edu +
         mean_lmexp + I(mean_lmexp^2) + mean_hours + change_prop + 
                            estimate + intelligence + verbal + numerical +
                            spatial + form + clerical + motor + finger + 
                            manual + eye_hand_foot + color + routine_cog + 
                            person_int | id + year | 0 | id")), 
             full_data_impute[full_data_impute$corpus==cor, ]))$coef[1,2] 
  }
  
}

## Ngram
ngram <- result %>%
  filter(corpus=="ngram") %>%
  mutate(sig=case_when(estimate+1.96*SE<0~"1",
                       .default = "0")) %>%
  mutate(model = case_when(model=="text"~"text",
                           model=="basic_lm"~"basic\nlabor",
                           model=="basic_skill_lm"~"basic\nskill",
                           model=="full"~"full")) %>%
  mutate(model = factor(model, levels=c("text","basic\nlabor","basic\nskill","full"))) %>%
  mutate(prestige = factor(prestige, levels=c("prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=model,y=estimate)) +
  geom_point(size=3,aes(color=sig)) +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE,color=sig), width=0,
                linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_color_manual(values=c("blue3","red4")) +
  ## ylim(-0.26,0.1) +
  facet_wrap(.~prestige, ncol=4) +
  xlab("controls (cumulative from left to right)") +
  ## ylab("coefficient") +
  ggtitle("Ngram, 1960-1990") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(1, "lines"))

## COCHA
COCHA <- result %>%
  filter(corpus=="COCHA") %>%
  mutate(sig=case_when(estimate-1.96*SE>0~"1",
                       estimate+1.96*SE<0~"1",
                       .default = "0")) %>%
  mutate(model = case_when(model=="text"~"text",
                           model=="basic_lm"~"basic\nlabor",
                           model=="basic_skill_lm"~"basic\nskill",
                           model=="full"~"full")) %>%
  mutate(model = factor(model, levels=c("text","basic\nlabor","basic\nskill","full"))) %>%
  mutate(prestige = factor(prestige, levels=c("prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=model,y=estimate)) +
  geom_point(size=3,aes(color=sig)) +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE,color=sig), width=0,
                linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_color_manual(values=c("blue3","red4")) +
  ## ylim(-0.26,0.1) +
  facet_wrap(.~prestige, ncol=4) +
  xlab("controls (cumulative from left to right)") +
  ## ylab("coefficient") +
  ggtitle("COCHA, 1960-1990") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(1, "lines"))

ggarrange(ngram,COCHA,ncol=1)

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/main/fig8_robust_control.tiff", 
       width = 18, height = 11, units = "cm", dpi = 300)
