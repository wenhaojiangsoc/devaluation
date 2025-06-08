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
library(car)

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

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Gendered Occupation - Table S8 ######
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

###### Male ######

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main')
male_data <- data.frame()

## COCHA
data <- read.csv("COCHA_gendered_occupation_male.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Male.words)
data$corpus <- "COCHA"
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
male_data <- rbind(male_data,data)

## Ngram
data <- read.csv("ngram_gendered_occupation_male.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Male.words)
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
data$corpus <- "ngram"
male_data <- rbind(male_data,data)

## merge with 1950 occupation official code
code <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main/1950occcode.csv")
code$occ <- gsub('n.e.c.', 'nec', code$occ)
male_data$occupation_origin <- gsub('n.e.c.', 'nec', male_data$occupation_origin)
male_data <- merge(male_data, code, by.y="occ", by.x="occupation_origin", all.x = T)
rm(code)

male_data <-
  merge(male_data %>% dplyr::select(-("gender")),full_data[,c("year","occupation_origin","gender","corpus","count")],
      by = c("year","occupation_origin","corpus"),
      all.x = T)

male_data <-
  male_data %>%
  group_by(corpus,code) %>%
  mutate(id = cur_group_id())

## respective regression
summary(felm(potency ~ gender + education + income | code + year | 0 | code, male_data[male_data$corpus=="COCHA",]))
summary(felm(potency ~ gender + education + income | code + year | 0 | code, full_data[full_data$corpus=="COCHA",]))

###### Female ######

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main')
female_data <- data.frame()

## COCHA
data <- read.csv("COCHA_gendered_occupation_female.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Female.words)
data$corpus <- "COCHA"
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
female_data <- rbind(female_data,data)

## Ngram
data <- read.csv("ngram_gendered_occupation_female.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Female.words)
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
data$corpus <- "ngram"
female_data <- rbind(female_data,data)

## merge with 1950 occupation official code
code <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main/1950occcode.csv")
code$occ <- gsub('n.e.c.', 'nec', code$occ)
female_data$occupation_origin <- gsub('n.e.c.', 'nec', female_data$occupation_origin)
female_data <- merge(female_data, code, by.y="occ", by.x="occupation_origin", all.x = T)
rm(code)

female_data <-
  merge(female_data %>% select(-("gender")),full_data[,c("year","occupation_origin","gender","corpus","count")],
        by = c("year","occupation_origin","corpus"),
        all.x = T)

female_data <-
  female_data %>%
  group_by(corpus,code) %>%
  mutate(id = cur_group_id())

## respective regression
summary(felm(potency ~ gender + education + income | code + year | 0 | code, female_data[female_data$corpus=="COCHA",]))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
##### Robust TWFE between Affluence and Gender - Figure S20 ######
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## save results - COCHA
results_COCHA <- data.frame(
  outcome = rep(c("income"),12),
  window = seq(0,11,1),
  coef = rep(NA,12),
  se = rep(NA,12))

for (out in c("income")){
  for (t in 1:11){
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"coef"] <- 
      FErobust(full_data[full_data$corpus=="COCHA", ], count=12, t=t, outcome=out,
               treatment="gender",control=c("education"))[1]
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"se"] <- 
      FErobust(full_data[full_data$corpus=="COCHA", ], count=12, t=t, outcome=out,
               treatment="gender",control=c("education"))[2]
  } 
}

## append the vanilla TWFE estimates
for (out in c("income")){
  results_COCHA[results_COCHA$outcome==out&results_COCHA$window==0,"coef"] <- 
    summary(felm(as.formula(paste(out,"~ gender + education | code + year | 0 | code")),
                 full_data[full_data$corpus=="COCHA", ]))$coef[1,1]
  results_COCHA[results_COCHA$outcome==out&results_COCHA$window==0,"se"] <- 
    summary(felm(as.formula(paste(out,"~ gender + education | code + year | 0 | code")),
                 full_data[full_data$corpus=="COCHA", ]))$coef[1,2]
}

results_COCHA %>%
  mutate(window=as.character(window)) %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       .default = "0")) %>%
  mutate(outcome=case_when(outcome=="prestige"~"general prestige",
                           .default = outcome)) %>%
  mutate(outcome=factor(outcome,levels=c("general prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=window,y=coef)) +
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ylim(-0.13,0.15) +
  scale_x_discrete(labels= c("full","1","2","3","4","5","6","7","8","9","10","11")) +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  ggtitle("Affluence (COCHA)") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/affluence_gender_COCHA.png", 
       width = 8, height = 7, units = "cm", dpi = 600)

#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~
##### Dimensional Correlation - Figure S3 #####
#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~

## 3-year moving average (smooth plot)
library(zoo)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

correlation <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Robustness/dimension_correlation.csv")
correlation[correlation$dimension=="income","dimension"] <- "affluence"
correlation[correlation$dimension=="education","dimension"] <- "cultivation"
correlation[correlation$corpus=="ngram","corpus"] <- "Ngram"
correlation[correlation$corpus=="COHA","corpus"] <- "COCHA"
correlation %>%
  group_by(corpus) %>%
  mutate(cos = three_ma(cos)) %>%
  mutate(corpus = factor(corpus, levels=c("Ngram","COCHA")),
         dimension = factor(dimension, levels=c("potency","prestige","evaluation",
                                                "activity","affluence","cultivation"))) %>%
  arrange(desc(year)) %>%
  ggplot(aes(x=year, y=cos, color=dimension)) +
  geom_point(size=1.4) +
  scale_color_manual(values=c("red4","blue3","green4","grey30","orange2","purple3")) +
  geom_line(aes(lty=dimension)) +
  facet_grid(. ~ corpus, scales="free") +
  xlim(1900,2000) +
  ylab("Cosine Similarity") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1900, 2010, by=10)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_classic() +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=12, angle = 45, vjust = 0.75, hjust = 0.75),
        axis.text.y = element_text(size=12),
        axis.title.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

## ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/dimension_correlation.png", width = 16, height = 11.6, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
##### Bigram COCHA - Figure S4 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
## bigram
bigram <- read.csv("COCHA_bigram.csv")
bigram <- bigram %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Bigram.words)
bigram <- bigram %>%
  distinct(occupation_single, year, .keep_all = T)

## create identifier
bigram <- bigram %>%
  filter(occupation_single!="retired") %>%
  group_by(occupation_origin) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  as.data.frame()

## merge with 1950 occupation official code
code <- read.csv("1950occcode.csv")
code$occ <- gsub('n.e.c.', 'nec', code$occ)
bigram$occupation_origin <- gsub('n.e.c.', 'nec', bigram$occupation_origin)
bigram <- merge(bigram, code, by.y="occ", by.x="occupation_origin", all.x = T)
rm(code)

## drop non-civilian jobs
full_data <-
  full_data %>%
  filter(code!=595&code!=987&code!=984&code!=982)

## count
full_data <-
  full_data %>%
  group_by(id) %>%
  mutate(count=mean(count,na.rm=T))

## import function
source("/Users/wenhao/Dropbox/Devaluation Word Embeddings/code2/function.R")

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
      FErobust(bigram, count=12, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[1]
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"se"] <- 
      FErobust(bigram, count=12, t=t, outcome=out,
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
  mutate(sig=case_when(coef+2.56*se<0~"1",
                       coef-2.56*se>0~"1",
                       .default = "0")) %>%
  mutate(outcome=case_when(outcome=="prestige"~"general prestige",
                           .default = outcome)) %>%
  mutate(outcome=factor(outcome,levels=c("general prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=window,y=coef)) +
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-2.56*se, ymax=coef+2.56*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ylim(-0.33,0.17) +
  scale_x_discrete(labels= c("full","1","2","3","4")) +
  facet_grid(.~outcome) +
  ggtitle("COCHA, 1900-2019") +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/bigram_COCHA.png", 
       width = 17, height = 5.5, units = "cm", dpi = 600)

#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~
##### Change of Gender Typing - Figure S6 #####
#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~

library(boot)
library(tidyr)
library(zoo)
library(ggpubr)

## bootstrap
boot_mean <- function(original_vector, resample_vector) {
  mean(original_vector[resample_vector],na.rm=T)
}

## 3-year moving average (smooth plot)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

## COCHA
t <-
  full_data %>%
  filter(corpus=="COCHA") %>%
  group_by(year) %>%
  summarise(mean_gender = mean(gender,na.rm=T),
            sd_gender = sd(boot(gender, boot_mean, R = 2010)$t),
            ## percent female
            mean_f = (weighted.mean(percent_f, w=count,na.rm=T) - 0.61)/4.6,
            sd_f = sd(boot(percent_f, boot_mean, R = 2010)$t)/4.6) %>%
  pivot_longer(contains("_"), names_to = c(".value", "variable"), names_sep = "_") %>%
  group_by(variable) %>%
  mutate(mean=three_ma(mean))

COCHA_trend <- t %>%
  ggplot(aes(x=year,y=mean,group=variable)) +
  geom_point(aes(color=variable), size=2) +
  geom_line(aes(color=variable)) +
  geom_ribbon(aes(y = mean, ymin = mean-sd, ymax = mean+sd,
                  group=variable,fill=variable), 
              alpha=0.2,
              show.legend=FALSE) +
  scale_y_continuous("Smoothed Female Typing",
                     sec.axis = sec_axis( ~ .*(4.6) + 0.65, name = "Proportion Female")) +
  scale_x_continuous(breaks = seq(1900,2010,20)) +
  theme_classic() +
  ggtitle("COCHA, 1900-2019") +
  scale_color_manual(name = "", labels=c("Proportion Female","Female Typing"), values=c("blue3","green4")) +
  scale_fill_manual(name = "", labels=c("Proportion Female","Female Typing"), values=c("blue3","green4")) +
  theme(text=element_text(family="Times",size=14),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom",
        plot.title = element_text(size=16,hjust=0.5,face="bold"),
        axis.text.x = element_text(size=14,angle=45,vjust=0.6),
        axis.text.y=element_text(size=14),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))


## Ngram
t <-
  full_data %>%
  filter(corpus=="ngram"&!is.na(count)&!is.na(gender)) %>%
  group_by(year) %>%
  summarise(mean_gender = weighted.mean(gender,w=count,na.rm=T),
            sd_gender = sd(boot(gender, boot_mean, R = 2000)$t),
            ## percent female
            mean_f = (weighted.mean(percent_f, w=count,na.rm=T) - 0.35)/11.5,
            sd_f = sd(boot(percent_f, boot_mean, R = 2000)$t)/11.5) %>%
  pivot_longer(contains("_"), names_to = c(".value", "variable"), names_sep = "_") %>%
  group_by(variable) %>%
  mutate(mean=three_ma(mean))

ngram_trend <- t %>%
  ggplot(aes(x=year,y=mean,group=variable)) +
  geom_point(aes(color=variable), size=2) +
  geom_line(aes(color=variable)) +
  geom_ribbon(aes(y = mean, ymin = mean-sd, ymax = mean+sd,
                  group=variable,fill=variable), 
              alpha=0.2,
              show.legend=FALSE) +
  scale_y_continuous("Smoothed Female Typing",
                     sec.axis = sec_axis( ~ .*(11.5) + 0.4, name = "Proportion Female")) +
  scale_x_continuous(breaks = seq(1900,2000,20)) +
  theme_classic() +
  ggtitle("Ngram, 1900-2009") +
  scale_color_manual(name = "", labels=c("Proportion Female","Female Typing"), values=c("blue3","green4")) +
  scale_fill_manual(name = "", labels=c("Proportion Female","Female Typing"), values=c("blue3","green4")) +
  theme(text=element_text(family="Times",size=14),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom",
        plot.title = element_text(size=16,hjust=0.5,face="bold"),
        axis.text.x = element_text(size=14,angle=45,vjust=0.6),
        axis.text.y=element_text(size=14),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

ggarrange(COCHA_trend,ngram_trend,ncol=2,common.legend=T,legend="bottom")
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/trend_typing_percent.png", width = 23, height = 11, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Comparison between Embedding-based Prestige and Survey-based Prestige - Figure S7 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

full_data %>% filter(year==1960&
                       presgl>0&
                       corpus=="COCHA") %>%
  mutate(occupation_word = word(occupation_single, 1),
         occupation_word = gsub(",","",occupation_word)) %>%
  distinct(year,presgl,.keep_all = TRUE) %>%
  ggplot(aes(x=prestige,y=presgl,size=count)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=F,alpha=0.1,
              fullrange = TRUE, mapping = aes(weight = count)) + 
  geom_point(color="blue",fill="white",shape=21,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_text(data = full_data %>% filter(year==1960&
                                          presgl>0&
                                          corpus=="COCHA") %>%
              mutate(occupation_word = word(occupation_single, 1),
                     occupation_word = gsub(",","",occupation_word)) %>%
              distinct(year,presgl,.keep_all = TRUE) %>%
              distinct(occupation_word, .keep_all = TRUE) %>%
              filter(occupation_word!="technician"),
            aes(label=occupation_word),
            size=5,
            check_overlap = T,
            family="Times") +
  xlab("Embedding-based General Prestige") +
  ylab("Siegel Prestige") +
  guides(size = FALSE) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/prestige_comparison_1960.png", width = 13.6, height = 13, units = "cm")


full_data$occupation_word <- word(full_data$occupation_origin, 1)
full_data$occupation_word <- gsub(",","",full_data$occupation_word)

full_data %>% filter(year==1990&
                       corpus=="ngram"&
                       prestige>-0.13) %>%
  mutate(occupation_word = word(occupation_single, 1),
         occupation_word = gsub(",","",occupation_word)) %>%
  distinct(year,prent90,.keep_all = TRUE) %>%
  ggplot(aes(x=prestige,y=prent90,size=count)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=F,alpha=0.1,
              mapping = aes(weight = count), 
              fullrange = TRUE) + 
  geom_point(color="blue",fill="white",shape=21,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_text(data = full_data %>% filter(year==1990&
                                          corpus=="ngram"&
                                          prestige>-0.13) %>%
              mutate(occupation_word = word(occupation_single, 1),
                     occupation_word = gsub(",","",occupation_word)) %>%
              distinct(year,presgl,.keep_all = TRUE) %>%
              distinct(occupation_word, .keep_all = TRUE) %>%
              filter(occupation_word!="attendant") %>%
              filter(occupation_word!="professor"),
            aes(label=occupation_word),
            size=5,
            check_overlap = T,
            family="Times") +
  xlab("Embedding-based General Prestige") +
  ylab("Nakao-Treas Prestige") +
  guides(size = FALSE) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
##### Full window decomposition TWFE #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

## import function
source("/Users/wenhao/Dropbox/Devaluation Word Embeddings/code2/function.R")

###### Generalized Robust ######

## save results - ngram
results_ngram <- data.frame(
  outcome = rep(c("prestige","potency","evaluation","activity"),10),
  window = c(rep(1,4),
             rep(2,4),
             rep(3,4),
             rep(4,4),
             rep(5,4),
             rep(6,4),
             rep(7,4),
             rep(8,4),
             rep(9,4),
             rep(10,4)),
  coef = rep(NA,40),
  se = rep(NA,40))

for (out in c("prestige","potency","evaluation","activity")){
  for (t in 1:10){
    results_ngram[results_ngram$outcome==out&results_ngram$window==t,"coef"] <- 
      FErobust(full_data[full_data$corpus=="ngram", ], count=11, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[1]
    results_ngram[results_ngram$outcome==out&results_ngram$window==t,"se"] <- 
      FErobust(full_data[full_data$corpus=="ngram", ], count=11, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[2]
  } 
}

results_ngram %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       .default = "0")) %>%
  mutate(outcome=case_when(outcome=="prestige"~"general prestige",
                           .default = outcome)) %>%
  mutate(outcome=factor(outcome,levels=c("general prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=window,y=coef)) +
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(1,10,1)) +
  ylim(-0.25,0.05) +
  facet_wrap(.~outcome, ncol=4) +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  ggtitle("Ngram, 1900-2009") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/full_ngram.png", 
       width = 20, height = 5.5, units = "cm", dpi = 600)

## save results - COCHA
results_COCHA <- data.frame(
  outcome = rep(c("prestige","potency","evaluation","activity"),11),
  window = c(rep(1,4),
             rep(2,4),
             rep(3,4),
             rep(4,4),
             rep(5,4),
             rep(6,4),
             rep(7,4),
             rep(8,4),
             rep(9,4),
             rep(10,4),
             rep(11,4)),
  coef = rep(NA,44),
  se = rep(NA,44))

for (out in c("prestige","potency","evaluation","activity")){
  for (t in 1:11){
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"coef"] <- 
      FErobust(full_data[full_data$corpus=="COCHA", ], count=12, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[1]
    results_COCHA[results_COCHA$outcome==out&results_COCHA$window==t,"se"] <- 
      FErobust(full_data[full_data$corpus=="COCHA", ], count=12, t=t, outcome=out,
               treatment="gender",control=c("education","income"))[2]
  } 
}

results_COCHA %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       coef-1.96*se>0~"1",
                       .default = "0")) %>%
  mutate(outcome=case_when(outcome=="prestige"~"general prestige",
                           .default = outcome)) %>%
  mutate(outcome=factor(outcome,levels=c("general prestige","potency","evaluation","activity"))) %>%
  ggplot(aes(x=window,y=coef)) +
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_x_continuous(breaks=seq(1,11,1)) +
  ylim(-0.28,0.07) +
  facet_grid(.~outcome) +
  ggtitle("COCHA, 1900-2019") +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/full_COCHA.png", 
       width = 20, height = 5.5, units = "cm", dpi = 600)


###### Decomposition - Figure S9 ######

## Ngram - general prestige
df <- difference(full_data[full_data$corpus=="ngram", ] %>% group_by(id) %>%
                   mutate(count=sum(!is.na(gender))) %>%
                   filter(count==count) %>%
                   as.data.frame(),
                 k=11-1,
                 outcome="prestige",treatment="gender",control=c("education","income"))

FD_results <- FEdecompose(df,
            k=11-1,
            outcome="prestige",treatment="gender",control=c("education","income"))
bias <- summary(felm(prestige~gender+education+income|id+year|0|id,
                     full_data[full_data$corpus=="ngram", ] %>% group_by(id) %>%
                       mutate(count=sum(!is.na(gender))) %>%
                       filter(count==count) %>%
                       as.data.frame()))$coef[1,1] - sum(FD_results$FD_regression*FD_results$weight)

### plot
cols <- c("FD Coefficient"="blue","Bias"="green4","TWFE"="red3","Weight"="grey")
adjust <- 0.56
ngram_prestige <- FD_results %>%
  mutate(gap=k*10) %>%
  ggplot(aes(x=gap,y=FD_regression+adjust)) +
  geom_point(aes(color="FD Coefficient"),shape=19,size=2.7) +
  scale_y_continuous(sec.axis = sec_axis(~ .-adjust, name = "First-Different Coefficient"),
                     limits = c(0,0.6)) +
  geom_bar(aes(x=gap, y=weight,fill="Weight"),stat="identity",color="grey", width=9, alpha=0.2) +
  geom_errorbar(aes(ymin=FD_regression-1.96*FD_SD+adjust, ymax=FD_regression+1.96*FD_SD+adjust,color="FD Coefficient"), width=0) +
  ylab("Weight") +
  xlab("Gap Years") +
  scale_x_continuous(breaks=seq(10,100,10)) +
  geom_hline(aes(yintercept=summary(felm(prestige ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="ngram", ]))$coef[1,1]+adjust,
                 linetype="TWFE"), color = "red3") +
  geom_hline(aes(yintercept=bias+adjust,linetype="Bias"),color="green4") +
  geom_hline(yintercept = adjust, color = "grey50", lty = 2) +
  scale_color_manual(name=NULL,values=cols) +
  scale_fill_manual(name=NULL,values=cols) +
  scale_linetype_manual(name = NULL, values=c(4,6),
                        guide = guide_legend(override.aes = list(color = c("green4","red3")))) +
  theme_bw() +
  ggtitle("Ngram, General Prestige") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black', size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        plot.title = element_text(size=16,hjust=0.5,face="bold"),
        axis.title.x = element_text(size=13),
        text=element_text(family="Times",size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=17))

## Ngram - potency
df <- difference(full_data[full_data$corpus=="ngram", ] %>% group_by(id) %>%
                   mutate(count=sum(!is.na(gender))) %>%
                   filter(count==count) %>%
                   as.data.frame(),
                 k=11-1,
                 outcome="potency",treatment="gender",control=c("education","income"))

FD_results <- FEdecompose(df,
                          k=11-1,
                          outcome="potency",treatment="gender",control=c("education","income"))
bias <- summary(felm(potency~gender+education+income|id+year|0|id,
                     full_data[full_data$corpus=="ngram", ] %>% group_by(id) %>%
                       mutate(count=sum(!is.na(gender))) %>%
                       filter(count==count) %>%
                       as.data.frame()))$coef[1,1] - sum(FD_results$FD_regression*FD_results$weight)

### plot
cols <- c("FD Coefficient"="blue","Bias"="green4","TWFE"="red3","Weight"="grey")
adjust <- 0.56
ngram_potency <- FD_results %>%
  mutate(gap=k*10) %>%
  ggplot(aes(x=gap,y=FD_regression+adjust)) +
  geom_point(aes(color="FD Coefficient"),shape=19,size=2.7) +
  scale_y_continuous(sec.axis = sec_axis(~ .-adjust, name = "First-Different Coefficient"),
                     limits = c(0,0.6)) +
  geom_bar(aes(x=gap, y=weight,fill="Weight"),stat="identity",color="grey", width=9, alpha=0.2) +
  geom_errorbar(aes(ymin=FD_regression-1.96*FD_SD+adjust, ymax=FD_regression+1.96*FD_SD+adjust,color="FD Coefficient"), width=0) +
  ylab("Weight") +
  xlab("Gap Years") +
  scale_x_continuous(breaks=seq(10,100,10)) +
  geom_hline(aes(yintercept=summary(felm(potency ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="ngram", ]))$coef[1,1]+adjust,
                 linetype="TWFE"), color = "red3") +
  geom_hline(aes(yintercept=bias+adjust,linetype="Bias"),color="green4") +
  geom_hline(yintercept = adjust, color = "grey50", lty = 2) +
  scale_color_manual(name=NULL,values=cols) +
  scale_fill_manual(name=NULL,values=cols) +
  scale_linetype_manual(name = NULL, values=c(4,6),
                        guide = guide_legend(override.aes = list(color = c("green4","red3")))) +
  theme_bw() +
  ggtitle("Ngram, Potency") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black', size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        plot.title = element_text(size=16,hjust=0.5,face="bold"),
        axis.title.x = element_text(size=13),
        text=element_text(family="Times",size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=17))

ggarrange(ngram_prestige,ngram_potency,common.legend=TRUE,legend="bottom")
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/twfe_decomp_ngram.png", width = 18, height = 10, units = "cm")

## COCHA - general prestige
df <- difference(full_data[full_data$corpus=="COCHA", ] %>% group_by(id) %>%
                   mutate(count=sum(!is.na(gender))) %>%
                   filter(count==count) %>%
                   as.data.frame(),
                 k=12-1,
                 outcome="prestige",treatment="gender",control=c("education","income"))

FD_results <- FEdecompose(df,
                          k=12-1,
                          outcome="prestige",treatment="gender",control=c("education","income"))
bias <- summary(felm(prestige~gender+education+income|id+year|0|id,
                     full_data[full_data$corpus=="COCHA", ] %>% group_by(id) %>%
                       mutate(count=sum(!is.na(gender))) %>%
                       filter(count==count) %>%
                       as.data.frame()))$coef[1,1] - sum(FD_results$FD_regression*FD_results$weight)

### plot
cols <- c("FD Coefficient"="blue","Bias"="green4","TWFE"="red3","Weight"="grey")
adjust <- 0.56
COCHA_prestige <- FD_results %>%
  mutate(gap=k*10) %>%
  ggplot(aes(x=gap,y=FD_regression+adjust)) +
  geom_point(aes(color="FD Coefficient"),shape=19,size=2.7) +
  scale_y_continuous(sec.axis = sec_axis(~ .-adjust, name = "First-Different Coefficient"),
                     limits = c(0,0.6)) +
  geom_bar(aes(x=gap, y=weight,fill="Weight"),stat="identity",color="grey", width=9, alpha=0.2) +
  geom_errorbar(aes(ymin=FD_regression-1.96*FD_SD+adjust, ymax=FD_regression+1.96*FD_SD+adjust,color="FD Coefficient"), width=0) +
  ylab("Weight") +
  xlab("Gap Years") +
  scale_x_continuous(breaks=seq(10,110,10)) +
  geom_hline(aes(yintercept=summary(felm(prestige ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="COCHA", ]))$coef[1,1]+adjust,
                 linetype="TWFE"), color = "red3") +
  geom_hline(aes(yintercept=bias+adjust,linetype="Bias"),color="green4") +
  geom_hline(yintercept = adjust, color = "grey50", lty = 2) +
  scale_color_manual(name=NULL,values=cols) +
  scale_fill_manual(name=NULL,values=cols) +
  scale_linetype_manual(name = NULL, values=c(4,6),
                        guide = guide_legend(override.aes = list(color = c("green4","red3")))) +
  theme_bw() +
  ggtitle("COCHA, General Prestige") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black', size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        plot.title = element_text(size=16,hjust=0.5,face="bold"),
        axis.title.x = element_text(size=13),
        text=element_text(family="Times",size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=17))

## COCHA - potency
df <- difference(full_data[full_data$corpus=="COCHA", ] %>% group_by(id) %>%
                   mutate(count=sum(!is.na(gender))) %>%
                   filter(count==count) %>%
                   as.data.frame(),
                 k=12-1,
                 outcome="potency",treatment="gender",control=c("education","income"))

FD_results <- FEdecompose(df,
                          k=12-1,
                          outcome="potency",treatment="gender",control=c("education","income"))
bias <- summary(felm(potency~gender+education+income|id+year|0|id,
                     full_data[full_data$corpus=="COCHA", ] %>% group_by(id) %>%
                       mutate(count=sum(!is.na(gender))) %>%
                       filter(count==count) %>%
                       as.data.frame()))$coef[1,1] - sum(FD_results$FD_regression*FD_results$weight)

### plot
cols <- c("FD Coefficient"="blue","Bias"="green4","TWFE"="red3","Weight"="grey")
adjust <- 0.56
COCHA_potency <- FD_results %>%
  mutate(gap=k*10) %>%
  ggplot(aes(x=gap,y=FD_regression+adjust)) +
  geom_point(aes(color="FD Coefficient"),shape=19,size=2.7) +
  scale_y_continuous(sec.axis = sec_axis(~ .-adjust, name = "First-Different Coefficient"),
                     limits = c(0,0.6)) +
  geom_bar(aes(x=gap, y=weight,fill="Weight"),stat="identity",color="grey", width=9, alpha=0.2) +
  geom_errorbar(aes(ymin=FD_regression-1.96*FD_SD+adjust, ymax=FD_regression+1.96*FD_SD+adjust,color="FD Coefficient"), width=0) +
  ylab("Weight") +
  xlab("Gap Years") +
  scale_x_continuous(breaks=seq(10,110,10)) +
  geom_hline(aes(yintercept=summary(felm(potency ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="COCHA", ]))$coef[1,1]+adjust,
                 linetype="TWFE"), color = "red3") +
  geom_hline(aes(yintercept=bias+adjust,linetype="Bias"),color="green4") +
  geom_hline(yintercept = adjust, color = "grey50", lty = 2) +
  scale_color_manual(name=NULL,values=cols) +
  scale_fill_manual(name=NULL,values=cols) +
  scale_linetype_manual(name = NULL, values=c(4,6),
                        guide = guide_legend(override.aes = list(color = c("green4","red3")))) +
  theme_bw() +
  ggtitle("COCHA, Potency") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black', size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        plot.title = element_text(size=16,hjust=0.5,face="bold"),
        axis.title.x = element_text(size=13),
        text=element_text(family="Times",size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=17))

ggarrange(COCHA_prestige,COCHA_potency,common.legend=TRUE,legend="bottom")
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/twfe_decomp_COCHA.png", width = 18, height = 10, units = "cm")

###### temporal heterogeneity - Figure S10 ######

library(margins)

## Ngram
fe <- lm(prestige ~ gender + income + education + factor(id) + factor(year)*gender, full_data%>%filter(corpus=="ngram"))
margin_ngram <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2000,10))))
margin_ngram$corpus <- "Ngram"

## COCHA
fe <- lm(prestige ~ gender + income + education + factor(id) + factor(year)*gender, full_data[full_data$corpus=="COCHA", ])
margin_cocha <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2010,10))))
margin_cocha$corpus <- "COCHA"

## 3-year moving average (smooth plot)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

## bind
margin <- rbind(margin_ngram,margin_cocha)
margin <- margin %>%
  mutate(AME=three_ma(AME)) %>%
  mutate(sig=case_when(AME+1.96*SE<=0~"1",
                       .default = "0"))

## plot - prestige
prestige <- ggplot(margin, aes(x=year,y=AME)) +
  geom_point(size=2.5,aes(color=sig))+
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE,color=sig), width=0) +
  scale_color_manual(values=c("blue3","red4")) +
  xlim(1900,2020) +
  ylim(-0.4,0.15) +
  ggtitle("General Prestige") +
  ylab("AME") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1900, 2010, by=20)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  facet_grid(~corpus) +
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        legend.text = element_text(size=13),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        legend.title = element_text(size=14),
        axis.text.x = element_text(angle = 45,vjust=0.9, hjust=0.9,size=13),
        axis.text.y=element_text(size=14),
        axis.title.x = element_blank(),
        axis.title = element_text(size=14))

## Ngram
fe <- lm(potency ~ gender + income + education + factor(id) + factor(year)*gender, full_data%>%filter(corpus=="ngram"))
margin_ngram <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2000,10))))
margin_ngram$corpus <- "Ngram"

## COCHA
fe <- lm(potency ~ gender + income + education + factor(id) + factor(year)*gender, full_data[full_data$corpus=="COCHA", ])
margin_cocha <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2010,10))))
margin_cocha$corpus <- "COCHA"

## 3-year moving average (smooth plot)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

## bind
margin <- rbind(margin_ngram,margin_cocha)
margin <- margin %>%
  mutate(AME=three_ma(AME)) %>%
  mutate(sig=case_when(AME+1.96*SE<=0~"1",
                       .default = "0"))

## plot - potency
potency <- ggplot(margin, aes(x=year,y=AME)) +
  geom_point(size=2.5,aes(color=sig))+
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE,color=sig), width=0) +
  scale_color_manual(values=c("red4","blue3")) +
  xlim(1900,2020) +
  ylim(-0.42,0.15) +
  ggtitle("Potency") +
  ylab("AME") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1900, 2010, by=20)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  facet_grid(~corpus) +
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        legend.text = element_text(size=13),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        legend.title = element_text(size=14),
        axis.text.x = element_text(angle = 45,vjust=0.9, hjust=0.9,size=13),
        axis.text.y=element_text(size=14),
        axis.title.x = element_blank(),
        axis.title = element_text(size=14))

ggarrange(prestige,potency,
          ncol=2)

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/temporal.png", width = 22, height = 8, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### PCA prestige correlation matrix - Figure S8 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

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
        by=c("code","year","corpus","id"),
        all.x=T)

## correlation matrix
correlation <- full_data_pca[,c("year","PCA","gender","id","corpus")] %>%
  filter(corpus=="COCHA") %>%
  arrange(year,id) 

## rank prestige
correlation <- correlation %>%
  group_by(year) %>%
  mutate(prestige = dense_rank(desc(PCA)),
         prestige = PCA)

correlation <-
  data.frame(
    prestige_1900 = correlation[which(correlation$year==1900),]$prestige,
    prestige_1910 = correlation[which(correlation$year==1910),]$prestige,
    prestige_1920 = correlation[which(correlation$year==1920),]$prestige,
    prestige_1930 = correlation[which(correlation$year==1930),]$prestige,
    prestige_1940 = correlation[which(correlation$year==1940),]$prestige,
    prestige_1950 = correlation[which(correlation$year==1950),]$prestige,
    prestige_1960 = correlation[which(correlation$year==1960),]$prestige,
    prestige_1970 = correlation[which(correlation$year==1970),]$prestige,
    prestige_1980 = correlation[which(correlation$year==1980),]$prestige,
    prestige_1990 = correlation[which(correlation$year==1990),]$prestige,
    prestige_2000 = correlation[which(correlation$year==2000),]$prestige,
    prestige_2010 = correlation[which(correlation$year==2010),]$prestige
  )

correlation <- cor(correlation, use="complete.obs")

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

library(reshape2)
correlation <- get_upper_tri(correlation)
correlation <- reshape2::melt(correlation, na.rm = TRUE)

ggplot(data = correlation, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "grey20", mid = "white", 
                       midpoint = 0.3, limit = c(0.3,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  scale_x_discrete(labels=seq(1900,2010,10)) +
  scale_y_discrete(labels=seq(1900,2010,10)) +
  xlab("Year") +
  ylab("Year") + 
  geom_text(aes(Var2, Var1, label = sprintf("%0.2f", round(value, digits = 2))), color = "white", size = 3, family="Times") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = "Times"),
    legend.justification = c(1, 0),
    legend.position = c(0.53, 0.76),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/prestige_cor_ngram.png", width = 10, height = 10, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### DPM Queueing and Devaluation - Figure S16 and S17 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

library(dpm)

result <-
  data.frame()

## dynamic panel - ngram - no lagged independent variable
data <- full_data[which(full_data$corpus=="ngram"),] %>% ungroup()
data_dpm <- panel_data(data %>% mutate(year = (year-1890)/10), id = id, wave = year)
for (i in c("prestige","potency","evaluation","activity")){
  
  ## estimation
  estimates <-
    summary(
      dpm(as.formula(paste(i, " ~ pre(gender) + income + education")),
          data = data_dpm,
          error.inv = TRUE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "ngram"
  estimates$model <- "main"
  estimates$prestige <- i
  estimates$variable <- c("female","affluence","cultivation","lagged prestige")
  estimates$lag <- "no lag"
  result <- rbind(result,estimates)
  
  estimates <-
    summary(
      dpm(as.formula(paste0("gender ~ pre(", i, ") + income + education")),
          data = data_dpm,
          error.inv = FALSE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "ngram"
  estimates$model <- "reciprocal"
  estimates$prestige <- i
  estimates$variable <- c("prestige","affluence","cultivation","lagged female")
  estimates$lag <- "no lag"
  result <- rbind(result,estimates)
  
  print(paste(i,"is done!"))
}

## dynamic panel - ngram - w/ lagged independent variable
for (i in c("prestige","potency","evaluation","activity")){
  
  ## estimation
  estimates <-
    summary(
      dpm(as.formula(paste(i, " ~ pre(gender) + pre(lag(gender)) + income + education")),
          data = data_dpm,
          error.inv = TRUE, 
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "ngram"
  estimates$model <- "main"
  estimates$prestige <- i
  estimates$variable <- c("female","lagged female","affluence","cultivation","lagged prestige")
  estimates$lag <- "lag"
  result <- rbind(result,estimates)
  
  estimates <-
    summary(
      dpm(as.formula(paste0("gender ~ pre(", i, ") + pre(lag(", i, ")) + income + education")),
          data = data_dpm,
          error.inv = FALSE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "ngram"
  estimates$model <- "reciprocal"
  estimates$prestige <- i
  estimates$variable <- c("prestige","lagged prestige","affluence","cultivation","lagged female")
  estimates$lag <- "lag"
  result <- rbind(result,estimates)
  
  print(paste(i,"is done!"))
}


## dynamic panel - COHA - no lagged independent variable
data <- full_data[which(full_data$corpus=="COCHA"),] %>% ungroup()
data_dpm <- panel_data(data %>% mutate(year = (year-1890)/10), id = id, wave = year)
for (i in c("prestige","potency","evaluation","activity")){
  
  ## estimation
  estimates <-
    summary(
      dpm(as.formula(paste(i, " ~ pre(gender) + income + education")),
          data = data_dpm,
          error.inv = TRUE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "COCHA"
  estimates$model <- "main"
  estimates$prestige <- i
  estimates$variable <- c("female","affluence","cultivation","lagged prestige")
  estimates$lag <- "no lag"
  result <- rbind(result,estimates)
  
  estimates <-
    summary(
      dpm(as.formula(paste0("gender ~ pre(", i, ") + income + education")),
          data = data_dpm,
          error.inv = FALSE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "COCHA"
  estimates$model <- "reciprocal"
  estimates$prestige <- i
  estimates$variable <- c("prestige","affluence","cultivation","lagged female")
  estimates$lag <- "no lag"
  result <- rbind(result,estimates)
  
  print(paste(i,"is done!"))
}

## dynamic panel - COCHA - w/ lagged independent variable
data <- full_data[which(full_data$corpus=="COCHA"),] %>% ungroup()
data_dpm <- panel_data(data %>% mutate(year = (year-1890)/10), id = id, wave = year)
for (i in c("prestige","potency","evaluation","activity")){
  
  ## estimation
  estimates <-
    summary(
      dpm(as.formula(paste(i, " ~ pre(gender) + pre(lag(gender)) + income + education")),
          data = data_dpm,
          error.inv = TRUE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "COCHA"
  estimates$model <- "main"
  estimates$prestige <- i
  estimates$variable <- c("female","lagged female","affluence","cultivation","lagged prestige")
  estimates$lag <- "lag"
  result <- rbind(result,estimates)
  
  estimates <-
    summary(
      dpm(as.formula(paste0("gender ~ pre(", i, ") + pre(lag(", i, ")) + income + education")),
          data = data_dpm,
          error.inv = FALSE,
          fixed.effects = TRUE,
          check.gradient = FALSE)
    )$coef[,2:3]
  estimates$corpus <- "COCHA"
  estimates$model <- "reciprocal"
  estimates$prestige <- i
  estimates$variable <- c("prestige","lagged prestige","affluence","cultivation","lagged female")
  estimates$lag <- "lag"
  result <- rbind(result,estimates)
  
  print(paste(i,"is done!"))
}


## devaluation w/o lag
devaluation <- result %>% filter(lag=="no lag") %>%
  filter(model=="main") %>%
  mutate(model="Devaluation model") %>%
  mutate(corpus=case_when(corpus=="ngram"~"Ngram",
                          .default = corpus)) %>%
  filter(variable=="female") %>%
  ggplot(aes(x=prestige, y=Est.)) +
  geom_point(position=position_dodge(2),size=2.7,color="red3")+
  geom_errorbar(aes(ymin=Est.-1.96*S.E., ymax=Est.+1.96*S.E.), width=0,
                position=position_dodge(2),color="red3") +
  facet_grid(model ~ corpus, scales="free") +
  coord_flip() +
  ylab("Coefficient")  +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.text.y = element_text(size = 14, colour = "black",face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

## queueing w/o lag
queue <- result %>% filter(lag=="no lag") %>%
  filter(model=="reciprocal") %>%
  mutate(model="Queueing model") %>%
  mutate(corpus=case_when(corpus=="ngram"~"Ngram",
                          .default = corpus)) %>%
  filter(variable=="prestige") %>%
  ggplot(aes(x=prestige, y=Est.)) +
  geom_point(position=position_dodge(2),size=2.7,color="blue3")+
  geom_errorbar(aes(ymin=Est.-1.96*S.E., ymax=Est.+1.96*S.E.), width=0,
                position=position_dodge(2),color="blue3") +
  facet_grid(model ~ corpus, scales="free") +
  coord_flip() +
  ylab("Coefficient")  +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(-0.2,0.05,0.1)) +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.text.y = element_text(size = 14, colour = "black",face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

## Figure S16
ggarrange(devaluation,queue,ncol=1)
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/dpm_memo.png", width = 12, height = 15, units = "cm")

## queueing w lag (contemporaneous)
cont <- result %>% filter(lag=="lag") %>%
  filter(model=="reciprocal") %>%
  mutate(model="Contemporaneous") %>%
  mutate(corpus=case_when(corpus=="ngram"~"Ngram",
                          .default = corpus)) %>%
  filter(variable=="prestige") %>%
  ggplot(aes(x=prestige, y=Est.)) +
  geom_point(position=position_dodge(2),size=2.7,color="red3")+
  geom_errorbar(aes(ymin=Est.-1.96*S.E., ymax=Est.+1.96*S.E.), width=0,
                position=position_dodge(2),color="red3") +
  facet_grid(model ~ corpus, scales="free") +
  coord_flip() +
  ylab("Coefficient")  +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(-0.35,0.1,0.2),limits = c(-0.35,0.1)) +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.text.y = element_text(size = 14, colour = "black",face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

## queueing w lag (lag)
lag <- result %>% filter(lag=="lag") %>%
  filter(model=="reciprocal") %>%
  mutate(model="Lagged") %>%
  mutate(corpus=case_when(corpus=="ngram"~"Ngram",
                          .default = corpus)) %>%
  filter(variable=="lagged prestige") %>%
  ggplot(aes(x=prestige, y=Est.)) +
  geom_point(position=position_dodge(2),size=2.7,color="blue3")+
  geom_errorbar(aes(ymin=Est.-1.96*S.E., ymax=Est.+1.96*S.E.), width=0,
                position=position_dodge(2),color="blue3") +
  facet_grid(model ~ corpus, scales="free") +
  coord_flip() +
  ylab("Coefficient")  +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  scale_y_continuous(breaks=seq(-0.1,0.199,0.1),limits = c(-0.1,0.2)) +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.text.y = element_text(size = 14, colour = "black",face="bold"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

## Figure S17
ggarrange(cont,lag,ncol=1)
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/dpm_memo_lag.png", width = 12, height = 15, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### FWL first-difference model - Figure S11 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

change <-
  full_data %>%
  filter(corpus=="COCHA") %>%
  group_by(id) %>%
  dplyr::mutate(gender_change=mean(gender[year==1940],na.rm=T)-mean(gender[year==1900],na.rm=T),
                prestige_change=mean(prestige[year==1940],na.rm=T)-mean(prestige[year==1900],na.rm=T),
                potency_change=mean(potency[year==1940],na.rm=T)-mean(potency[year==1900],na.rm=T),
                income_change=mean(income[year==1940],na.rm=T)-mean(income[year==1900],na.rm=T),
                education_change=mean(education[year==1940],na.rm=T)-mean(education[year==1900],na.rm=T),
                count=max(count,na.rm=T)) %>%
  distinct(id,.keep_all = T) %>%
  filter(!is.na(gender_change))

## FWL
change$prestige_change <- resid(lm(prestige_change~education_change+income_change,change))
change$potency_change <- resid(lm(potency_change~education_change+income_change,change))
change$gender_change <- resid(lm(gender_change~education_change+income_change,change))

summary(lm(potency_change~gender_change,change))
library(ggrepel)

## extract first word
library(stringr)
change$occupation_word <- word(change$occupation_single, 1)
change$occupation_word <- gsub(",","",change$occupation_word)

## prestige change - 1900~1940
change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  filter(count > 0 & gender_change > -0.2) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=prestige_change,size=count)) +
  geom_jitter(color="blue3",fill="white",shape=21,width=0.015,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2,
              fullrange = TRUE,
              mapping = aes(weight = count)) +
  xlim(-0.18,0.15) +
  ylim(-0.17,0.2) +
  guides(size = FALSE) +
  xlab("Increase in Female Typing (Residual)") +
  ylab("Increase in General Prestige (Residual)") + 
  geom_text_repel(
    data = change %>% 
      distinct(gender_change,.keep_all = TRUE) %>%
      filter((gender_change>0.03 & prestige_change< -0.03)|
               (gender_change< -0.05 & prestige_change > 0.05)) %>%
      filter(!(occupation_word%in%c("professor","administrator","engineer","mechanic","plumber",
                                    "manager","huckster","operator","inspector"))),
    aes(label=occupation_word),
    size = 5,
    family = "Times",
    nudge_y = -0.01
  ) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/gender_prestige_change_0040.png", width = 13, height = 12, 
       units = "cm", dpi=600)

## potency change - 1900~1940
change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  filter(count > 0 & gender_change > -0.2) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=potency_change,size=count)) +
  geom_jitter(color="blue3",fill="white",shape=21,width=0.015,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2,
              fullrange = TRUE,
              mapping = aes(weight = count)) +
  xlim(-0.18,0.15) +
  ylim(-0.17,0.2) +
  guides(size = FALSE) +
  xlab("Increase in Female Typing (Residual)") +
  ylab("Increase in Potency (Residual)") + 
  geom_text_repel(
    data = change %>% 
      distinct(gender_change,.keep_all = TRUE) %>%
      filter((gender_change>0.03 & potency_change< -0.03)|
               (gender_change< -0.05 & potency_change > 0.05)) %>%
      filter(!(occupation_word%in%c("economist","professor","administrator","engineer","plumber",
                                    "attendant","housekeeper"))),
    aes(label=occupation_word),
    size = 5,
    family = "Times",
    nudge_y = -0.01
  ) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/gender_potency_change_0040.png", width = 13, height = 12, 
       units = "cm", dpi=600)

## prestige change - 1950~2019

change <-
  full_data %>%
  filter(corpus=="COCHA") %>%
  group_by(id) %>%
  dplyr::mutate(gender_change=mean(gender[year==2000],na.rm=T)-mean(gender[year==1950],na.rm=T),
                prestige_change=mean(prestige[year==2000],na.rm=T)-mean(prestige[year==1950],na.rm=T),
                potency_change=mean(potency[year==2000],na.rm=T)-mean(potency[year==1950],na.rm=T),
                income_change=mean(income[year==2000],na.rm=T)-mean(income[year==1950],na.rm=T),
                education_change=mean(education[year==2000],na.rm=T)-mean(education[year==1950],na.rm=T),
                count=max(count,na.rm=T)) %>%
  distinct(id,.keep_all = T) %>%
  filter(!is.na(gender_change))

## FWL
change$prestige_change <- resid(lm(prestige_change~education_change+income_change,change))
change$potency_change <- resid(lm(potency_change~education_change+income_change,change))
change$gender_change <- resid(lm(gender_change~education_change+income_change,change))

summary(lm(potency_change~gender_change,change))

## extract first word
library(stringr)
change$occupation_word <- word(change$occupation_single, 1)
change$occupation_word <- gsub(",","",change$occupation_word)

change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  filter(count > 0&gender_change>-0.16) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=prestige_change,size=count)) +
  geom_jitter(color="blue3",fill="white",shape=21,width=0.025,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2,aes(weight=count)) +
  guides(size = FALSE) +
  xlab("Increase in Female Typing (Residual)") +
  ylab("Increase in General Prestige (Residual)") +
  geom_text(
    data = change %>% 
      distinct(gender_change,.keep_all = TRUE) %>%
      filter((gender_change>0.05 & prestige_change< 0.01)|
               (gender_change < -0.06 & gender_change>-0.16&prestige_change<0.1)) %>%
      filter(occupation_word!="shoemaker"&occupation_word!="mechanic"&occupation_word!="counter"
             &occupation_word!="housekeeper"),
    aes(label=occupation_word),
    size = 5,
    check_overlap = T,
    family = "Times",
    nudge_y = -0.01
  ) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/gender_prestige_change_5009.png", width = 13, height = 12, units = "cm")

## potency change - 1950~2009

change <-
  full_data %>%
  filter(corpus=="COCHA") %>%
  group_by(id) %>%
  dplyr::mutate(gender_change=mean(gender[year==2010],na.rm=T)-mean(gender[year==1940],na.rm=T),
                prestige_change=mean(prestige[year==2010],na.rm=T)-mean(prestige[year==1940],na.rm=T),
                potency_change=mean(potency[year==2010],na.rm=T)-mean(potency[year==1940],na.rm=T),
                income_change=mean(income[year==2010],na.rm=T)-mean(income[year==1940],na.rm=T),
                education_change=mean(education[year==2010],na.rm=T)-mean(education[year==1940],na.rm=T),
                count=max(count,na.rm=T)) %>%
  distinct(id,.keep_all = T) %>%
  filter(!is.na(gender_change))

## FWL
change$prestige_change <- resid(lm(prestige_change~education_change+income_change,change))
change$potency_change <- resid(lm(potency_change~education_change+income_change,change))
change$gender_change <- resid(lm(gender_change~education_change+income_change,change))

change$occupation_word <- word(change$occupation_single, 1)
change$occupation_word <- gsub(",","",change$occupation_word)

change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  filter(count > 0&gender_change>-0.16&potency_change<0.1) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=potency_change,size=count)) +
  geom_jitter(color="blue",fill="white",shape=21,width=0.025,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2) +
  guides(size = FALSE) +
  xlab("Increase in Female Typing (Residual)") +
  ylab("Increase in Potency (Residual)") +
  geom_text(
    data = change %>% 
      filter(gender_change>-0.16) %>%
      filter(potency_change>-0.11&potency_change<0.1) %>%
      distinct(gender_change,.keep_all = TRUE) %>%
      filter((gender_change>0.05 & potency_change< 0.01)|
               (gender_change < -0.01 & gender_change>-0.15&potency_change>0.0)) %>%
      filter(!(occupation_word%in%c("collector","geologist","jeweler","dressmaker","housekeeper",
                                    "carpenter","metalsmith"))),
    aes(label=occupation_word),
    size = 5,
    check_overlap = T,
    family = "Times",
    nudge_y = -0.01
  ) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/gender_potency_change_5009.png", width = 13, height = 12, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Cross-Sectional Analysis - Figure S12 and S13 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## cross-sectional analysis only linear - Figure S12
result_linear <-
  data.frame(year=rep(seq(1900,2000,10),4),
             coef=rep(NA,44),
             se=rep(NA,44),
             prestige=c(rep("prestige",11),rep("potency",11),
                            rep("evaluation",11),rep("activity",11)))

## by year regression
for (p in c("prestige","potency","evaluation","activity")){
 
  ## for each year
  for (i in seq(1900,2000,10)){
    
    ## linear term coefficient
    result_linear[result_linear$year==i&result_linear$prestige==p,"coef"] <- 
      summary(
        lm(as.formula(paste(p,"~gender+factor(corpus)")),
                            full_data %>% filter(year==i))
      )$coef[2,1]
    
    ## linear term se
    result_linear[result_linear$year==i&result_linear$prestige==p,"se"] <- 
      summary(
        lm(as.formula(paste(p,"~gender+factor(corpus)")),
           full_data %>% filter(year==i))
      )$coef[2,2]
  }
  
}

## Figure S12
result_linear %>%
  mutate(sig=case_when((coef-1.96*se>=0)|(coef+1.96*se<=0) ~ "1",
                       .default = "0")) %>%
  ggplot(aes(x=year,y=coef)) +
  geom_point(aes(color=sig),size=2.7) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0.18) +
  theme_minimal() +
  geom_hline(aes(yintercept=0),lty=2,color="grey35") +
  scale_x_continuous(breaks=seq(1900,2000,20)) +
  scale_color_manual(values=c("blue3","red4")) +
  ylab("Coefficient") +
  ggtitle("First-Order Cross-Sectional Association") +
  facet_wrap(~prestige,ncol=4) +
  theme(text=element_text(family="Times",size=14),
        legend.position = "none",
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12,angle=45,vjust=0.65),
        axis.title = element_text(size=16))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/cross_sec_association.png", 
       width = 19.2, height = 8, units = "cm", dpi = 600)

## cross-sectional analysis both linear and quadratic - Figure S13
result_quad <-
  data.frame(year=rep(seq(1900,2000,10),4),
             coef=rep(NA,44),
             se=rep(NA,44),
             prestige=c(rep("prestige",11),rep("potency",11),
                        rep("evaluation",11),rep("activity",11)))

## by year regression
for (p in c("prestige","potency","evaluation","activity")){
  
  ## for each year
  for (i in seq(1900,2000,10)){
    
    ## linear term coefficient
    result_quad[result_quad$year==i&result_quad$prestige==p,"coef"] <- 
      summary(
        lm(as.formula(paste(p,"~gender+I(gender^2)+factor(corpus)")),
           full_data %>% filter(year==i))
      )$coef[3,1]
    
    ## linear term se
    result_quad[result_quad$year==i&result_quad$prestige==p,"se"] <- 
      summary(
        lm(as.formula(paste(p,"~gender+I(gender^2)+factor(corpus)")),
           full_data %>% filter(year==i))
      )$coef[3,2]
  }
  
}

## plot Figure S13
result_linear <- result_linear %>%
  mutate(sig=case_when((coef-1.96*se>=0)|(coef+1.96*se<=0) ~ "1",
                       .default = "0")) %>%
  ggplot(aes(x=year,y=coef)) +
  geom_point(aes(color=sig),size=2.7) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0.18) +
  theme_minimal() +
  geom_hline(aes(yintercept=0),lty=2,color="grey35") +
  scale_x_continuous(breaks=seq(1900,2000,20)) +
  scale_color_manual(values=c("blue3","red4")) +
  ylab("Coefficient") +
  ggtitle("Linear Term Coefficient") +
  facet_wrap(~prestige) +
  theme(text=element_text(family="Times",size=14),
        legend.position = "none",
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12,angle=45,vjust=0.65),
        axis.title = element_text(size=16))

result_quad <- result_quad %>%
  mutate(sig=case_when((coef-1.96*se>=0)|(coef+1.96*se<=0) ~ "1",
                       .default = "0")) %>%
  ggplot(aes(x=year,y=coef)) +
  geom_point(aes(color=sig),size=2.7) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0.18) +
  theme_minimal() +
  ylim(-2,2.5) +
  geom_hline(aes(yintercept=0),lty=2,color="grey35") +
  scale_x_continuous(breaks=seq(1900,2000,20)) +
  scale_color_manual(values=c("blue3","red4")) +
  ylab("Coefficient") +
  ggtitle("Quadratic Term Coefficient") +
  facet_wrap(~prestige) +
  theme(text=element_text(family="Times",size=14),
        legend.position = "none",
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12,angle=45,vjust=0.65),
        axis.title = element_text(size=16))

ggarrange(result_linear,
          result_quad,
          ncol=2)

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/cross_sec.png", 
       width = 21.2, height = 13, units = "cm", dpi = 600)

## Wald and F test

## for each year
for (i in seq(1900,2000,10)){
  
  ## model with both linear and quadratic terms
  model <- 
    lm(as.formula(paste("activity","~gender+I(gender^2)+factor(corpus)")),
         full_data %>% filter(year==i))
  
  ## perform joint test: H0: beta1 = beta2 = 0
  print(linearHypothesis(model, c("gender = 0", "I(gender^2) = 0")))
}

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
##### Longitudinal Analysis with Interaction - Table S6 and S7 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

demean <- 
  full_data %>%
  group_by(corpus,code) %>%
  mutate(prestige=prestige-mean(prestige,na.rm=T),
         potency=potency-mean(potency,na.rm=T),
         evaluation=evaluation-mean(evaluation,na.rm=T),
         activity=activity-mean(activity,na.rm=T),
         gender=gender-mean(gender,na.rm=T),
         income=income-mean(income,na.rm=T),
         education=education-mean(education,na.rm=T)
  ) %>%
  mutate(quadratic_gender=gender*gender) %>%
  group_by(corpus,code) %>%
  mutate(quadratic_gender=quadratic_gender-mean(quadratic_gender,na.rm=T))

## general prestige
summary(
  felm(prestige~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="COCHA",])
)
summary(
  felm(prestige~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="ngram",])
)

## potency
summary(
  felm(potency~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="COCHA",])
)
summary(
  felm(potency~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="ngram",])
)

## evaluation
summary(
  felm(evaluation~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="COCHA",])
)
summary(
  felm(evaluation~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="ngram",])
)

## activity
summary(
  felm(activity~gender+quadratic_gender+income+education
       | year | 0 | code,
     demean[demean$corpus=="COCHA",])
)
summary(
  felm(activity~gender+quadratic_gender+income+education
       | year | 0 | code,
       demean[demean$corpus=="ngram",])
)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Group Slopes - Figure S19 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

results_ngram <-
  data.frame(
    prestige=c("prestige","potency","evaluation","activity"),
    coef=rep(NA,4),
    se=rep(NA,4)
  )

for (p in c("prestige","potency","evaluation","activity")){
  results_ngram[results_ngram$prestige==p,"coef"] <-
    summary(
      felm(as.formula(paste(p, "~ gender + income + education + factor(category)*year | code + year | 0 | code")),
                            full_data[full_data$corpus=="ngram",])
    )$coef[1,1]
  
  results_ngram[results_ngram$prestige==p,"se"] <-
    summary(
      felm(as.formula(paste(p, "~ gender + income + education + factor(category)*year | code + year | 0 | code")),
           full_data[full_data$corpus=="ngram",])
    )$coef[1,2]
}

results_COCHA <-
  data.frame(
    prestige=c("prestige","potency","evaluation","activity"),
    coef=rep(NA,4),
    se=rep(NA,4)
  )

for (p in c("prestige","potency","evaluation","activity")){
  results_COCHA[results_COCHA$prestige==p,"coef"] <-
    summary(
      felm(as.formula(paste(p, "~ gender + income + education + factor(category)*year | code + year | 0 | code")),
           full_data[full_data$corpus=="COCHA",])
    )$coef[1,1]
  
  results_COCHA[results_COCHA$prestige==p,"se"] <-
    summary(
      felm(as.formula(paste(p, "~ gender + income + education + factor(category)*year | code + year | 0 | code")),
           full_data[full_data$corpus=="COCHA",])
    )$coef[1,2]
}

## plot
results_COCHA <-
  results_COCHA %>%
  mutate(prestige=factor(prestige,levels=c("prestige","potency","evaluation","activity"))) %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       coef-1.96*se>0~"1",
                       .default = "0")) %>%
  ggplot(aes(x=prestige,y=coef)) +
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ggtitle("COCHA, FEIS") +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("Prestige Dimension") +
  ylab("coefficient") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

results_ngram <-
  results_ngram %>%
  mutate(prestige=factor(prestige,levels=c("prestige","potency","evaluation","activity"))) %>%
  mutate(sig=case_when(coef+1.96*se<0~"1",
                       coef-1.96*se>0~"1",
                       .default = "0")) %>%
  ggplot(aes(x=prestige,y=coef)) +
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ggtitle("Ngram, FEIS") +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("Prestige Dimension") +
  ylab("coefficient") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

ggarrange(results_COCHA,
          results_ngram,ncol=2)

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/group_slope.png", 
       width = 18.5, height = 5.5, units = "cm", dpi = 600)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Count Weighted Occupation - Figure S5 #####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Misc.')
full_data <- data.frame()

## COCHA
data <- read.csv("COCHA_weight.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data$corpus <- "COCHA"
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
full_data <- rbind(full_data,data)

## Ngram
data <- read.csv("ngram_weight.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
data$corpus <- "ngram"
full_data <- rbind(full_data,data)

## merge with 1950 occupation official code
code <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main/1950occcode.csv")
code$occ <- gsub('n.e.c.', 'nec', code$occ)
full_data$occupation_origin <- gsub('n.e.c.', 'nec', full_data$occupation_origin)
full_data <- merge(full_data, code, by.y="occ", by.x="occupation_origin", all.x = T)
rm(code)

## merge with 1950 occupation actual measures
occ1950 <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main/occ1950.csv")
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
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ylim(-0.25,0.1) +
  scale_x_discrete(labels= c("full","1","2","3","4")) +
  facet_wrap(.~outcome, ncol=4) +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  ggtitle("Ngram, 1900-2009") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/count_weighted_ngram.png", 
       width = 17, height = 5.5, units = "cm", dpi = 600)

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
  geom_point(aes(color=sig),size=3) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se,color=sig), width=0,linewidth=0.58) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_minimal() +
  ylim(-0.25,0.07) +
  scale_x_discrete(labels= c("full","1","2","3","4")) +
  facet_grid(.~outcome) +
  ggtitle("COCHA, 1900-2019") +
  scale_color_manual(values=c("blue3","red4")) +
  xlab("parallel trend window (decades)") +
  ylab("coefficient") +
  theme(legend.position = "none",
        text=element_text(family="Times",size=14),
        plot.title=element_text(hjust=0.5,face="bold",size=14),
        strip.text.x = element_text(size = 14, colour = "black"),
        panel.spacing.x = unit(0.75, "lines"))

## save figure
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/R&R/count_weighted_COCHA.png", 
       width = 17, height = 5.5, units = "cm", dpi = 600)
