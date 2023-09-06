library(dplyr)
library(lfe)
library(dpm)
library(plm)
library(ggplot2)
library(ggpubr)
library(zoo)
library(reshape2)
library(Rtsne)
library(ggrepel)
library(stringr)

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main')
full_data <- data.frame()

## COHA
data <- read.csv("COHA.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data$corpus <- "COHA"
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

## COCA
data <- read.csv("COCA.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
data$corpus <- "COCA"
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
  group_by(occupation_origin) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  as.data.frame()

## single occupation name
full_data$occupation_word <- word(full_data$occupation_single, 1)
full_data$occupation_word <- gsub(",","",full_data$occupation_word)

## occupation category
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

#~#~#~#~#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#
#~#~#~#~#~Cross-Sectional Gender - Prestige~#~#~#~#~#~#
#~#~#~#~#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#

result <- data.frame(
  year=c(rep(seq(1900,2000,10),2),seq(1910,2010,10)),
  coef=rep(NA,33),
  se=rep(NA,33),
  corpus=c(rep("Ngram",11),rep("COHA",11),rep("COCA",11))
)

## year-specific cross-sectional
for (i in seq(1900,2000,10)){
  result[result$year==i&result$corpus=="Ngram","coef"] <- 
    summary(lm(prestige~I(gender^2)+gender+income+education+moral,full_data[which(full_data$year==i&full_data$corpus=="ngram"),]))$coef[2,1]
  result[result$year==i&result$corpus=="Ngram","se"] <- 
    summary(lm(prestige~I(gender^2)+gender+income+education+moral,full_data[which(full_data$year==i&full_data$corpus=="ngram"),]))$coef[2,2]
  
  result[result$year==i&result$corpus=="COHA","coef"] <- 
    summary(lm(prestige~I(gender^2)+gender+income+education+moral,full_data[which(full_data$year==i&full_data$corpus=="COHA"),]))$coef[2,1]
  result[result$year==i&result$corpus=="COHA","se"] <- 
    summary(lm(prestige~I(gender^2)+gender+income+education+moral,full_data[which(full_data$year==i&full_data$corpus=="COHA"),]))$coef[2,2]
}

for (i in seq(1990,2010,10)){
  result[result$year==i&result$corpus=="COCA","coef"] <- 
    summary(lm(prestige~I(gender^2)+gender+income+education+moral,full_data[which(full_data$year==i&full_data$corpus=="COCA"),]))$coef[2,1]
  result[result$year==i&result$corpus=="COCA","se"] <- 
    summary(lm(prestige~I(gender^2)+gender+income+education+moral,full_data[which(full_data$year==i&full_data$corpus=="COCA"),]))$coef[2,2]
}

## 3-year moving average (smooth plot)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

result %>%
  filter(!is.na(coef)) %>%
  mutate(corpus = factor(corpus, levels=c("Ngram","COHA","COCA"))) %>%
  arrange(desc(year)) %>%
ggplot(aes(x=year, y=coef)) +
  geom_point(position=position_dodge(5),size=2.7,color="blue")+
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=2,
                position=position_dodge(5),color="blue") +
  facet_grid(. ~ corpus, scales="free") +
  coord_flip() +
  xlim(1900,2020) +
  ylab("Coefficient") +
  xlab("Year") +
  ylim(-5,5) +
  scale_x_reverse(breaks = seq(1900, 2010, by=10)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  theme_classic() +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_blank(),
        axis.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/cross_sectional_deval_ushape.png", width = 18, height = 9.6, units = "cm")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#
#~#~#~#~#~Longitudinal FE Gender - Prestige~#~#~#~#~#~#
#~#~#~#~#~#~#~#~#~#~#~#~#~#~##~#~#~#~#~#~#~#~#~#~#~#~#~#

## store results
result <- data.frame(
  coef=rep(NA,24),
  se=rep(NA,24),
  corpus=c(rep("ngram",8),rep("COHA",8),rep("COCA",8)),
  variable=rep(c("female","affluence","cultivation","morality"),6),
  control=rep(c(rep("wocontrol",4),rep("wcontrol",4)),3)
)

for (cor in c("ngram","COHA","COCA")){
  
  ## w/ control
  regression <- 
    summary(
    felm(prestige ~ gender + income + education + moral  | id + year | 0 | id, full_data[full_data$corpus==cor, ])
  )
  ## coef
  result[result$corpus==cor & result$control == "wcontrol","coef"] <-
    regression$coef[1:4,1]
  
  ## se
  result[result$corpus==cor & result$control == "wcontrol","se"] <-
    regression$coef[1:4,2]
  
  ## w/o control
  regression <- 
    summary(
      felm(prestige ~ gender | id + year | 0 | id, full_data[full_data$corpus==cor, ])
    )
  ## coef
  result[result$corpus==cor & result$control == "wocontrol","coef"] <-
    regression$coef[1,1]
  
  ## se
  result[result$corpus==cor & result$control == "wocontrol","se"] <-
    regression$coef[1,2]
}

result[which(result$control=="wocontrol"&result$variable!="female"),"coef"] <- NA
result[which(result$control=="wocontrol"&result$variable!="female"),"se"] <- NA

result[result$corpus=="ngram","corpus"] <- "Ngram"

library("ggh4x")
result %>%
  mutate(corpus = factor(corpus, levels=c("Ngram","COHA","COCA")),
         variable = factor(variable,levels=c("morality","cultivation","affluence","female"))) %>%
  ggplot(aes(x=variable,y=coef,group=control,color=control,shape=control)) +
  geom_point(position=position_dodge(0.6),size=2.7) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0.25,
                position=position_dodge(0.6)) +
  ylim(-0.3,0.3) +
  facet_grid(. ~ corpus, scales="free") +
  facetted_pos_scales(
    y = list(
      corpus == "ngram" ~ scale_y_continuous(limits = c(-0.1,0.3)),
      corpus == "COHA" ~ scale_y_continuous(limits = c(-0.35,0.35)),
      corpus == "COCA" ~ scale_y_continuous(limits = c(-0.7,0.7))
    )
  ) +
  coord_flip() +
  theme_bw() + 
  ylab("coefficient") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  scale_color_manual(name = "", labels=c("with control","bivariate"), values=c("blue","green4")) +
  scale_shape_manual(name = "", labels=c("with control","bivariate"), values=c(19,19)) +
  scale_x_discrete(labels=c("morality","cultivation","affluence","female"),
                   expand=c(0.3,0.3)) +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_blank(),
        axis.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))
  
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/twfe.png", width = 18, height = 9, units = "cm")

####### plot first-difference model FWL #######
change <-
  full_data %>%
  filter(corpus=="COHA") %>%
  group_by(id) %>%
  dplyr::mutate(gender_change=mean(gender[year==1940],na.rm=T)-mean(gender[year==1900],na.rm=T),
                prestige_change=mean(prestige[year==1940],na.rm=T)-mean(prestige[year==1900],na.rm=T),
                income_change=mean(income[year==1940],na.rm=T)-mean(income[year==1900],na.rm=T),
                education_change=mean(education[year==1940],na.rm=T)-mean(education[year==1900],na.rm=T),
                count=max(count,na.rm=T)) %>%
  distinct(id,.keep_all = T) %>%
  filter(!is.na(gender_change))

## FWL
change$prestige_change <- resid(lm(prestige_change~education_change+income_change,change))
change$gender_change <- resid(lm(gender_change~education_change+income_change,change))

summary(lm(prestige_change~gender_change,change))
library(ggrepel)

## extract first word
library(stringr)
change$occupation_word <- word(change$occupation_single, 1)
change$occupation_word <- gsub(",","",change$occupation_word)

## gender change - 1900~1950
change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  filter(count > 0 & gender_change > -0.2) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=prestige_change,size=count)) +
  geom_jitter(color="blue",fill="white",shape=21,width=0.015,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2,
              fullrange = TRUE,
              mapping = aes(weight = count)) +
  xlim(-0.18,0.15) +
  ylim(-0.17,0.2) +
  guides(size = FALSE) +
  xlab("Increase in Feminine Stereotypes (Residual)") +
  ylab("Increase in Prestige (Residual)") + 
  geom_text(
    data = change %>% 
      distinct(gender_change,.keep_all = TRUE) %>%
      filter((gender_change>0.01 & prestige_change< -0.03)|
               (gender_change< -0.05 & prestige_change > 0.05)) %>%
      filter(occupation_word!="administrator"&occupation_word!="engineer"&occupation_word!="statistician"),
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

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/gender_prestige_change_0040.png", width = 13, height = 12, 
       units = "cm", dpi=600)

## gender change - 1950~1999

## extract first word
library(stringr)
change$occupation_word <- word(change$occupation_single, 1)
change$occupation_word <- gsub(",","",change$occupation_word)

change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  filter(count > 0&gender_change>-0.2) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=prestige_change,size=count)) +
  geom_jitter(color="blue",fill="white",shape=21,width=0.025,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2,aes(weight=count)) +
  guides(size = FALSE) +
  xlab("Increase in Gender Stereotyping (Residual)") +
  ylab("Increase in Prestige (Residual)") +
  geom_text(
    data = change %>% 
      distinct(gender_change,.keep_all = TRUE) %>%
      filter((gender_change>0.01 & prestige_change< 0.06)|
               (gender_change < -0.05 & gender_change>-0.2&prestige_change>0.1)) %>%
      filter(occupation_word!="shoemaker"&occupation_word!="mechanic"&occupation_word!="counter"),
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

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/gender_prestige_change_5090.png", width = 13, height = 12, units = "cm")


####### marginal effect #######
library(margins)

## ngram
fe <- lm(prestige ~ gender + income + education + factor(id) + factor(year)*gender, full_data%>%filter(corpus=="ngram"))
margin_ngram <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2000,10))))
margin_ngram$corpus <- "ngram"

## COHA
fe <- lm(prestige ~ gender + income + education + factor(id) + factor(year)*gender, full_data[full_data$corpus=="COHA", ])
margin_coha <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2000,10))))
margin_coha$corpus <- "COHA"

## COCA
fe <- lm(prestige ~ gender + income + education + factor(id) + factor(year)*gender, full_data[full_data$corpus=="COCA", ])
margin_coca <- summary(margins(fe, variables = "gender", at = list(year = seq(1990,2010,10))))
margin_coca$corpus <- "COCA"

## bind
margin <- rbind(margin_ngram,margin_coha,margin_coca)

## 3-year moving average (smooth plot)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

ggplot(margin, aes(x=year,y=three_ma(AME),group=corpus,color=corpus,shape=corpus)) +
  geom_point(position=position_dodge(5),size=1.5)+
  geom_errorbar(aes(ymin=three_ma(AME)-1.96*SE, ymax=three_ma(AME)+1.96*SE), width=0,
                position=position_dodge(5), size=0.5) +
  xlim(1900,2020)  +
  ylim(-0.6,0.3) +
  ylab("AME") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1900, 2010, by=10)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  scale_color_manual(name = "Corpus", labels=c("COCA","COHA","Ngram"), values=c("brown","green4","darkblue")) +
  scale_shape_manual(name = "Corpus", labels=c("COCA","COHA","Ngram"), values=c(21,22,24)) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(angle = 45,vjust=0.9, hjust=0.9,size=14),
        axis.text.y=element_text(size=14),
        axis.title.x = element_blank(),
        axis.title = element_text(size=14))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/margin_wocontrol.png", width = 11, height = 11.3, units = "cm")


###~###~####~###~###~###~ occupation heterogeneity ###~###~####~###~###~###~

data <- full_data

incgroup <-
  data %>%
  group_by(code) %>%
  summarize(wage_male_quantile=mean(wage_male[year>=1950&year<=2010],na.rm=T)) %>%
  mutate(incgroup = case_when(wage_male_quantile <= quantile(wage_male_quantile, 0.25, na.rm=T) ~ 1,
                              wage_male_quantile > quantile(wage_male_quantile, 0.25, na.rm=T) &
                                wage_male_quantile <= quantile(wage_male_quantile, 0.5, na.rm=T) ~ 2,
                              wage_male_quantile > quantile(wage_male_quantile, 0.5, na.rm=T) &
                                wage_male_quantile <= quantile(wage_male_quantile, 0.75, na.rm=T) ~ 3,
                              wage_male_quantile > quantile(wage_male_quantile, 0.75, na.rm=T) ~ 4))


data <- merge(data,incgroup[,c("code","incgroup")],by=c("code"),all.x=T)

data <- data %>%
  group_by(corpus,code) %>%
  mutate(id = cur_group_id())

########## w control ###########

## prepare output
result <- data.frame(
  incgroup = rep(seq(1,4,1),3),
  coef=rep(NA,12),
  se=rep(NA,12),
  range=c(rep("full",4),rep("thirty",4),rep("sixty",4))
)

## full range
for (j in seq(1,4,1)){
  reg <- summary(
    felm(prestige ~ gender + education + income + moral | id + year | 0 | id, data %>% 
           filter(incgroup==j)))
  result[which(result$incgroup==j&result$range=="full"),"coef"] <- reg$coef[1,1]
  result[which(result$incgroup==j&result$range=="full"),"se"] <- reg$coef[1,2]
}


for (j in seq(1,4,1)){
  df <-
    difference(data[data$incgroup==j&data$corpus!="COCA", ] %>%
                 group_by(id) %>%
                 mutate(count=sum(!is.na(gender))) %>%
                 filter(count==11) %>%
                 as.data.frame(),
               k=10,
               outcome="prestige",treatment="gender",control = c("income","education","moral"))
  FD_results <- FEdecompose(df,k=10,outcome="prestige",treatment="gender",control = c("income","education","moral"))
  ## 20-year gap
  result[which(result$incgroup==j&result$range=="thirty"),"coef"] <- 
    sum((FD_results$FD_regression*FD_results$weight)[1:2])/(sum(FD_results$weight[1:2])/sum(FD_results$weight[1:10]))
  result[which(result$incgroup==j&result$range=="thirty"),"se"] <- 
    sum((FD_results$FD_SD*FD_results$weight)[1:2])/(sum(FD_results$weight[1:2])/sum(FD_results$weight[1:10]))
  
  ## 50-year gap
  result[which(result$incgroup==j&result$range=="sixty"),"coef"] <- 
    sum((FD_results$FD_regression*FD_results$weight)[1:5])/(sum(FD_results$weight[1:5])/sum(FD_results$weight[1:10]))
  result[which(result$incgroup==j&result$range=="sixty"),"se"] <- 
    sum((FD_results$FD_SD*FD_results$weight)[1:5])/(sum(FD_results$weight[1:5])/sum(FD_results$weight[1:10]))
}

result_1 <- result

########## w/o control ###########

## prepare output
result <- data.frame(
  incgroup = rep(seq(1,4,1),3),
  coef=rep(NA,12),
  se=rep(NA,12),
  range=c(rep("full",4),rep("thirty",4),rep("sixty",4))
)

## full range
for (j in seq(1,4,1)){
  reg <- summary(
    felm(prestige ~ gender | id + year | 0 | id, data %>% 
           filter(incgroup==j)))
  result[which(result$incgroup==j&result$range=="full"),"coef"] <- reg$coef[1,1]
  result[which(result$incgroup==j&result$range=="full"),"se"] <- reg$coef[1,2]
}


for (j in seq(1,4,1)){
  df <-
    difference(data[data$incgroup==j&data$corpus!="COCA", ] %>%
                 group_by(id) %>%
                 mutate(count=sum(!is.na(gender))) %>%
                 filter(count==11) %>%
                 as.data.frame(),
               k=10,
               outcome="prestige",treatment="gender")
  FD_results <- FEdecompose(df,k=10,outcome="prestige",treatment="gender")
  ## 20-year gap
  result[which(result$incgroup==j&result$range=="thirty"),"coef"] <- 
    sum((FD_results$FD_regression*FD_results$weight)[1:2])/(sum(FD_results$weight[1:2])/sum(FD_results$weight[1:10]))
  result[which(result$incgroup==j&result$range=="thirty"),"se"] <- 
    sum((FD_results$FD_SD*FD_results$weight)[1:2])/(sum(FD_results$weight[1:2])/sum(FD_results$weight[1:10]))
  
  ## 50-year gap
  result[which(result$incgroup==j&result$range=="sixty"),"coef"] <- 
    sum((FD_results$FD_regression*FD_results$weight)[1:5])/(sum(FD_results$weight[1:5])/sum(FD_results$weight[1:10]))
  result[which(result$incgroup==j&result$range=="sixty"),"se"] <- 
    sum((FD_results$FD_SD*FD_results$weight)[1:5])/(sum(FD_results$weight[1:5])/sum(FD_results$weight[1:10]))
}

result_2 <- result

## combine
result_1$control <- "wcontrol"
result_2$control <- "wocontrol"
result <- rbind(result_1,result_2)

label <- c(`full`="Full Range",`thirty`="20-year Gap",`sixty`="50-year Gap")
result %>%
  mutate(range = factor(range, levels=c("full","thirty","sixty")),
         incgroup = factor(incgroup,levels=c(4,3,2,1))) %>%
  ggplot(aes(x=incgroup,y=coef,group=control,color=control,shape=control)) +
  geom_point(position=position_dodge(0.55),size=2.7) +
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0.18,
                position=position_dodge(0.55)) +
  scale_color_manual(name = "", labels=c("with control","bivariate"), values=c("blue","green4")) +
  scale_shape_manual(name = "", labels=c("with control","bivariate"), values=c(19,19)) +
  scale_x_discrete(expand=c(0.3,0.3)) +
  facet_grid(. ~ range, scales="free", labeller = as_labeller(label)) +
  coord_flip() +
  ylim(-0.5,0.3) +
  theme_classic() + 
  ylab("coefficient") +
  xlab("Income Quartile") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/occ_hetero.png", width = 17, height = 9.6, units = "cm")

## verify prestige measure
summary(
  lm(presgl/100 ~ prestige, full_data %>% filter(corpus=="ngram",year==1960&presgl>0))
)

summary(
  lm(prent90/100 ~ prestige, data %>% filter(year==1980&presgl>0))
)


## baseline with other measures
summary(
  lm(presgl ~ npboss, full_data %>% filter(corpus=="ngram",year==1960&presgl>0))
)
summary(
  lm(presgl ~ eduscore, full_data %>% filter(corpus=="ngram",year==1960&presgl>0))
)
summary(
  lm(presgl ~ earnscore, full_data %>% filter(corpus=="ngram",year==1960&presgl>0))
)

summary(
  lm(prent90 ~ npboss, full_data %>% filter(corpus=="ngram",year==1960&presgl>0))
)
summary(
  lm(prent90 ~ earnscore, data %>% filter(year==1990&presgl>0))
)
summary(
  lm(prent90 ~ eduscore, data %>% filter(year==1990&presgl>0))
)

## plot prestige measures
prestige <-
  full_data %>% filter(corpus=="ngram") %>%
  filter(year==1970&presgl>0) %>%
  ggplot(aes(prestige,presgl,
             size=count^(2))) +
  geom_point(alpha=0.6, color="grey25") +
  geom_smooth(method='lm', formula= y~x, color="darkred", 
              show.legend = FALSE, se=T) +
  xlab("Text-based Prestige") +
  ylab("Siegel Prestige") +
  theme_bw() +
  guides(size = FALSE) + 
  theme(text=element_text(family="Times"))

npboss <-
  data %>%
  filter(year==1960&presgl>0) %>%
  ggplot(aes(npboss,presgl,
             size=count^(2))) +
  geom_point(alpha=0.6, color="grey25") +
  geom_smooth(method='lm', formula= y~x, color="darkred", 
              show.legend = FALSE, se=T) +
  xlab("NPBOSS") +
  ylab("Siegel Prestige") +
  theme_bw() +
  guides(size = FALSE) + 
  theme(text=element_text(family="Times"))

ggarrange(prestige, npboss)

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/prestige_siegel_npboss.png", width = 15, height = 8, units = "cm")

rm(duncansei,prestige,occ1950)

## correlation matrix
data <- full_data
correlation <- data[,c("year","prestige","gender","id","corpus")] %>%
  filter(corpus=="COHA") %>%
  arrange(year,id) 

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
  prestige_2000 = correlation[which(correlation$year==2000),]$prestige
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
  scale_x_discrete(labels=seq(1900,2000,10)) +
  scale_y_discrete(labels=seq(1900,2000,10)) +
  xlab("Year") +
  ylab("Year") + 
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "white", size = 3, family="Times") +
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

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/prestige_cor_COHA.png", width = 10, height = 10, units = "cm")


####### Change of Gender Stereotypes #######

library(boot)
library(tidyr)

boot_mean <- function(original_vector, resample_vector) {
  mean(original_vector[resample_vector],na.rm=T)
}

## COHA
t <-
  full_data %>%
  filter(corpus=="COHA") %>%
  group_by(year) %>%
  summarise(mean_gender = mean(gender,na.rm=T),
            sd_gender = sd(boot(gender, boot_mean, R = 2000)$t),
            ## percent female
            mean_f = (weighted.mean(percent_f, w=count,na.rm=T) - 0.65)/4.6,
            sd_f = sd(boot(percent_f, boot_mean, R = 2000)$t)/4.6) %>%
  pivot_longer(contains("_"), names_to = c(".value", "variable"), names_sep = "_") %>%
  group_by(variable) %>%
  mutate(mean=three_ma(mean))

t %>%
  ggplot(aes(x=year,y=mean,group=variable)) +
  geom_point(aes(color=variable), size=2.7) +
  geom_line(aes(color=variable)) +
  geom_ribbon(aes(y = mean, ymin = mean-sd, ymax = mean+sd,
                  group=variable,fill=variable), 
                  alpha=0.2,
                  show.legend=FALSE) +
  scale_y_continuous("Smoothed Female Stereotypes",
                    sec.axis = sec_axis( ~ .*(4.6) + 0.65, name = "Proportion Female")) +
  scale_x_continuous(n.breaks = 10, limits = c(1900,2000)) +
  theme_classic() +
  scale_color_manual(name = "", labels=c("Proportion Female","Female Stereotypes"), values=c("blue3","green4")) +
  scale_fill_manual(name = "", labels=c("Proportion Female","Female Stereotypes"), values=c("blue3","green4")) +
  theme(text=element_text(family="Times",size=14),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/trend_COHA.png", width = 15, height = 15, units = "cm")


## ngram
t <-
  full_data %>%
  filter(corpus=="ngram"&!is.na(count)&!is.na(gender)) %>%
  group_by(year) %>%
  summarise(mean_gender = weighted.mean(gender,w=count,na.rm=T),
            sd_gender = sd(boot(gender, boot_mean, R = 2000)$t),
            ## percent female
            mean_f = (weighted.mean(percent_f, w=count,na.rm=T) - 0.4)/11.5,
            sd_f = sd(boot(percent_f, boot_mean, R = 2000)$t)/11.5) %>%
  pivot_longer(contains("_"), names_to = c(".value", "variable"), names_sep = "_") %>%
  group_by(variable) %>%
  mutate(mean=three_ma(mean))

t %>%
  ggplot(aes(x=year,y=mean,group=variable)) +
  geom_point(aes(color=variable), size=2.7) +
  geom_line(aes(color=variable)) +
  geom_ribbon(aes(y = mean, ymin = mean-sd, ymax = mean+sd,
                  group=variable,fill=variable), 
              alpha=0.2,
              show.legend=FALSE) +
  scale_y_continuous("Smoothed Female Stereotypes",
                     sec.axis = sec_axis( ~ .*(11.5) + 0.4, name = "Proportion Female")) +
  scale_x_continuous(n.breaks = 10, limits = c(1900,2000)) +
  theme_classic() +
  scale_color_manual(name = "", labels=c("Proportion Female","Female Stereotypes"), values=c("blue3","green4")) +
  scale_fill_manual(name = "", labels=c("Proportion Female","Female Stereotypes"), values=c("blue3","green4")) +
  theme(text=element_text(family="Times",size=14),
        legend.text = element_text(size=16),
        legend.title = element_text(size=14),
        legend.position = "bottom",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/trend_ngram.png", width = 15, height = 15, units = "cm")



####### cross-sectional #########

full_data %>%
  mutate(occupation_word = word(occupation_single,1),
         occupation_word = gsub(",","",occupation_word)) %>%
  dplyr::filter(corpus=="ngram"&year==2000) %>%
  filter(gender>-0.2) %>%
  ggplot(aes(x=gender,y=prestige,size=count))+
  ylim(-0.1,0.25) +
  geom_jitter(color="blue",fill="white",shape=21,width=0.02) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=T,alpha=0.2) +
  guides(size = "none") +
  xlab("Feminine Stereotypes") +
  ylab("Prestige") +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16)) +
  geom_text( 
    data = full_data %>%
      mutate(occupation_word = word(occupation_single,1),
             occupation_word = gsub(",","",occupation_word)) %>%
      filter((corpus=="ngram"&year==2000)) %>%
      filter(gender>-0.2) %>%
      ungroup() %>%
      distinct(occupation_word,.keep_all = T),
    aes(label=occupation_word),
    check_overlap = T,
    size=5,
    family="Times",
    nudge_x=-0.02
  )

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/gender_prestige_ngram_2000.png", width = 13, height = 12, 
       units = "cm", dpi=600) 







