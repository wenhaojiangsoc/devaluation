library(dplyr)
library(lfe)
library(ggplot2)

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
  filter(occupation_single!="retired") %>%
  group_by(occupation_origin) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  as.data.frame()

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

############# Main Analyses ###############

summary(
  felm(wage_fem ~ percent_f + edu_fem + lmexp_fem + I(lmexp_fem^2) + hours_fem | id + year | 0 | id, 
       full_data[full_data$corpus=="COHA", ])
)

summary(
  felm(wage_male ~ percent_f + edu_male + lmexp_male + I(lmexp_male^2) + hours_male | id + year | 0 | id, 
       full_data[full_data$corpus=="COHA" & full_data$year<=1960, ])
)

summary(
  felm(prestige ~ gender | id + year | 0 | id, full_data[full_data$corpus=="ngram", ])
)


## TWFE
summary(
  felm(prestige ~ gender + income + education + moral | id + year | 0 | id, full_data[full_data$corpus=="ngram", ])
)

## FEIS
summary(
  felm(prestige ~ gender + income + education + moral + factor(category)*year | id + year | 0 | id, 
       full_data[full_data$corpus=="COCA", ])
)

## decompose
df <-
  difference(full_data[full_data$corpus=="ngram", ] %>%
               group_by(id) %>%
               mutate(count=sum(!is.na(gender))) %>%
               filter(count==11) %>%
               as.data.frame(),
               k=10,
             outcome="prestige",treatment="gender")
FD_results <- FEdecompose(df,k=10,outcome="prestige",treatment="gender")

### plot
cols <- c("FD Coefficient"="blue","TWFE"="red3","Weight"="grey")
adjust <- 0.55
FD_results %>%
  mutate(gap=k*10) %>%
  ggplot(aes(x=gap,y=FD_regression+adjust)) +
  geom_point(aes(color="FD Coefficient"),shape=19,size=2.7) +
  scale_y_continuous(sec.axis = sec_axis(~ .-adjust, name = "First-Different Coefficient"),
                     limits = c(0,0.8)) +
  geom_bar(aes(x=gap, y=weight,fill="Weight"),stat="identity",color="grey", width=9, alpha=0.2) +
  geom_errorbar(aes(ymin=FD_regression-1.96*FD_SD+adjust, ymax=FD_regression+1.96*FD_SD+adjust,color="FD Coefficient"), width=2.5) +
  ylab("Weight") +
  xlab("Gap Years") +
  scale_x_continuous(breaks=seq(10,100,10)) +
  geom_hline(aes(yintercept=summary(felm(prestige ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="ngram", ]))$coef[1,1]+adjust,
                 linetype="TWFE"), color = "red3") +
  geom_hline(yintercept = adjust, color = "grey50", lty = 2) +
  scale_color_manual(name=NULL,values=cols) +
  scale_fill_manual(name=NULL,values=cols) +
  scale_linetype_manual(name = NULL, values=4,
                        guide = guide_legend(override.aes = list(color = "red3"))) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black', size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        plot.title = element_text(size=11,hjust=0.5),
        axis.title.x = element_text(size=13),
        text=element_text(family="Times",size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=17))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/twfe_decomp_ngram.png", width = 11.5, height = 14, units = "cm")

## decompose
df <-
  difference(full_data[full_data$corpus=="ngram", ] %>%
               group_by(id) %>%
               mutate(count=sum(!is.na(gender))) %>%
               filter(count==11) %>%
               as.data.frame(),
             k=10,
             outcome="prestige",treatment="gender",c("income","education","moral"))
FD_results <- FEdecompose(df,k=10,outcome="prestige",treatment="gender",c("income","education","moral"))

### plot
cols <- c("FD Coefficient"="blue","TWFE"="red3","Weight"="grey","bias"="green4")
adjust <- 0.55
FD_results %>%
  mutate(gap=k*10) %>%
  ggplot(aes(x=gap,y=FD_regression+adjust)) +
  geom_point(aes(color="FD Coefficient"),shape=19,size=2.7) +
  scale_y_continuous(sec.axis = sec_axis(~ .-adjust, name = "First-Different Coefficient"),
                     limits = c(0,0.8)) +
  geom_bar(aes(x=gap, y=weight,fill="Weight"),stat="identity",color="grey", width=9, alpha=0.2) +
  geom_errorbar(aes(ymin=FD_regression-1.96*FD_SD+adjust, ymax=FD_regression+1.96*FD_SD+adjust,color="FD Coefficient"), width=2.5) +
  ylab("Weight") +
  xlab("Gap Years") +
  scale_x_continuous(breaks=seq(10,100,10)) +
  geom_hline(aes(yintercept=summary(felm(prestige ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="ngram", ]))$coef[1,1]+adjust,
                 linetype="TWFE"), color = "red3") +
  geom_hline(aes(yintercept=summary(felm(prestige ~ gender + income + education | id + year | 0 | id, full_data[full_data$corpus=="ngram", ]))$coef[1,1]-
                   sum(FD_results$FD_regression*FD_results$weight)+adjust,
                 linetype="bias"), color = "green4") +
  geom_hline(yintercept = adjust, color = "grey50", lty = 2) +
  scale_color_manual(name=NULL,values=cols) +
  scale_fill_manual(name=NULL,values=cols) +
  scale_linetype_manual(name = NULL, values=c(4,3),
                        guide = guide_legend(override.aes = list(color = c("green4","red3")))) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black', size = 0.2),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),size=13),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        plot.title = element_text(size=11,hjust=0.5),
        axis.title.x = element_text(size=13),
        text=element_text(family="Times",size=13),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=17))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/twfe_decomp_ngram_wcontrol.png", width = 11.5, height = 14, units = "cm")



## heterogeneity over time
result <- data.frame()

## 3-year moving average (smooth plot)
library(zoo)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

## marginal effect
library(margins)
for (c in c("ngram","COHA","COCA")){
  fe <- lm(prestige ~ gender + income + education + factor(id) + factor(year)*gender, full_data[full_data$corpus==c, ])
  if (c!="COCA"){
    margin_ngram <- summary(margins(fe, variables = "gender", at = list(year = seq(1900,2000,10)))) 
  } else{
    margin_ngram <- summary(margins(fe, variables = "gender", at = list(year = seq(1990,2010,10))))
  }
  margin_ngram$AME <- three_ma(margin_ngram$AME)
  margin_ngram$corpus <- c
  result <- rbind(result, margin_ngram)
}

result[result$corpus=="ngram","corpus"] <- "Ngram"
result %>%
  mutate(corpus = factor(corpus, levels=c("Ngram","COHA","COCA"))) %>%
  arrange(desc(year)) %>%
  ggplot(aes(x=year, y=AME)) +
  geom_point(position=position_dodge(5),size=2.7,color="blue")+
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE), width=2.5,
                position=position_dodge(5),color="blue") +
  facet_grid(. ~ corpus, scales="free") +
  coord_flip() +
  xlim(1900,2020) +
  ylim(-0.6,0.5) +
  ylab("Coefficient") +
  xlab("Year") +
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

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/temporal_heterogeneity.png", width = 18, height = 9.6, units = "cm")

##### plot first-difference model #####
change <-
  data %>%
  group_by(id) %>%
  dplyr::mutate(gender_change=mean(gender[year==1990],na.rm=T)-mean(gender[year==1960],na.rm=T),
         prestige_change=mean(prestige[year==1990],na.rm=T)-mean(prestige[year==1960],na.rm=T),
         income_change=mean(income[year==1990],na.rm=T)-mean(income[year==1960],na.rm=T),
         education_change=mean(education[year==1990],na.rm=T)-mean(education[year==1960],na.rm=T),
         count=max(count,na.rm=T)) %>%
  distinct(id,.keep_all = T) %>%
  filter(!is.na(gender_change))

## FWL
change$prestige_change <- resid(lm(prestige_change~education_change+income_change,change))
change$gender_change <- resid(lm(gender_change~education_change+income_change,change))

summary(lm(prestige_change~gender_change,change))

## gender change
change %>% 
  distinct(gender_change,.keep_all = TRUE) %>%
  as.data.frame() %>%
  ggplot(aes(x=gender_change,y=prestige_change,size=count)) +
  geom_jitter(color="#3A8FB7",fill="white",shape=21,width=0.025,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4", 
              mapping = aes(weight = count), 
              show.legend = FALSE, se=T,alpha=0.2) +
  guides(size = FALSE) +
  xlab("Increase in Feminine Stereotypes (Residual)") +
  ylab("Increase in Prestige (Residual)") +
  theme_classic() +
  theme(text=element_text(family="Times"))

## Table 1: summary table of completeness
t <-
  full_data %>%
  group_by(corpus,year) %>%
  arrange(corpus,year) %>%
  summarize(complete = sum(!is.na(gender)))

t %>%
  group_by(corpus) %>%
  summarize(complete = mean(complete))

###~###~####~###~###~###~ replace prestige ###~###~####~###~###~###~

change <-
  full_data %>%
  filter(corpus=="ngram") %>%
  group_by(id) %>%
  summarize(prestige_change = mean(prent90[year>=1980&year<=1980],na.rm=T)-
              mean(presgl[year>=1960&year<=1960],na.rm=T),
         gender_change = mean(gender[year>=1980&year<=1980],na.rm=T)-
           mean(gender[year>=1960&year<=1960],na.rm=T),
         count=max(count))
summary(
  lm(prestige_change~gender_change, change)
)


##### check percent change and subjective perception change 

library(ggrepel)

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

t <-
  data %>%
  filter(corpus=="ngram") %>%
  group_by(id) %>%
  mutate(p_change=(percent_f[year==2000]-percent_f[year==1940]),
         gender_change=(gender[year==2000]-gender[year==1940]),
         prestige_change=(prestige[year==2000]-prestige[year==1940])) %>%
  distinct(id,.keep_all = T)

library(stringr)
t$occupation_word <- word(t$occupation_single, 1)
t$occupation_word <- gsub(",","",t$occupation_word)
t[which(t$occupation_word=="head"),"occupation_word"] <- "manager"
t[which(t$occupation_origin=="Dentists"),"occupation_word"] <- "dentist"
t[which(t$occupation_word=="dentist"),"incgroup"] <- 4

## ngram
t %>%
  mutate(incgroup = case_when(incgroup==4 ~ "1",
                              incgroup!=4 ~ "0")) %>%
  ggplot(aes(x=p_change,y=gender_change,group=as.factor(incgroup), color=as.factor(incgroup))) +
  geom_point(aes(size=count),fill="white",shape=21,alpha=1,stroke=0.5) +
  geom_smooth(method='lm', formula= y~x, 
              show.legend = T, se=F, fullrange=TRUE) +
  guides(size = "none") +
  scale_color_manual(name = "Income Group", labels=c("1st-3rd Quartile","4th Quartile"), values=c("blue3","red4")) +
  xlab("Changes in Female Worker Share") +
  ylab("Changes in Female Stereotypes") +
  geom_text_repel( 
    data = t %>%
      mutate(incgroup = case_when(incgroup==4 ~ "1",
                                  incgroup!=4 ~ "0")) %>%
      filter(incgroup=="1"&p_change>=0.06&gender_change<0.025&occupation_origin!="Personnel and labor relations workers"&
               occupation_origin!="Electrical-Engineers"&occupation_origin!="Metallurgical, metallurgists-Engineers"&
               occupation_origin!="Engineers (nec)"&occupation_origin!="Civil-Engineers"&
               occupation_word!="editor"&occupation_word!="sales"&
               occupation_origin!="Inspectors, public administration"),
    aes(label=occupation_word),
    box.padding = 0.25,
    size=5,
    family = "Times"
  ) + 
  scale_size(range = c(1,10)) +
  geom_hline(yintercept=mean(t$gender_change, na.rm=T), linetype="dashed", color = "grey50") +
  geom_vline(xintercept=mean(t$p_change, na.rm=T), linetype="dashed", color = "grey50") +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom")

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/percent_f_gender_changes_professional_ngram.png", width = 14.5, height = 15, 
       units = "cm", dpi=600)

## ngram, 1940-2009
t <-
  data %>%
  filter(corpus=="COHA") %>%
  group_by(id) %>%
  mutate(p_change=(percent_f[year==2000]-percent_f[year==1940]),
         gender_change=(gender[year==2000]-gender[year==1940]),
         prestige_change=(prestige[year==2000]-prestige[year==1940])) %>%
  distinct(id,.keep_all = T)

library(stringr)
t$occupation_word <- word(t$occupation_single, 1)
t$occupation_word <- gsub(",","",t$occupation_word)
t[which(t$occupation_word=="head"),"occupation_word"] <- "manager"
t[which(t$occupation_origin=="Dentists"),"occupation_word"] <- "dentist"
t[which(t$occupation_word=="dentist"),"incgroup"] <- 4

## ngram
t %>%
  mutate(incgroup = case_when(incgroup==4 ~ "1",
                              incgroup!=4 ~ "0")) %>%
  ggplot(aes(x=p_change,y=gender_change,group=as.factor(incgroup), color=as.factor(incgroup))) +
  geom_point(aes(size=count),fill="white",shape=21,alpha=1,stroke=0.5) +
  geom_smooth(method='lm', formula= y~x, 
              show.legend = T, se=F, fullrange=TRUE) +
  guides(size = "none") +
  scale_color_manual(name = "Income Group", labels=c("1st-3rd Quartile","4th Quartile"), values=c("blue3","red4")) +
  xlab("Changes in Female Worker Share") +
  ylab("Changes in Female Stereotypes") +
  geom_text_repel( 
    data = t %>%
      mutate(incgroup = case_when(incgroup==4 ~ "1",
                                  incgroup!=4 ~ "0")) %>%
      filter(incgroup=="1"&p_change>=0.06&gender_change<0.2&occupation_origin!="Personnel and labor relations workers"&
               occupation_origin!="Electrical-Engineers"&occupation_origin!="Metallurgical, metallurgists-Engineers"&
               occupation_origin!="Engineers (nec)"&occupation_origin!="Civil-Engineers"&
               occupation_word!="editor"&occupation_word!="sales"&
               occupation_origin!="Inspectors, public administration"),
    aes(label=occupation_word),
    box.padding = 0.25,
    size=5,
    family = "Times"
  ) + 
  scale_size(range = c(1,10)) +
  geom_hline(yintercept=mean(t$gender_change, na.rm=T), linetype="dashed", color = "grey50") +
  geom_vline(xintercept=mean(t$p_change, na.rm=T), linetype="dashed", color = "grey50") +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom")

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/percent_f_gender_changes_professional_COHA_1940_2009.png", width = 14.5, height = 15, 
       units = "cm", dpi=600)

## COCA
t <-
  data %>%
  filter(corpus=="COCA") %>%
  group_by(id) %>%
  mutate(p_change=(percent_f[year==2010]-percent_f[year==1990]),
         gender_change=(gender[year==2010]-gender[year==1990]),
         prestige_change=(prestige[year==2010]-prestige[year==1990])) %>%
  distinct(id,.keep_all = T)

library(stringr)
t$occupation_word <- word(t$occupation_single, 1)
t$occupation_word <- gsub(",","",t$occupation_word)
t[which(t$occupation_word=="head"),"occupation_word"] <- "manager"
t[which(t$occupation_origin=="Dentists"),"occupation_word"] <- "dentist"
t[which(t$occupation_word=="dentist"),"incgroup"] <- 4

t %>%
  mutate(incgroup = case_when(incgroup==4 ~ "1",
                              incgroup!=4 ~ "0")) %>%
  ggplot(aes(x=p_change,y=gender_change,group=as.factor(incgroup), color=as.factor(incgroup))) +
  geom_point(aes(size=count),fill="white",shape=21,alpha=1,stroke=0.5) +
  geom_smooth(method='lm', formula= y~x, 
              show.legend = T, se=F, fullrange=TRUE) +
  guides(size = "none") +
  scale_color_manual(name = "Income Group", labels=c("1st-3rd Quartile","4th Quartile"), values=c("blue3","red4")) +
  xlab("Changes in Female Worker Share") +
  ylab("Changes in Female Stereotypes") +
  geom_text_repel( 
    data = t %>%
      mutate(incgroup = case_when(incgroup==4 ~ "1",
                                  incgroup!=4 ~ "0")) %>%
      filter(incgroup=="1"&p_change>=0.02&gender_change<0.05&occupation_origin!="Personnel and labor relations workers"&
               occupation_origin!="Civil-Engineers"&occupation_origin!="Chemical-Engineers"&
               occupation_word!="editor"&occupation_word!="sales"),
    aes(label=occupation_word),
    box.padding = 0.25,
    size=5,
    family = "Times"
  ) + 
  scale_size(range = c(1,10)) +
  geom_hline(yintercept=mean(t$gender_change, na.rm=T), linetype="dashed", color = "grey50") +
  geom_vline(xintercept=mean(t$p_change, na.rm=T), linetype="dashed", color = "grey50") +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom")


ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/percent_f_gender_changes_professional_COCA.png", width = 14.5, height = 15, 
       units = "cm", dpi=600)

############# 
full_data <- 
  full_data %>%
  mutate(prestige_survey = case_when(year==1960~presgl,
                                     year==1990~prent90,
                                     .default = NA))
summary(
  felm(prestige ~ lag(percent_f,3) + gender + wage + edu + lmexp | id + year | 0 | id, full_data[full_data$corpus=="COHA", ])
)

summary(
  lm(prestige ~ percent_f + gender + wage + edu + lmexp + I(lmexp^2), full_data[full_data$corpus=="COCA"&full_data$year==2000, ])
)

full_data %>%
  filter(year==2000) %>%
  ggplot(aes(x=percent_f,y=gender)) +
  geom_point(aes(size=count),color="green4",fill="white") +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(cols = vars(corpus)) +
  theme_bw()
  

