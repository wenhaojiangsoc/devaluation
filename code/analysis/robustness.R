library(dplyr)
library(lfe)
library(ggplot2)
library(zoo)


##### Read Main ######

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main')
full_data <- data.frame()

## COHA
data <- read.csv("COHA.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data$corpus <- "COHA"
full_data <- rbind(full_data,data)

## Ngram
data <- read.csv("ngram.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
data$corpus <- "ngram"
full_data <- rbind(full_data,data)

## COCA
data <- read.csv("COCA.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Single.words)
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


##### Read Gendered Occupation - Male ######

## read data
setwd('/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Robustness')
male_data <- data.frame()

## COHA
data <- read.csv("COHA_gendered_occupation_male.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Male.words)
data$corpus <- "COHA"
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

## COCA
data <- read.csv("COCA_gendered_occupation_male.csv")
data <- data %>%
  rename(occupation_origin = Occupation..1950.basis,
         occupation_single = Male.words)
data <- data %>%
  distinct(occupation_single, year, .keep_all = T)
data$corpus <- "COCA"
male_data <- rbind(male_data,data)

## merge with 1950 occupation official code
code <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Main/1950occcode.csv")
code$occ <- gsub('n.e.c.', 'nec', code$occ)
male_data$occupation_origin <- gsub('n.e.c.', 'nec', male_data$occupation_origin)
male_data <- merge(male_data, code, by.y="occ", by.x="occupation_origin", all.x = T)
rm(code)

male_data <-
  merge(male_data %>% select(-("gender")),full_data[,c("year","occupation_origin","gender","corpus","count")],
      by = c("year","occupation_origin","corpus"),
      all.x = T)

male_data <-
  male_data %>%
  group_by(corpus,code) %>%
  mutate(id = cur_group_id())

## respective regression
summary(felm(prestige ~ gender + education + income + moral | code + year | 0 | code, male_data[male_data$corpus=="ngram",]))

summary(felm(prestige ~ gender + education + income + moral | code + year | 0 | code, male_data[male_data$corpus=="COHA",],
             weights = male_data[male_data$corpus=="COHA",]$count))

summary(felm(prestige ~ gender + education + income + moral | code + year | 0 | code, male_data[male_data$corpus=="COCA",],
             weights = male_data[male_data$corpus=="COCA",]$count))

##### Dimensional Correlation #####

## 3-year moving average (smooth plot)
library(zoo)
three_ma <- function(x) c(x[1], rollmean(x, 3), x[length(x)])

correlation <- read.csv("/Users/wenhao/Dropbox/Devaluation Word Embeddings/Data/Robustness/dimension_correlation.csv")
correlation[correlation$dimension=="income","dimension"] <- "affluence"
correlation[correlation$dimension=="education","dimension"] <- "cultivation"
correlation[correlation$dimension=="moral","dimension"] <- "morality"
correlation[correlation$corpus=="ngram","corpus"] <- "Ngram"
correlation %>%
  group_by(corpus) %>%
  mutate(cos = three_ma(cos)) %>%
  mutate(corpus = factor(corpus, levels=c("Ngram","COHA"))) %>%
  arrange(desc(year)) %>%
  ggplot(aes(x=year, y=cos, color=dimension)) +
  geom_point(size=2) +
  scale_color_manual(values=c("red4","blue3","green4","grey30")) +
  geom_line() +
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

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/dimension_correlation.png", width = 16, height = 11.6, units = "cm")
