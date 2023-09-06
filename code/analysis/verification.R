library(ggplot2)
library(ggrepel)
library(stringr)

## single occupation name
full_data$occupation_word <- word(full_data$occupation_origin, 1)
full_data$occupation_word <- gsub(",","",full_data$occupation_word)

full_data[which(full_data$occupation_origin==
                  "Medical Sciences-Professors and instructors"),"occupation_word"] <-
  "Medical Science (Professors)"

## select professional and technical
pro <-
  full_data %>% filter(corpus=="COHA") %>% 
  group_by(id) %>%
  dplyr::mutate(count=max(count,na.rm=T)) %>%
  filter(year==1980 & category=="Professional, Technical")

summary(lm(percent_f~gender,pro))
cor(pro$percent_f,pro$gender, use="complete.obs")

pro %>%
  ggplot(aes(x=gender,y=percent_f,size=count)) +
  geom_point(color="blue2",fill="white",shape=21,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              mapping = aes(weight = count), 
              show.legend = FALSE, se=F,alpha=0.1,
              lwd=0.8,
              fullrange = TRUE) + 
  guides(size = FALSE) +
  xlim(-0.22,0.23) +
  geom_text_repel(
    data = pro %>% filter(occupation_word %in% c("Nurses","Dancers","Teachers","Therapists","Librarians","Dietians",
                                          "Therapists","Designers","Psychologists","Entertainers",
                                          "Editors","Artists","Statisticians","Accountants","Economists",
                                          "Photographers","Physicians","Lawyers",
                                          "Pilots","Pharmacists","Dietitians",
                                          "Geologists","Mechanical-Engineers",
                                          "Medical Science (Professors)")),
    aes(label=occupation_word),
    size=5,
    family = "Times",
    box.padding = unit(0.15, "lines"),
    point.padding = unit(0.15, "lines")
  ) + 
  xlab("Female Stereotypes") +
  ylab("Percent Female") +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/percent_stereotype_coha_1980_professional.png", width = 13.6, height = 13, units = "cm")

## select service workers
pro <-
  full_data %>% filter(corpus=="COHA") %>% 
  group_by(id) %>%
  dplyr::mutate(count=max(count,na.rm=T)) %>%
  filter(year==1980 & category=="Service Workers") %>%
  filter(occupation_word!="Counter")

summary(lm(percent_f~gender,pro))
cor(pro$percent_f,pro$gender, use="complete.obs")

pro[which(pro$occupation_origin=="Attendants, hospital and other institution"),"occupation_word"] <- 
  "Attendants (hospital)"
pro[which(pro$occupation_origin=="Charwomen and cleaners"),"occupation_word"] <- 
  "Cleaners"
pro[which(pro$occupation_origin=="Private household workers (nec)"),"occupation_word"] <- 
  "Housekeeper (Private)"
pro[which(pro$occupation_origin=="Practical nurses"),"occupation_word"] <- 
  "Practical Nurses"
pro[which(pro$occupation_origin=="Elevator operators"),"occupation_word"] <- 
  "Elevator Operators"
pro[which(pro$occupation_origin=="Counter and fountain workers"),"occupation_word"] <- 
  "Counter Workers"

pro %>%
  ggplot(aes(x=gender,y=percent_f,size=count)) +
  geom_point(color="blue2",fill="white",shape=21,alpha=1) +
  scale_size(range = c(1,10)) +
  geom_smooth(method='lm', formula=y~x, color="red4",
              show.legend = FALSE, se=F,alpha=0.1,
              lwd=0.8,
              fullrange = TRUE) + 
  guides(size = FALSE) +
  xlim(-0.22,0.23) +
  geom_text_repel(
    data = pro %>% filter(occupation_word %in% c("Laundressses","Attendants (hospital)","Housekeeper (Private)","Cooks",
                                                 "Bartenders","Ushers","Janitors","Guards","Sheriffs",
                                                 "Porters","Elevator Operators","Policemen",
                                                 "Firemen","Charwomen","Counter Workers","Cleaners","Barbers")),
    aes(label=occupation_word),
    size=5,
    family = "Times",
    box.padding = unit(0.15, "lines"),
    point.padding = unit(0.15, "lines")
  ) + 
  xlab("Semantic Association with Women") +
  ylab("Percent Female") +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/percent_stereotype_coha_1980_service.png", width = 13.6, height = 13, units = "cm")



## verify prestige measure - 1960 Siegel
## Siegel did not differentiate occupations and give same score to several same occupations
## I keep unique prestige scores

summary(
  lm(presgl/100 ~ npboss, full_data %>% filter(year==1960&
                                                   presgl>0&
                                                   corpus=="COHA") %>%
       distinct(year,presgl,.keep_all = TRUE),
     weights = count)
)

t <- full_data %>% filter(year==1960&
                            presgl>0&
                            corpus=="COHA") %>%
  distinct(year,presgl,.keep_all = TRUE)
cor.test(t$presgl, t$eduscore)

summary(
  lm(prent90/100 ~ prestige, full_data %>% filter(year==1990&
                                                   prent90>0&
                                                   corpus=="ngram")%>%
       distinct(year,presgl,.keep_all = TRUE),
     weights = count)
)

full_data %>% filter(year==1960&
                       presgl>0&
                       corpus=="COHA") %>%
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
                                         corpus=="COHA") %>%
              mutate(occupation_word = word(occupation_single, 1),
                     occupation_word = gsub(",","",occupation_word)) %>%
              distinct(year,presgl,.keep_all = TRUE) %>%
              distinct(occupation_word, .keep_all = TRUE) %>%
                filter(occupation_word!="technician"),
            aes(label=occupation_word),
            size=5,
            check_overlap = T,
            family="Times") +
  xlab("Embedding-based Prestige") +
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
                       corpus=="ngram") %>%
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
                                          corpus=="ngram") %>%
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
  xlab("Embedding-based Prestige") +
  ylab("Nakao-Treas Prestige") +
  guides(size = FALSE) +
  theme_classic() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=16))
ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/prestige_comparison_1990.png", width = 13.6, height = 13, units = "cm")

t <- full_data %>% filter(year==1990&
                            corpus=="ngram")
cor.test(t$prent90, t$npboss)

summary(
  lm(prent90/100 ~ earnscore, full_data %>% filter(year==1990&
                                                    prent90>0&
                                                    corpus=="ngram"),
     weights = count)
)
