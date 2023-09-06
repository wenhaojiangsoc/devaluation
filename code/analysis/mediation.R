library(ggplot2)
library(mediation)

## analysis of COHA
t <-
  full_data[full_data$corpus=="COHA", ] %>%
  group_by(code) %>%
  mutate(gender = gender - mean(gender,na.rm=T),
         percent_f = percent_f - mean(percent_f,na.rm=T),
         prestige = prestige - mean(prestige,na.rm=T)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(gender = gender - mean(gender,na.rm=T),
         percent_f = percent_f - mean(percent_f,na.rm=T),
         prestige = prestige - mean(prestige,na.rm=T)) %>%
  ungroup() %>%
  group_by(code) %>%
  arrange(year) %>%
  mutate(percent_f = dplyr::lag(percent_f,1)) 

## lag percent_f after de-mean the measures by occupation and year (equivalent to TWFE)
fitM <- lm(gender ~ percent_f, data=t, weight=count)
fitY <- lm(prestige ~ percent_f + gender, data=t, weight=count)

mediation_model <- mediate(
  fitM,
  fitY,
  treat = "percent_f",
  mediator = "gender",
  boot = T
)

effect <- 
  data.frame(
  effect = c(mediation_model$tau.coef, ## total effect
  mediation_model$z0, ## direct effect
  mediation_model$d0), ## mediation effect
  
  ci = c((mediation_model$tau.ci[2] - mediation_model$tau.ci[1])/2,
         (mediation_model$z0.ci[2] - mediation_model$z0.ci[1])/2,
         (mediation_model$d0.ci[2] - mediation_model$d0.ci[1])/2),
  
  name = c("total","direct","mediation")
  
)

## plot
ggplot(effect,aes(x=name,y=effect)) +
  geom_errorbar(aes(ymin=effect-ci, ymax=effect+ci), colour="blue2", width=0) +
  geom_point(size=4,color="blue2") +
  xlab("Effect Path") +
  ylab("Coefficient") +
  ylim(min(effect$effect)-0.06,max(effect$effect)+0.03) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red3", linetype = "dashed") +
  scale_x_discrete(expand=c(0.3,0.3)) +
  theme_bw() +
  theme(text=element_text(family="Times",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12,angle=90,vjust=0.5,hjust=0.5),
        axis.title = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom")

ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/mediation_COHA_lag1.png", width = 8, height = 8, 
       units = "cm")  
