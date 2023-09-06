library(dpm)

result <-
  data.frame()

## dynamic panel - ngram
data <- full_data[which(full_data$corpus=="ngram"),] %>% ungroup()
data_dpm <- panel_data(data %>% mutate(year = (year-1890)/10), id = id, wave = year)
estimates <-
  summary(
  dpm(prestige ~  pre(gender) + income + education + moral,
      data = data_dpm,
      error.inv = TRUE, information = "observed",
      missing = "fiml",
      fixed.effects = TRUE,
      check.gradient = FALSE)
)$coef[,2:3]
estimates$corpus <- "ngram"
estimates$model <- "main"
estimates$variable <- c("female","affluence","cultivation","morality","lagged prestige")
result <- rbind(result,estimates)

estimates <-
  summary(
  dpm(gender ~  pre(prestige) + pre(lag(prestige)) + income + moral + education,
      data = data_dpm,
      error.inv = FALSE, information = "observed",
      missing = "fiml",
      fixed.effects = TRUE,
      check.gradient = FALSE)
)$coef[,2:3]
estimates$corpus <- "ngram"
estimates$model <- "reciprocal"
estimates$variable <- c("prestige","lagged prestige","affluence","cultivation","morality","lagged female")
result <- rbind(result,estimates)


## dynamic panel - COHA
data <- full_data[which(full_data$corpus=="COHA"),] %>% ungroup()
data_dpm <- panel_data(data %>% mutate(year = (year-1890)/10), id = id, wave = year)
estimates <-
  summary(
    dpm(prestige ~  pre(gender) + income + education + moral,
        data = data_dpm,
        error.inv = TRUE, information = "observed",
        missing = "fiml",
        fixed.effects = TRUE,
        check.gradient = FALSE)
  )$coef[,2:3]
estimates$corpus <- "COHA"
estimates$model <- "main"
estimates$variable <- c("female","affluence","cultivation","morality","lagged prestige")
result <- rbind(result,estimates)

estimates <-
  summary(
  dpm(gender ~  pre(prestige) + pre(lag(prestige)) + income + education + moral,
      data = data_dpm,
      error.inv = FALSE, information = "observed",
      missing = "fiml",
      fixed.effects = TRUE,
      check.gradient = FALSE)
)$coef[,2:3]
estimates$corpus <- "COHA"
estimates$model <- "reciprocal"
estimates$variable <- c("prestige","lagged prestige","affluence","cultivation","morality","lagged female")
result <- rbind(result,estimates)

## plot
result[which(result$model=="main"),"model"] <- "Devaluation"
result[which(result$model=="reciprocal"),"model"] <- "Queueing"
result %>%
  mutate(Est.=case_when(
    model=="Devaluation"&variable=="female"&corpus=="ngram" ~ Est.-0.005,
    .default = Est.
  )) %>%
  mutate(variable=factor(variable,level=c(
                                          "lagged female","lagged prestige",
                                          "morality","affluence","cultivation",
                                          "female","prestige")),
         corpus=factor(corpus,level=c("ngram","COHA"),
                       labels=c("Ngram","COHA"))) %>%
  ggplot(aes(x=variable, y=Est.,color=model)) +
  geom_point(position=position_dodge(3),size=2.7)+
  geom_errorbar(aes(ymin=Est.-1.96*S.E., ymax=Est.+1.96*S.E.), width=0,
                position=position_dodge(3)) +
  facet_grid(model ~ corpus, scales="free") +
  coord_flip() +
  ylab("Coefficient") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50") +
  scale_color_manual(values=c("blue","green4")) +
  scale_x_discrete(labels=c("female"=expression(bold(female)),
                            "prestige"=expression(bold(prestige)),
                            parse=TRUE)) +
  theme_classic() +
  theme(text=element_text(family="Times",size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(size=10),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.text.y = element_text(size = 14, colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth=0.6))


ggsave("~/Dropbox/Devaluation Word Embeddings/Figures/New/dpm.png", width = 14, height = 13.6, units = "cm")


