ori_ngram <-
  felm(prestige ~ gender + income + education + moral | id + year | 0 | id, full_data[full_data$corpus=="ngram", ])

full_ngram <-
  felm(prestige ~ gender + income + education + moral + 
         race_1 + race_2 + race_3 + race_4 + wage + edu +
         lmexp + I(lmexp^2) + hours | id + year | 0 | id, full_data[full_data$corpus=="ngram", ])

ori_COHA <-
  felm(prestige ~ gender + income + education + moral | id + year | 0 | id, full_data[full_data$corpus=="COHA", ])

full_COHA <-
  felm(prestige ~ gender + income + education + moral + 
         race_1 + race_2 + race_3 + race_4 + wage + edu +
         lmexp + I(lmexp^2) + hours | id + year | 0 | id, full_data[full_data$corpus=="COHA", ])

ori_COCA <-
  felm(prestige ~ gender + income + education + moral | id + year | 0 | id, full_data[full_data$corpus=="COCA", ])

full_COCA <-
  felm(prestige ~ gender + income + education + moral + 
         race_1 + race_2 + race_3 + race_4 + wage + edu +
         lmexp + I(lmexp^2) + hours | id + year | 0 | id, full_data[full_data$corpus=="COCA", ])

library(stargazer)

stargazer(ori_ngram,
          full_ngram,
          ori_COHA,
          full_COHA,
          ori_COCA,
          full_COCA,
          type="latex",
          digits = 3)

summary(full_COCA)
