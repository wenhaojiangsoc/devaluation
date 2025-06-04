library(dplyr)
library(svMisc)

difference <- function(data,
                       k=1, outcome, treatment, control) {
  
  ## create copy of the data
  df <- data
  
  ## tilde y
  tilde_outcome <- paste0("tilde_",as.character(outcome))
  
  df <-
    eval(parse(text=paste0(
      "df %>%
    group_by(year) %>%
    summarize(",tilde_outcome, "= mean(", outcome,", na.rm=T)) %>%
    merge(df, by='year',all.y=T)")))
  df[,tilde_outcome] <- df[,outcome] - df[,tilde_outcome]
  
  ## tilde x
  tilde_treatment <- paste0("tilde_",as.character(treatment))
  df <-
    eval(parse(text=paste0(
      "df %>%
    group_by(year) %>%
    summarize(",tilde_treatment, "= mean(", treatment,", na.rm=T)) %>%
    merge(df, by='year',all.y=T)")))
  df[,tilde_treatment] <- df[,treatment] - df[,tilde_treatment]
  
  ## controls are optional
  if(missing(control)) {
      
    ## create k differences
    for (i in 1:k){
      
      outcome_name <- paste0("diff_",as.character(outcome),"_",as.character(i))
      treatment_name <- paste0("diff_",as.character(treatment),"_",as.character(i))
      
      df <-
        eval(parse(text=paste0(
          "df %>%
        group_by(id) %>%
        arrange(year) %>%
        mutate(",outcome_name, "= ", outcome, "-lag(",outcome,",i),",
          treatment_name, "= ", treatment,"-lag(",treatment,",i))"
        )))
      
      tilde_outcome_name <- paste0("diff_tilde_",as.character(outcome),"_",as.character(i))
      tilde_treatment_name <- paste0("diff_tilde_",as.character(treatment),"_",as.character(i))
      
      df <-
        eval(parse(text=paste0(
          "df %>%
        group_by(id) %>%
        arrange(year) %>%
        mutate(",tilde_outcome_name, "= ", tilde_outcome, "-lag(",tilde_outcome,",i),",
          tilde_treatment_name, "= ", tilde_treatment,"-lag(",tilde_treatment,",i))"
        )))
    
    }
    
    return(df)
    
  } else {
    
    ## create k differences
    for (i in 1:k){
      
      outcome_name <- paste0("diff_",as.character(outcome),"_",as.character(i))
      treatment_name <- paste0("diff_",as.character(treatment),"_",as.character(i))
      
      df <-
        eval(parse(text=paste0(
          "df %>%
         group_by(id) %>%
        arrange(year) %>%
        mutate(",outcome_name, "= ", outcome, "-lag(",outcome,",i),",
          treatment_name, "= ", treatment,"-lag(",treatment,",i))"
        )))
      
      tilde_outcome_name <- paste0("diff_tilde_",as.character(outcome),"_",as.character(i))
      tilde_treatment_name <- paste0("diff_tilde_",as.character(treatment),"_",as.character(i))
      
      df <-
        eval(parse(text=paste0(
          "df %>%
        group_by(id) %>%
        arrange(year) %>%
        mutate(",tilde_outcome_name, "= ", tilde_outcome, "-lag(",tilde_outcome,",i),",
          tilde_treatment_name, "= ", tilde_treatment,"-lag(",tilde_treatment,",i))"
        )))
      
      ## loop over controls
      for (j in 1:length(control)){
        
        control_name <- paste0("diff_",as.character(control[j]),"_",as.character(i))
        
        df <-
          eval(parse(text=paste0(
            "df %>%
         group_by(id) %>%
        arrange(year) %>%
        mutate(",control_name, "= ", control[j], "-lag(",control[j],",i))"
          )))
        
        tilde_control_name <- paste0("diff_tilde_",as.character(control[j]),"_",as.character(i))
        tilde_control <- paste0("tilde_",as.character(control[j]))
        
        if (i==1){
          df <-
            eval(parse(text=paste0(
              "df %>%
            group_by(year) %>%
            summarize(",tilde_control, "= mean(", control[j],", na.rm=T)) %>%
            merge(df, by='year',all.y=T)")))
          df <-
            eval(parse(text=paste0(
              "df %>%
            mutate(",tilde_control, "= ", control[j], "-", tilde_control, ")")))
        } else {}
        
        df <-
          eval(parse(text=paste0(
            "df %>%
         group_by(id) %>%
        arrange(year) %>%
        mutate(",tilde_control_name, "= ", tilde_control, "-lag(",tilde_control,",i))"
          )))
      
      }
    
    }
    return(df)
  }
}

FEdecompose <- function(data,
                        k=1, outcome, treatment, control) {
  
  df <- data
  
  ## store regression results
  FD_results <-
    data.frame(k=seq(1,k,1),
               FD_regression=rep(NA,k),
               FD_SD=rep(NA,k),
               FD_mechanic=rep(NA,k),
               weight=rep(NA,k))
  
  
  if (missing(control)) {
    
    ## create k differences
    for (i in 1:k){
      
      outcome_name <- paste0("diff_",as.character(outcome),"_",as.character(i))
      treatment_name <- paste0("diff_",as.character(treatment),"_",as.character(i))
      tilde_outcome_name <- paste0("diff_tilde_",as.character(outcome),"_",as.character(i))
      tilde_treatment_name <- paste0("diff_tilde_",as.character(treatment),"_",as.character(i))
      
      if (i < k) {
        
        FD_results[i,"FD_regression"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, "+", "factor(year), df))")))$coef[2,1]
        
        FD_results[i,"FD_SD"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, "+", "factor(year), df))")))$coef[2,2]
        
        FD_results[i,"FD_mechanic"] <-
          sum(df[,tilde_treatment_name]*df[,tilde_outcome_name],na.rm=T)/sum((df[,tilde_treatment_name])^2,na.rm=T)
        
        FD_results[i,"weight"] <- sum((df[,tilde_treatment_name])^2,na.rm=T)
      } else {
        
        FD_results[i,"FD_regression"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, ",df))")))$coef[2,1]
        
        FD_results[i,"FD_SD"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, ",df))")))$coef[2,2]
        
        FD_results[i,"FD_mechanic"] <-
          sum(df[,tilde_treatment_name]*df[,tilde_outcome_name],na.rm=T)/sum((df[,tilde_treatment_name])^2,na.rm=T)
        
        FD_results[i,"weight"] <- sum((df[,tilde_treatment_name])^2,na.rm=T)
      }
    }
    
    FD_results$weight <- FD_results$weight/sum(FD_results$weight)
    
    FD_results <- FD_results %>%
      dplyr::select(-c("FD_mechanic"))
    return(FD_results)
  }
  
  else {
    
    df_t <- df
    
    ## create weight
    tilde_treatment_mean <- paste0("tilde_",as.character(treatment),"_mean")
    tilde_treatment <- paste0("tilde_",as.character(treatment))
    df_t <- eval(parse(text=paste0(
      "df_t %>%
         group_by(id) %>%
        summarize(",tilde_treatment_mean, "= mean(", tilde_treatment, ", na.rm=T)) %>%
      merge(df_t, by = 'id', all.y=T) %>% mutate(", tilde_treatment, " = ",
      tilde_treatment, "-", tilde_treatment_mean, ")"
    )))
    
    controls <- c()
    for (j in 1:length(control)){
      tilde_control_name_mean <- paste0("tilde_",as.character(control[j]),"_mean")
      tilde_control_name <- paste0("tilde_",as.character(control[j]))
      df_t <- eval(parse(text=paste0(
        "df_t %>%
         group_by(id) %>%
        summarize(",tilde_control_name_mean, "= mean(", tilde_control_name, ", na.rm=T)) %>%
      merge(df_t, by = 'id', all.y=T) %>% mutate(", tilde_control_name, " = ",
        tilde_control_name, "-", tilde_control_name_mean, ")"
      )))
      controls <- c(controls, tilde_control_name)
    }
    
    model <-
      eval(parse(text=paste0(
      "lm(",tilde_treatment,"~", paste(controls, collapse = " + "),
      ", df_t)")))
    
    ## create k differences
    for (i in 1:k){
      
      outcome_name <- paste0("diff_",as.character(outcome),"_",as.character(i))
      treatment_name <- paste0("diff_",as.character(treatment),"_",as.character(i))
      tilde_treatment_name <- paste0("diff_tilde_",as.character(treatment),"_",as.character(i))
      
      ## predict
      df_t <- df_t[ , !(names(df_t) %in% controls)]
      
      ## rename
      for (j in 1:length(control)){
        tilde_control_name <- paste0("tilde_",as.character(control[j]))
        diff_tilde_control_name_i <- paste0("diff_",tilde_control_name,"_",as.character(i))
        df_t <- eval(parse(text=paste0(
          "df_t %>%
          dplyr::rename(",tilde_control_name, "=", diff_tilde_control_name_i,")"
        )))
      }
      
      fit_name <- paste0("fit_",as.character(i))
      
      df <- eval(parse(text=paste0(
        "df %>% ungroup() %>% mutate(",fit_name," = predict.lm(model, df_t))")
      ))
      
      if (i < k) {
        
        FD_results[i,"FD_regression"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, "+", paste(control, collapse = " + "),
            "+ factor(year), df))")))$coef[2,1]
        
        FD_results[i,"FD_SD"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, "+", paste(control, collapse = " + "),
            "+ factor(year), df))")))$coef[2,2]
        
        FD_results[i,"weight"] <- sum((df[,tilde_treatment_name]-df[,fit_name])^2,na.rm=T)
        
      } else {
        
        FD_results[i,"FD_regression"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, "+", paste(control, collapse = " + "),
            ",df))")))$coef[2,1]
        
        FD_results[i,"FD_SD"] <-
          eval(parse(text=paste0(
            "summary(lm(",outcome_name,"~", treatment_name, "+", paste(control, collapse = " + "),
            ",df))")))$coef[2,2]
        
        FD_results[i,"weight"] <- sum((df[,tilde_treatment_name]-df[,fit_name])^2,na.rm=T)
        
      }
    }
    FD_results$weight <- FD_results$weight/sum(FD_results$weight)
    
    FD_results <- FD_results %>%
      dplyr::select(-c("FD_mechanic"))
    return(FD_results)
  }
}

## robust FE estimator SE
FErobust <- function(data, count, t, outcome, treatment, control) {
  
  ## synthesizing function
  synthesize <- function(FD_results, t) {
    robust_b <-
      sum(FD_results[FD_results$k <= t, "FD_regression"] * FD_results[FD_results$k <= t, "weight"]) /
      sum(FD_results[FD_results$k <= t, "weight"]) + unique(FD_results$B)
    
    robust_se <-
      sum(FD_results[FD_results$k <= t, "FD_SD"] * FD_results[FD_results$k <= t, "weight"]) /
      sum(FD_results[FD_results$k <= t, "weight"])
    
    return(c(robust_b,robust_se))
  }
    
  ## decompose
  if (missing(control)) {
    df <- difference(data %>% group_by(id) %>%
                       mutate(count=sum(!is.na(treatment))) %>%
                       filter(count==count) %>%
                       as.data.frame(),
                     k=count-1,
                     outcome=outcome,treatment=treatment)
    FD_results <- FEdecompose(df,k=count-1,outcome=outcome,treatment=treatment)
    FD_results$B <- 0
    FE_robust <- synthesize(FD_results, t=t)
  } else {
    df <- difference(data %>% group_by(id) %>%
                       mutate(count=sum(!is.na(treatment))) %>%
                       filter(count==count) %>%
                       as.data.frame(),
                     k=count-1,
                     outcome=outcome,treatment=treatment,control=control)
    FD_results <- FEdecompose(df,k=count-1,outcome=outcome,treatment=treatment,control=control)
    
    ## compute B
    BB <- data %>% group_by(id) %>%
      mutate(count=sum(!is.na(treatment))) %>%
      filter(count==count) %>%
      as.data.frame()
    FD_results$B <- eval(parse(text=paste0(
      "summary(lm(",outcome,"~", treatment, "+", paste(control, collapse = " + "),
      "+ factor(year) + factor(id), BB))")))$coef[2,1] - sum(FD_results$FD_regression*FD_results$weight)
    FE_robust <- synthesize(FD_results, t=t)
  }
  
  return(FE_robust)
  
}

## decompose
FErobust(full_data[full_data$corpus=="ngram", ], count=11, t=5, outcome="prestige",
             treatment="gender",control=c("education","income"))



df <- difference(full_data[full_data$corpus=="ngram", ] %>% group_by(id) %>%
                   mutate(count=sum(!is.na(gender))) %>%
                   filter(count==count) %>%
                   as.data.frame(),
                 k=11-1,
                 outcome="prestige",treatment="gender",control=c("education","income"))
FD_results <- FEdecompose(df,k=11-1,outcome="prestige",treatment="gender",control=c("education","income"))
