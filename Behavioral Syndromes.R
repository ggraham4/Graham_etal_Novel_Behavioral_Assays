#Searching for behavioral syndromes using pearsons correlations

#libraries
{
  library(readxl)
  library(ggplot2)
  library(dplyr)
  library(lme4)
  library(ggsignif)
  library(sjPlot)
  library(ggpubr)
  library(lmerTest)
  library(MuMIn)
  library(ggpubr)
  library(tidyverse)
  library(ggeffects)
  library(emmeans)
  library(irr)
  library(psych)
  library(forcats)
library(factoextra)
library(ggrepel)
}

pairwise_pearson <- function(terms, df) {
  total_models <- 0
  significant_models <- 0
  
  dag <- data.frame()
  for (i in seq_along(terms)) {
    for (j in seq_along(terms)) {
      if (i != j) {  # Ensure not modeling the term against itself
        term1 <- terms[i]
        term2 <- terms[j]
        
        # Perform Pearson's correlation test
        cor_test <- cor.test(df[[term1]], df[[term2]])
        pval <- cor_test$p.value
        cor_value <- cor_test$estimate
        df_value <- cor_test$parameter
        
        
        # Count significant models
        out <- data.frame(term1 = term1,
                          term2 = term2, 
                          pval = pval,
                          cor_value = cor_value,
                          df_value = df_value)
        dag <- rbind(dag, out)
        
        total_models <- total_models + 1
      }
    }
  }
  
  dag$qval <- p.adjust(p = dag$pval, method = 'fdr', n = nrow(dag))
  return(dag)
}

{
  data <- read.csv("Assays Data.csv", na = "NA")
data <- subset(data,Sex != "G" & (Rater == "GG"| Rater == "EP" | Rater == "IW")& 
                 Tank != "T12"& 
                 Tank != "B11" &
                 Tank != "B21"& 
                 Tank != "T3")
data$Sum.Behaviors <- as.numeric(data$Sum.Behaviors)
data$Aggressive.per.minute <- as.numeric(data$Aggressive.per.minute)
data$Round <- as.factor(data$Round)
data$Latency <- as.numeric(data$Latency)
data$Quiver <- as.numeric(data$Quiver)
data$Latency <- as.numeric(data$Latency)
data$Latency <- ifelse(is.nan(data$Latency) | is.na(data$Latency), data$Time.For.Assay.minutes * 60, data$Latency)
}

together.data.f <- data%>% 
  filter(Sex == 'F')%>%
 group_by(Fish, Assay) %>%
   summarize(Mean_Sum_Behaviors = mean(Sum.Behaviors, na.rm = TRUE), .groups = 'drop')%>%
  pivot_wider(names_from = c(Assay), 
              values_from = Mean_Sum_Behaviors,
              names_sep = "_")
 
 together.data.m <- data%>% 
  filter(Sex == 'M')%>%
 group_by(Fish, Assay) %>%
   summarize(Mean_Sum_Behaviors = mean(Sum.Behaviors, na.rm = TRUE), .groups = 'drop')%>%
  pivot_wider(names_from = c(Assay), 
              values_from = Mean_Sum_Behaviors,
              names_sep = "_")

assays<- colnames(together.data.f[3:9])

male_pairs <- pairwise_pearson(assays, together.data.m)
male_pairs$signif <- ifelse(male_pairs$qval<0.05, '*', NA)
#3 Significant correlations

female_pairs <- pairwise_pearson(assays, together.data.f)
female_pairs$signif <- ifelse(female_pairs$qval<0.05, '*', NA)
#no signiicant correlations
