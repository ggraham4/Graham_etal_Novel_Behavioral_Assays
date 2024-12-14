#Within sex ICC calculations

#libraries
library(ggsignif)
library(ggplot2)
library(readxl)
library(tidyverse)
library(emmeans)
library(ggeffects)
library(ggpubr)
library(lme4)
library(openxlsx)
library(effectsize)
library(psych)
library(lmerTest)
  library(readxl)



#Data
{
  data <- read.csvl("Assays Data.csv", na = "NA")
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
data$Latency <- ifelse(is.nan(data$Latency),data$Time.For.Assay.minutes*60, data$Latency)
data$Pec.fans <- as.numeric(data$Pec.fans)
data$Sex <- as.factor(data$Sex)
data$Eggs <- as.factor(data$Eggs)

}
#Subset data frames
{
parenting.data <- data%>%
  filter(Assay == "Parenting")

crab.data <- data%>%
  filter(Assay == "CrabLeashed")

damsel.data <- data%>%
  filter(Assay == "Damsel")

Male.data <- data%>%
  filter(Assay == "Male")

Female.data <- data%>%
  filter(Assay == "Female")

Hand.data <- data%>%
  filter(Assay == "Hand")

Rock.data <- data%>%
  filter(Assay == "Rock")
}

#initialize an empty data frame
split_sex_icc <- data.frame()

#parenting
parenting_behaviors <- colnames(parenting.data[,c(14:18, 26)])
#for each sex, and each behavior fit a model to eggs*round with a random effect of fish, then calcuate the ICC and p value of random effect
for(i in c('F','M')){ 
  loop_data <- subset(parenting.data, Sex == i)
for(j in parenting_behaviors){
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Parenting',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}

#crab
crab_behaviors <- colnames(crab.data[,c(19,21,22, 26)])
for(i in c('F','M')){
  loop_data <- subset(crab.data, Sex == i)
for(j in crab_behaviors){
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Small Intruder',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}

#Damsel
Damsel_behaviors <- colnames(damsel.data[,c(19,21:23,25, 26)])
for(i in c('F','M')){
  loop_data <- subset(damsel.data, Sex == i)
for(j in Damsel_behaviors){
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Large Intruder',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}

#Male
Male_behaviors <- colnames(Male.data[,c(19,21:23,25, 26)])
for(i in c('F','M')){
  loop_data <- subset(Male.data, Sex == i)
for(j in Male_behaviors){
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Male-Oriented',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}

#Female
Female_behaviors <- colnames(Female.data[,c(19,21:23,25, 26)])
for(i in c('F','M')){
  loop_data <- subset(Female.data, Sex == i)
for(j in Female_behaviors){
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Female-Oriented',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}

#Hand
Hand_behaviors <- colnames(Hand.data[,c(19,25,23, 26)])
for(i in c('F','M')){
  loop_data <- subset(Hand.data, Sex == i)
for(j in Hand_behaviors){
  
  if(i == 'M' & j == 'Chase.Charge'){next}
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
  if(is.null(model)){next}
     
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Immediate Reaction',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}

#Rock
for(i in c('F','M')){
  loop_data <- subset(Rock.data, Sex == i)
for(j in crab_behaviors){
  mod_func <- as.formula(paste0(j,'~Eggs*Round+(1|Fish)'))
  
  model <- lmer(mod_func, data = loop_data)
  
    variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance (THis is the part I dont understand)
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what i assume is the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)
  
  rep_p_val <- as.data.frame(ranova(model))$`Pr(>Chisq)`[2]
  
  temp_data <- data.frame(
    Assay = 'Nest Maintenance',
    Behavior = j,
    Sex = i,
    ICC = repeatability,
    p_val = rep_p_val, singlular = isSingular(model))
  
  split_sex_icc <- rbind(temp_data, split_sex_icc)
}
}
split_sex_icc$q_value <- p.adjust(p=split_sex_icc$p_val, n = nrow(split_sex_icc), method = 'fdr') #because of repeated measures, adjust to q value


