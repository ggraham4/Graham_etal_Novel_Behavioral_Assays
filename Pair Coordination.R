#Libraries
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

library(dplyr)
library(tidyr)


data_pivoted <- data %>%
  group_by(Tank, Eggs, Round, Assay) %>%
  pivot_wider(
    id_cols = c(Tank, Eggs, Round, Assay),  # Ensure these are the only grouping variables
    names_from = Sex,                      # Use Sex (M/F) to create new columns
    values_from = colnames(data)[14:26],   # Behavior columns
    names_glue = "{.value}{Sex}"           # Append M/F to the behavior columns
  ) %>%
  ungroup()  # Ungroup after pivoting


data_pivoted$ID_Male <- paste0(data_pivoted$Tank, 'M')
data_pivoted$ID_Female <- paste0(data_pivoted$Tank, 'F')

data_pivoted[, 5:30] <- lapply(data_pivoted[, 5:30], as.numeric)

######## Modeling male behaviors with female as a covariate ######

parenting.behaviors <- c('Nips',
                              'Pec.fans',
                              'Tail.Fans',
                              'Entrances',
                              'Time',
                              'Sum.Behaviors')


male.data <- data.frame()
for(i in parenting.behaviors){
  
  dat <- subset(data_pivoted, Assay == 'Parenting')
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  male.parenting.mod.func <- as.formula(
    paste0(male.behavior,'~',
      female.behavior,'+Eggs+Round+(1|ID_Male)'
      )
    )
  model <- lmer(male.parenting.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5] #is the female term significant
  
    estimate<- summary(model)$coefficients[2,1] #what is its effect

  temp_data <- data.frame(
    assay = 'Parenting',
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  male.data <- rbind(temp_data, male.data) #add to df
  assign('male.parenting.data',male.data, envir = .GlobalEnv)
}


crab.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                              'Sum.Behaviors')
for(i in crab.behaviors){
  

  var = 'Crab'
    dat <- subset(data_pivoted, Assay == 'CrabLeashed')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  male.mod.func <- as.formula(
    paste0(male.behavior,'~',
      female.behavior,'+Eggs+Round+(1|ID_Male)'
      )
    )
  model <- lmer(male.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  male.data <- rbind(temp_data, male.data)
    assign('male.crab.data',male.data, envir = .GlobalEnv)

}


damsel.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                      'Display',
                    'Chase.Charge',
                              'Sum.Behaviors')


for(i in damsel.behaviors){
  

  var = 'Damsel'
    dat <- subset(data_pivoted, Assay == 'Damsel')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  male.mod.func <- as.formula(
    paste0(male.behavior,'~',
      female.behavior,'+Eggs+Round+(1|ID_Male)'
      )
    )
  model <- lmer(male.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  male.data <- rbind(temp_data, male.data)
      assign('male.damsel.data',male.data, envir = .GlobalEnv)

}


Male.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                      'Display',
                    'Chase.Charge',
                              'Sum.Behaviors')


for(i in Male.behaviors){
  

  var = 'Male'
    dat <- subset(data_pivoted, Assay == 'Male')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  male.mod.func <- as.formula(
    paste0(male.behavior,'~',
      female.behavior,'+Eggs+Round+(1|ID_Male)'
      )
    )
  model <- lmer(male.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  male.data <- rbind(temp_data, male.data)
        assign('male.male.data',male.data, envir = .GlobalEnv)

}

Female.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                      'Display',
                    'Chase.Charge',
                              'Sum.Behaviors')


for(i in Female.behaviors){
  

  var = 'Female'
    dat <- subset(data_pivoted, Assay == 'Female')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  male.mod.func <- as.formula(
    paste0(male.behavior,'~',
      female.behavior,'+Eggs+Round+(1|ID_Male)'
      )
    )
  model <- lmer(male.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  male.data <- rbind(temp_data, male.data)
  
}

Hand.behaviors <- c('Latency',
                    'Display',
                    'Chase.Charge',
                    'Sum.Behaviors')

for(i in Hand.behaviors){
  
  var = 'Hand'
  dat <- subset(data_pivoted, Assay == 'Hand')
  
  male.behavior <- paste0(i, 'M')
  Hand.behavior <- paste0(i, 'F')
  
  male.mod.func <- as.formula(
    paste0(male.behavior, '~', Hand.behavior, '+Eggs+Round+(1|ID_Male)')
  )
  
  # Using tryCatch for both errors and warnings
  model <- tryCatch({
    lmer(male.mod.func, data = dat)
  }, error = function(e) {
    NULL  # Return NULL in case of an error
  }, warning = function(w) {
    NULL  # Return NULL in case of a warning
  })

  # Skip the current iteration if model is NULL
  if (is.null(model)) {
    next
  }
  
  f_signif <- summary(model)$coefficients[2, 5]
  estimate <- summary(model)$coefficients[2, 1]
  
  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  
  male.data <- rbind(temp_data, male.data)
  assign('male.hand.data',male.data, envir = .GlobalEnv)
}




Rock.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                              'Sum.Behaviors')
for(i in Rock.behaviors){
  

  var = 'Rock'
    dat <- subset(data_pivoted, Assay == 'Rock')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  male.mod.func <- as.formula(
    paste0(male.behavior,'~',
      female.behavior,'+Eggs+Round+(1|ID_Male)'
      )
    )
  model <- lmer(male.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  male.data <- rbind(temp_data, male.data)
    assign('male.rock.data',male.data, envir = .GlobalEnv)

}


###########modeling female behaviors with male as a covariate ########

parenting.behaviors <- c('Nips',
                              'Pec.fans',
                              'Tail.Fans',
                              'Entrances',
                              'Time',
                              'Sum.Behaviors')

temp_data <- data.frame()
female.data <- data.frame()
for(i in parenting.behaviors){
  
  dat <- subset(data_pivoted, Assay == 'Parenting')
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  female.parenting.mod.func <- as.formula(
    paste0(female.behavior,'~',
      male.behavior,'+Eggs+Round+(1|ID_Female)'
      )
    )
  model <- lmer(female.parenting.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = 'Parenting',
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  female.data <- rbind(temp_data, female.data)
  assign('female.parenting.data',male.data, envir = .GlobalEnv)

}

crab.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                              'Sum.Behaviors')
for(i in crab.behaviors){
  

  var = 'Crab'
    dat <- subset(data_pivoted, Assay == 'CrabLeashed')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  female.mod.func <- as.formula(
    paste0(female.behavior,'~',
      male.behavior,'+Eggs+Round+(1|ID_Female)'
      )
    )
  model <- lmer(female.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  female.data <- rbind(temp_data, female.data)
    assign('female.crab.data',male.data, envir = .GlobalEnv)

}


damsel.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                      'Display',
                    'Chase.Charge',
                              'Sum.Behaviors')


for(i in damsel.behaviors){
  

  var = 'Damsel'
    dat <- subset(data_pivoted, Assay == 'Damsel')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  female.mod.func <- as.formula(
    paste0(female.behavior,'~',
      male.behavior,'+Eggs+Round+(1|ID_Female)'
      )
    )
  model <- lmer(female.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  female.data <- rbind(temp_data, female.data)
      assign('female.damsel.data',male.data, envir = .GlobalEnv)

}


Male.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                      'Display',
                    'Chase.Charge',
                              'Sum.Behaviors')


for(i in Male.behaviors){
  

  var = 'Male'
    dat <- subset(data_pivoted, Assay == 'Male')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  female.mod.func <- as.formula(
    paste0(female.behavior,'~',
      male.behavior,'+Eggs+Round+(1|ID_Female)'
      )
    )
  model <- lmer(female.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  female.data <- rbind(temp_data, female.data)
      assign('female.male.data',male.data, envir = .GlobalEnv)
  
}

Female.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                      'Display',
                    'Chase.Charge',
                              'Sum.Behaviors')


for(i in Female.behaviors){
  

  var = 'Female'
    dat <- subset(data_pivoted, Assay == 'Female')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  female.mod.func <- as.formula(
    paste0(female.behavior,'~',
      male.behavior,'+Eggs+Round+(1|ID_Female)'
      )
    )
  model <- lmer(female.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  female.data <- rbind(temp_data, female.data)
        assign('female.female.data',male.data, envir = .GlobalEnv)

}

Hand.behaviors <- c('Latency',
                    'Display',
                    'Chase.Charge',
                    'Sum.Behaviors')

for(i in Hand.behaviors){
  
  var = 'Hand'
  dat <- subset(data_pivoted, Assay == 'Hand')
  
  male.behavior <- paste0(i, 'M')
  Hand.behavior <- paste0(i, 'F')
  
  female.mod.func <- as.formula(
    paste0(male.behavior, '~', Hand.behavior, '+Eggs+Round+(1|ID_Female)')
  )
  
  # Using tryCatch for both errors and warnings
  model <- tryCatch({
    lmer(female.mod.func, data = dat)
  }, error = function(e) {
    NULL  # Return NULL in case of an error
  }, warning = function(w) {
    NULL  # Return NULL in case of a warning
  })

  # Skip the current iteration if model is NULL
  if (is.null(model)) {
    next
  }
  
  f_signif <- summary(model)$coefficients[2, 5]
  estimate <- summary(model)$coefficients[2, 1]
  
  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  
  female.data <- rbind(temp_data, female.data)
          assign('female.hand.data',male.data, envir = .GlobalEnv)

}




Rock.behaviors <- c('Latency',
                              'Tail.strike',
                              'Bite',
                              'Sum.Behaviors')
for(i in Rock.behaviors){
  

  var = 'Rock'
    dat <- subset(data_pivoted, Assay == 'Rock')

  
    male.behavior <- paste0(i, 'M')
  female.behavior <- paste0(i, 'F')
  
  female.mod.func <- as.formula(
    paste0(female.behavior,'~',
      male.behavior,'+Eggs+Round+(1|ID_Female)'
      )
    )
  model <- lmer(female.mod.func,
                data = dat)
  
  f_signif<- summary(model)$coefficients[2,5]
  
    estimate<- summary(model)$coefficients[2,1]

  temp_data <- data.frame(
    assay = var,
    behavior = i,
    f_signif = f_signif,
    estimate = estimate
  )
  female.data <- rbind(temp_data, female.data)
            assign('female.rock.data',male.data, envir = .GlobalEnv)

}

full_data <- female.data%>%
  right_join(male.data, by = c('assay','behavior'))

colnames(full_data) <- c('Assay',
                         'Behavior',
                         'Male_P',
                         'Male_Estimate',
                         'Female_P',
                         'Female_Estimate'
)
full_data




                    
                                       
                                       
                                       
