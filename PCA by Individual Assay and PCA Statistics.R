#PCA or individual assays

#libraries and functions
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

#calculate euclidean distance using 2 dimensions
pca_distance_2 <-function(pca, data, by){
if (!inherits(pca, "princomp")) {
    stop("The 'pca' argument must be a 'princomp' object.")
}
  library(dplyr)
  
  x <- pca
  y <- data

  
 means <- data.frame(pca$scores, group = data[[by]])  
 
  grouped.means <- means%>%
  group_by(group)%>%
    summarize(across(starts_with("Comp."), mean))

 mean.1 <- grouped.means[1, 2:3]  
  mean.2 <- grouped.means[2, 2:3]
  
distance <- dist(rbind(as.numeric(mean.1), as.numeric(mean.2)))

print(distance)

}

#t.test assuming equal variance 
pca_t.test<- function(pca, data, by){ 
  # Check if the PCA object is a 'princomp' object
  if (!inherits(pca, "princomp")) {
    stop("The 'pca' argument must be a 'princomp' object.")
  }
  
  # Extract scores for the first principal component and create a data frame
  scores_pc1 <- data.frame(Scores = pca$scores[, 1], Group = data[[by]])
  
  # Filter scores for each group
  scores_group1 <- scores_pc1[scores_pc1$Group == 'F', 'Scores']
  scores_group2 <- scores_pc1[scores_pc1$Group == 'M', 'Scores']
  
  # Perform the t-test, assuming unequal variances (Welch's t-test)
  t_test_results <- t.test(scores_group1, scores_group2, var.equal = FALSE)
  
  # Print the t-test results
  print(t_test_results)
}

#t.test assuming unequal variance 
pca_t.test.2 <- function(pca, data, by){
  # Check if the PCA object is a 'princomp' object
  if (!inherits(pca, "princomp")) {
    stop("The 'pca' argument must be a 'princomp' object.")
  }
  
  # Extract scores for the first principal component and create a data frame
  scores_pc1 <- data.frame(Scores = pca$scores[, 2], Group = data[[by]])
  
  # Filter scores for each group
  scores_group1 <- scores_pc1[scores_pc1$Group == 'F', 'Scores']
  scores_group2 <- scores_pc1[scores_pc1$Group == 'M', 'Scores']
  
  # Perform the t-test, assuming unequal variances (Welch's t-test)
  t_test_results <- t.test(scores_group1, scores_group2, var.equal = TRUE, alternative = "two.sided")
  
  # Print the t-test results
  print(t_test_results)
  


}

#use levenes test to compare variance between sexes
pca_levene <- function(pca, data, by){
  # Check if PCA object is correct
  if (!inherits(pca, "princomp")) {
    stop("The 'pca' argument must be a 'princomp' object.")
  }
  
  library(car)

  # Convert PCA scores to a data frame (if not already)
  scores <- as.data.frame(pca$scores)
  
  # Add the grouping variable from 'data' to 'scores'
  scores$Group <- data[[by]]
  
  # Run Levene's test on the first principal component
  var_test <- leveneTest(scores[, 1] ~ Group, data = scores)
  
  print(var_test)
}

#calculate r2 of PC1
pca_r2 <- function(pca, data, by){
  if (!inherits(pca, "princomp")) {
    stop("The 'pca' argument must be a 'princomp' object.")
  }
  
  scores <- data.frame(PC1 = pca$scores[,1])
  scores$Sex <- data[[by]]
  
  model <- lm(PC1~Sex, data = scores)
  
  rsquared <- summary(model)$r.squared
  print(paste("R-squared for PC1 by Sex: ", rsquared))
}

#calculate r2 of PC2
pca_r2.2 <- function(pca, data, by){
  if (!inherits(pca, "princomp")) {
    stop("The 'pca' argument must be a 'princomp' object.")
  }
  
  scores <- data.frame(PC2 = pca$scores[,2])
  scores$Sex <- data[[by]]
  
  model <- lm(PC2~Sex, data = scores)
  
  rsquared <- summary(model)$r.squared
  print(paste("R-squared for PC2 by Sex: ", rsquared))
}

}

{
  data <- read_excel("Assays Data/Assays Data.xlsx", na = "NA")
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

together.data <- data%>% 
 group_by(Fish, Assay, Sex) %>%
   summarize(Mean_Sum_Behaviors = mean(Sum.Behaviors, na.rm = TRUE), .groups = 'drop')%>%
  pivot_wider(names_from = c(Assay), 
              values_from = Mean_Sum_Behaviors,
              names_sep = "_")

females <- unique(data$Fish[together.data$Sex=='F'])
males <- unique(data$Fish[together.data$Sex=='M'])

female_renamer <- function(data, list) {
  data$arbitrary_id <- data$Fish # Initialize with original values
  i <- 1
  for (j in list) {
    new_female_id <- paste0('F', i)
    data$arbitrary_id[data$Fish == j] <- new_female_id
    i <- i + 1
  }
  return(data)
}


male_renamer <- function(data, list) {
  i <- 1
  for (j in list) {
    new_male_id <- paste0('M', i)
    data$arbitrary_id[data$Fish == j] <- new_male_id
    i <- i + 1
  }
  return(data)
}
data <- female_renamer(data, females)
data <- male_renamer(data, males)

##### Assays together #####

pca_t <-  princomp(together.data[,4:10],cor = TRUE) 
#COR determines weather data are scaled or not and they should due to different scales

pca_levene(pca_t, together.data, 'Sex') 
#Variances are not significantly different so use t.test

 pca_t.test(pca_t, together.data, 'Sex')

###### Assays separately #####
#Parenting
{
data$Pec.fans <- as.numeric(data$Pec.fans)

Parenting.data <- data%>%
  filter(Assay == "Parenting")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Nip = mean(Nips),
            'Pec Fan' = mean(Pec.fans),
            'Tail Fan' = mean(Tail.Fans),
            Entrance = mean(Entrances),
            'Time in Nest' = mean(Time))

pca_Parenting <-  princomp(Parenting.data[,5:9],cor = TRUE) #COR determines weather data are scaled or not and I think they should
}

#Crab
{
CrabLeashed.data<- data%>%
  filter(Assay == "CrabLeashed")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Bite = mean(Bite),
            Strike = mean(Tail.strike),
            Latency = mean(Latency))
CrabLeashed.data

pca_CrabLeashed <- princomp(CrabLeashed.data[,5:7], cor = TRUE)
}

#Rock
{
Rock.data<- data%>%
  filter(Assay == "Rock")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Bite = mean(Bite),
            Strike = mean(Tail.strike),
            Latency = mean(Latency))
Rock.data

pca_Rock <- princomp(Rock.data[,5:7], cor = TRUE)
}


#Damsel
{
  data$Tail.strike <- as.numeric(data$Tail.strike)
  
Damsel.data<- data%>%
  filter(Assay == "Damsel")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Bite = mean(Bite),
            Strike = mean(Tail.strike),
            Latency = mean(Latency),
            Display = mean(Display),
            Chase.Charge = mean(Chase.Charge))%>%
    mutate(Strike = ifelse(is.na(Strike), 0, Strike))
Damsel.data

pca_Damsel <- princomp(Damsel.data[,5:9], cor = TRUE)
}


#Male
{
  data$Tail.strike <- as.numeric(data$Tail.strike)
  
Male.data<- data%>%
  filter(Assay == "Male")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Bite = mean(Bite),
            Strike = mean(Tail.strike),
            Latency = mean(Latency),
            Display = mean(Display),
            Chase.Charge = mean(Chase.Charge))%>%
    mutate(Strike = ifelse(is.na(Strike), 0, Strike))
Male.data

pca_Male <- princomp(Male.data[,5:9], cor = TRUE)
}


#Female
{
  data$Tail.strike <- as.numeric(data$Tail.strike)
  
Female.data<- data%>%
  filter(Assay == "Female")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Bite = mean(Bite),
            Strike = mean(Tail.strike),
            Latency = mean(Latency),
            Display = mean(Display),
            Chase.Charge = mean(Chase.Charge))%>%
    mutate(Strike = ifelse(is.na(Strike), 0, Strike))
Female.data

pca_Female <- princomp(Female.data[,5:9], cor = TRUE)

}

#Hand
{
  data$Tail.strike <- as.numeric(data$Tail.strike)
  
Hand.data<- data%>%
  filter(Assay == "Hand")%>%
  group_by(Tank,Fish,Sex, arbitrary_id)%>%
  summarize(Bite = mean(Bite),
            Strike = mean(Tail.strike),
            Latency = mean(Latency),
            Display = mean(Display),
            Chase.Charge = mean(Chase.Charge))%>%
    mutate(Strike = ifelse(is.na(Strike), 0, Strike))
Hand.data

pca_Hand <- princomp(Hand.data[,5:9], cor = TRUE)


}

#### Statistics for Each Assay #####

#list of assays
  list <- c('Parenting', 'CrabLeashed', 'Damsel', 'Male', 'Female', 'Hand', 'Rock')

#for loop to make a data frame for each pca
for (i in list) {
  pca_name <- paste0("pca_", i)  # Create the name of the PCA object
  data_name <- paste0(i, ".data")  # Create the name of the data object
  
  if (exists(pca_name) && exists(data_name)) {
    pca_object <- get(pca_name)  # Retrieve the PCA object using its name
    data_object <- get(data_name)  # Retrieve the data object using its name

    if (!inherits(pca_object, "princomp")) {
      cat("The object", pca_name, "is not a valid 'princomp' object.\n")
    } else {
      output <- pca_distance_2(pca_object, data_object, 'Sex')
      print(paste(i, output))
    }
  } else {
    cat("Either PCA or data object for", i, "does not exist.\n")
  }
}
# Kolomogorov Smirnov test to determine if data come from the same distribution
  {
    #ks test for all pca
      list <- c('Parenting', 'CrabLeashed', 'Damsel', 'Male', 'Female', 'Hand', 'Rock')

list <- c('Parenting', 'CrabLeashed', 'Damsel', 'Male', 'Female', 'Hand', 'Rock')

for (i in list) {
  pca_name <- paste0("pca_", i)
  if (exists(pca_name)) {
    pca_object <- get(pca_name)
    if (!inherits(pca_object, "princomp")) {
      cat("The object", pca_name, "is not a valid 'princomp' object.\n")
    } else {
      scores <- pca_object$scores[,1]
      normalized_scores <- (scores - mean(scores)) / sd(scores)
      output <- ks.test(normalized_scores, "pnorm", mean = 0, sd = 1)
      
      cat(i, "D statistic:", output$statistic, "\n")
      cat(i, "P-value:", output$p.value, "\n")
      cat(i, "Test:", output$method, "\n")
    }
  } else {
    cat("PCA object for", i, "does not exist.\n")
  }
}
    
  }

  #Use levenes test to test for equal variance
 
for (i in list) {
  pca_name <- paste0("pca_", i)  # Create the name of the PCA object
  data_name <- paste0(i, ".data")  # Create the name of the data object
  
  if (exists(pca_name) && exists(data_name)) {
    pca_object <- get(pca_name)  # Retrieve the PCA object using its name
    data_object <- get(data_name)  # Retrieve the data object using its name

    if (!inherits(pca_object, "princomp")) {
      cat("The object", pca_name, "is not a valid 'princomp' object.\n")
    } else {
      output <- #pca.levene(pca_object, data_object, 'Sex')
      print(paste(i))
      output
    }
  } else {
    cat("Either PCA or data object for", i, "does not exist.\n")
  }
  #All p values > 0.05, thus, var = EQUAL, we use pca_t.test.2()
}  

 list <- c('Parenting', 'CrabLeashed', 'Damsel', 'Male', 'Female', 'Hand', 'Rock')

 #Test if PC1 significantly differs between sexes
for (i in list) {
  pca_name <- paste0("pca_", i)  # Create the name of the PCA object
  data_name <- paste0(i, ".data")  # Create the name of the data object
  
  if (exists(pca_name) && exists(data_name)) {
    pca_object <- get(pca_name)  # Retrieve the PCA object using its name
    data_object <- get(data_name)  # Retrieve the data object using its name

    if (!inherits(pca_object, "princomp")) {
      cat("The object", pca_name, "is not a valid 'princomp' object.\n")
    } else {
      output <- pca_t.test(pca_object, data_object, 'Sex')
      print(paste(i))
      
       cat(i, "T statistic:", output$statistic, "\n")
      cat(i, "P-value:", output$p.value, "\n")
      cat(i, "Estimate:", output$estimate,"female, male", "\n")
      cat(i, "95% CI", output$conf.int)
      
      cat(i, "\nSerr", output$stderr)
    }
  } else {
    cat("Either PCA or data object for", i, "does not exist.\n")
  }
}
#We report results in supplementary excel file 20
 
 ### R2 for principal component 1 ###
 
   for (i in list) {
  pca_name <- paste0("pca_", i)  # Create the name of the PCA object
  data_name <- paste0(i, ".data")  # Create the name of the data object
  
  if (exists(pca_name) && exists(data_name)) {
    pca_object <- get(pca_name)  # Retrieve the PCA object using its name
    data_object <- get(data_name)  # Retrieve the data object using its name

    if (!inherits(pca_object, "princomp")) {
      cat("The object", pca_name, "is not a valid 'princomp' object.\n")
    } else {
      output <- pca_r2(pca_object, data_object, 'Sex')
      print(paste(i))
    }
  } else {
    cat("Either PCA or data object for", i, "does not exist.\n")
  }
   }
 
  pca_r2(pca_t, together.data, 'Sex')
  
 ### R2 for principal component 2 ###
  
  for (i in list) {
    pca_name <- paste0("pca_", i)  # Create the name of the PCA object
    data_name <- paste0(i, ".data")  # Create the name of the data object
    
    if (exists(pca_name) && exists(data_name)) {
      pca_object <- get(pca_name)  # Retrieve the PCA object using its name
      data_object <- get(data_name)  # Retrieve the data object using its name
      
      if (!inherits(pca_object, "princomp")) {
        cat("The object", pca_name, "is not a valid 'princomp' object.\n")
      } else {
        output <- pca_r2.2(pca_object, data_object, 'Sex')
        print(paste(i))
      }
    } else {
      cat("Either PCA or data object for", i, "does not exist.\n")
    }
}

