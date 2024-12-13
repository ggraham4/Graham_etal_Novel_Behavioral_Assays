#Libraries
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

  {
  data <- read_excel("Assays Data/Assays Data.xlsx", na = "NA")
data <- subset(data,Sex != "G" & (Rater == "GG"| Rater == "EP" | Rater == "IW")& #Subset to all videos that have been rated
                 Tank != "T12"& 
                 Tank != "B11" &
                 Tank != "B21"& 
                 Tank != "T3") #Exclude specific tanks that were incomplete
    
data$Sum.Behaviors <- as.numeric(data$Sum.Behaviors)
data$Round <- as.factor(data$Round)
data$Latency <- as.numeric(data$Latency)
data$Quiver <- as.numeric(data$Quiver)
data$Latency <- as.numeric(data$Latency)
data$Latency <- ifelse(is.nan(data$Latency),data$Time.For.Assay.minutes*60, data$Latency)
data$Pec.fans <- as.numeric(data$Pec.fans)
data$Sex <- as.factor(data$Sex)
data$Eggs <- as.factor(data$Eggs)

}
#Create dfs subset to specific assays 
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

#Define function to calculate ICC
repeatability_function <- function(model){
  library(lmerTest)
  
  #Define lmer model as model
  model <- model
  
  #All variance
  variance_components <- as.data.frame(VarCorr(model))
  
  #extract individual variance 
  individual_variance <-variance_components[1,4]

  #Calculate residual variance 
  residual_variance <- attr(VarCorr(model), "sc")^2
  
  #individual variance/individual variance + what the sum of the mean square error
  repeatability <- individual_variance / (individual_variance + residual_variance)

  #cat("\n REPEATABILITY:", repeatability,"\n")
  return(repeatability)
}

#Define function to output ICC, skewness, random effect p value, kurtosis
ez <- function(model){
  model <- model
  
  message('ICC')
  ICC <- repeatability_function(model)
  cat('ICC',round(ICC,3),'\n\n')
  
  message('Rand')
  rand <- ranova(model)[2,6]
  cat('Random P.val',rand, '\n\n')
  
  message('skew')
  s <- skew(summary(model)$residuals)
  cat('skew', round(s,3), '\n\n')
  
  message('kurtosis')
  k <- kurtosi(summary(model)$residuals)
  cat('kurtosi', round(k,3), '\n\n')
}

####### PARENTING ###########
sum.parenting.lmer <- lmer(sqrt(Sum.Behaviors)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = parenting.data)

nip.parenting.lmer <- lmer(sqrt(Nips)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = parenting.data)

pec.parenting.lmer <- lmer(sqrt(Pec.fans)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = parenting.data)

tail.parenting.lmer <- lmer(sqrt(Tail.Fans)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = parenting.data)

time.parenting.lmer <- lmer(Time~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = parenting.data)

entrances.parenting.lmer <- lmer(sqrt(Entrances)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = parenting.data)

entrances.x.time <- lmer(sqrt(Entrances)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+ #here, we include a time:sex interaction 
                           Time:Sex+(1|Fish), data = parenting.data)

parenting.list <- c('sum.parenting.lmer', 'nip.parenting.lmer', 'pec.parenting.lmer', 'tail.parenting.lmer', 'time.parenting.lmer', 'entrances.parenting.lmer','entrances.x.time')

for(i in parenting.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}

######## Crab ########


crab.sum <- lmer(Sum.Behaviors~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = crab.data)

crab.lat <- lmer(sqrt(Latency)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = crab.data)

crab.strik <- lmer(sqrt(Tail.strike)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = crab.data)

crab.bit <- lmer(sqrt(Bite)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = crab.data)


crab.list <- c('crab.sum', 'crab.lat', 'crab.strik', 'crab.bit')

for(i in crab.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}

############## Damsel #####################

Damsel.sum <- lmer(sqrt(Sum.Behaviors)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = damsel.data)

Damsel.lat <- lmer(log(Latency+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = damsel.data)


Damsel.chas <- lmer(log(Chase.Charge+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = damsel.data)


Damsel.disp <- lmer(sqrt(Display)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = damsel.data)


Damsel.strik <- lmer(log(Tail.strike+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = damsel.data)

Damsel.bit <- lmer(log(Bite+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = damsel.data)

dam.list <- c('Damsel.sum','Damsel.lat','Damsel.chas','Damsel.disp','Damsel.strik','Damsel.bit')
for(i in dam.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}

####### Male #######
Male.sum <- lmer(sqrt(Sum.Behaviors)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Male.data)

Male.lat <- lmer(log(Latency+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Male.data)


Male.chas <- lmer(log(Chase.Charge+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Male.data)

Male.disp <- lmer(Display~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Male.data)


Male.bit <- lmer(log(Bite+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Male.data)

Male.strike <- lmer(sqrt(Tail.strike)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Male.data)


male.data.eggs <- Male.data%>% #Comparing own vs none conditions
  filter(Eggs != 'None')

male.data.eggs$egg.age <- as.numeric(parenting.data$Age.eggs.prior+3) #considering age of eggs
male.data.eggs$eggs.prior <- as.numeric(parenting.data$Eggs.prior)
male.data.eggs$eggs.after <- as.numeric(parenting.data$Eggs.after)

Male.lat.age <- lmer(log(Latency+1)~Eggs+egg.age+eggs.prior+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = male.data.eggs)


mal.list <- c('Male.lat.age')
for(i in mal.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}

############### FEMALE ###########
Female.sum <- lmer(sqrt(Sum.Behaviors)~Relative.size.counterpart+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Female.data)

Female.lat <- lmer(log(Latency+1)~Relative.size.counterpart+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Female.data)

Female.chas <- lmer(sqrt(Chase.Charge)~Relative.size.counterpart+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Female.data)

Female.disp <- lmer(sqrt(Display)~Relative.size.counterpart+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Female.data)

Female.bit <- lmer(sqrt(Bite)~Relative.size.counterpart+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Female.data)

Female.strik <- lmer(Tail.strike~Relative.size.counterpart+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Female.data)

female.data.eggs <- Female.data%>% #comparing own vs surrogate conditions
    filter(Eggs != 'None')
female.data.eggs$age <- as.numeric(parenting.data$Age.eggs.prior+4)
female.data.eggs$eggs.prior <- as.numeric(parenting.data$Eggs.prior)
female.data.eggs$eggs.after <- as.numeric(parenting.data$Eggs.after)

Female.sum.combined<- lmer(sqrt(Sum.Behaviors)~Relative.size.counterpart+age+eggs.after+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = female.data.eggs)

Female.disp.count.age <- lmer(sqrt(Display)~Relative.size.counterpart+age+eggs.after+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = female.data.eggs)

Female.strik.age.count <- lmer(Tail.strike~Relative.size.counterpart+age+eggs.after+Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = female.data.eggs)

femmal.list <- c('Female.sum.combined','Female.disp.count.age','Female.strik.age.count')
for(i in femmal.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}

  #comparing age of eggs in two conditions
t.test(female.data.eggs$age[female.data.eggs$Eggs == 'Own' & female.data.eggs$Sex == 'M'], female.data.eggs$age[female.data.eggs$Eggs == 'Surrogate'& female.data.eggs$Sex == 'M'], 'two.sided')
  #comparing count of eggs in two conditions
t.test(female.data.eggs$eggs.after[female.data.eggs$Eggs == 'Own'& female.data.eggs$Sex == 'M'], female.data.eggs$eggs.after[female.data.eggs$Eggs == 'Surrogate'& female.data.eggs$Sex == 'M'], 'two.sided')


################### RATIO ########################################
div.data <- data%>%
  filter(Assay == "Male"|Assay == "Female")

#pivoting
vs2 <- div.data%>%
  group_by(Fish, Sex, Round, Eggs, Tank)%>%
  summarize(Male = Sum.Behaviors[Assay == "Male"]+1, Female = Sum.Behaviors[Assay == "Female"]+1)
vs2

vs2$Ratio = vs2$Female/vs2$Male
vs2$Log.rat = log(vs2$Ratio)


ratio.lmer <-  lmer(Log.rat ~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish),
                    data =vs2)

ez(ratio.lmer)
############## HAND ################
Hand.sum <- lmer(Sum.Behaviors~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Hand.data)

Hand.lat <- lmer(Latency~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Hand.data)

Hand.chas <- lmer(log(Chase.Charge+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Hand.data)

Hand.disp <- lmer(Display~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Hand.data)

Hand.strike <- lmer(log(Tail.strike+1)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Hand.data)

Hand.bit <- lmer((Bite)^(1/3)~Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Hand.data)

hand.eggs <- Hand.data%>% #comparing own vs surrogate
    filter(Eggs != 'None')

hand.eggs$age <- as.numeric(parenting.data$Age.eggs.after)
hand.eggs$eggs.after <- as.numeric(parenting.data$Eggs.after)

Hand.lat.count.age <- lmer(Latency~Eggs+age+ eggs.after+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = hand.eggs)

hand.list <- c('Hand.lat.count.age', 'hand.eggs','Hand.bit','Hand.strike','Hand.disp','Hand.chas','Hand.lat','Hand.sum')
for(i in hand.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}

############### ROCK ################
rock.sum <- lmer(Sum.Behaviors ~ Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Rock.data)

rock.lat <- lmer(log(Latency+1) ~ Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Rock.data)

rock.strik <- lmer(Tail.strike ~ Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Rock.data)
  
rock.bit <- lmer(sqrt(Bite) ~ Eggs+Sex+Round+Sex:Eggs+Sex:Round+(1|Fish), data = Rock.data)

rock.list <- c('rock.sum','rock.lat','rock.strik','rock.bit')
for(i in rock.list){
  m <- get(i)
  cat('\n',i, '\n')
  ez(m)
}
